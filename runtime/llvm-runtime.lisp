;;;; Copyright (c) 2017 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :llvm-runtime)

(deftype octet-vector ()
  `(simple-array (unsigned-byte 8) (*)))

(defstruct (llvm-context
             (:constructor make-llvm-context-1))
  ;; Stack pointer for the LLVM stack, only used to implement alloca instructions.
  ;; Stack grows down.
  (stack-pointer 0 :type (unsigned-byte 64))
  ;; LLVM memory, pointer accesses access this array.
  (memory (error "memory not supplied") :type octet-vector)
  ;; Was the code compiled for le32 or for le64.
  is-32-bit
  ;; Function pointers are implemented as indices into this array.
  ;; They're offset by 1, with a function pointer of 0/null corresponding to
  ;; out-of-bounds index -1.
  (function-table (error "function table not supplied") :type simple-vector)
  ;; A symbol, not a function pointer.
  entry-point
  ;; Non-octet objects cannot be stored in the memory array, this vector
  ;; allows setjmp trampoline closures to be referred to by an integer value.
  (setjmp-stack (make-array 8 :adjustable t :fill-pointer 0))
  personality)

(defun register-callback (function-designator context)
  (check-type function-designator (or function symbol))
  (let ((existing (position function-designator (llvm-context-function-table context))))
    (when (not existing)
      (setf (llvm-context-function-table context)
            (adjust-array (llvm-context-function-table context)
                          (1+ (length (llvm-context-function-table context)))))
      (setf existing (1- (length (llvm-context-function-table context))))
      (setf (aref (llvm-context-function-table context) existing)
            function-designator))
    ;; +1 due to the pointer offset
    (1+ existing)))

(defclass unix-personality ()
  ((%brk :initarg :brk :accessor unix-personality-brk)
   (%file-table :initform (make-array 8 :adjustable t :fill-pointer 0)
                :accessor unix-personality-file-table)
   (%current-directory :initarg :current-directory :accessor unix-personality-current-directory))
  (:default-initargs :current-directory nil))

;; All memory accesses are offset by this value, with index 0 in the context
;; memory array being address #x10000. Addresses below this value correspond
;; to negative indices, and will cause a bounds error when accesses.
;; This allows null pointer accesses to be caught.
(defconstant +null-avoidance-offset+ #x10000)

(defun align-up (value boundary)
  (let ((n (+ value (1- boundary))))
    (- n (rem n boundary))))

(defun make-llvm-context (personality data-start data-initializer is-32-bit function-table entry-point personality-initargs)
  "Create a new LLVM context.
You probably don't want to call this function directly.
The generated MAKE-CONTEXT function will call this function with the
appropriate arguments."
  ;; DATA-START/DATA-INITIALIZER are the base address and initial contents of the data section.
  ;; The stack pointer is initialized to the start of the data section, so the LLVM stack ranges from from 0 to the data section.
  (assert (eql personality :unix))
  (assert is-32-bit)
  (let* ((memory-size (- (align-up (+ data-start (length data-initializer)) #x10000)
                         +null-avoidance-offset+))
         (memory (make-array memory-size
                             :element-type '(unsigned-byte 8))))
    (replace memory data-initializer
             :start1 (- data-start +null-avoidance-offset+))
    (make-llvm-context-1
     :stack-pointer data-start
     :memory memory
     :is-32-bit is-32-bit
     :function-table function-table
     :entry-point entry-point
     :personality (apply #'make-instance
                         'unix-personality
                         :brk (+ memory-size +null-avoidance-offset+)
                         personality-initargs))))

(defmacro define-llvm-function (name (lambda-list &key need-frame-pointer uses-setjmp context personality) &body body)
  "Defines an LLVM function.
NEED-FRAME-POINTER must be true if ALLOCA is used.
USES-SETJMP must be true if SETJMP.PREPARE is used.
CONTEXT is bound the the current LLVM context.
PERSONALITY is bound to the context's personality object."
  (multiple-value-bind (body decls doc)
      (alexandria:parse-body body :documentation t)
    `(defun ,name (llvm-context ,@lambda-list)
       (declare (ignorable llvm-context ,@lambda-list))
       ,@decls
       ,doc
       (let (,@(when personality `((,personality (llvm-context-personality llvm-context))))
             ,@(when context `((,context llvm-context))))
         (declare (ignorable ,@(when personality `(,personality))
                             ,@(when context `(,context))))

         ,(if (or need-frame-pointer
                  uses-setjmp)
              `(let ((frame-pointer (llvm-context-stack-pointer llvm-context))
                     (saved-setjmp-offset (fill-pointer (llvm-context-setjmp-stack llvm-context))))
                 (unwind-protect
                      (locally
                          ,@body)
                   (setf (llvm-context-stack-pointer llvm-context) frame-pointer
                         (fill-pointer (llvm-context-setjmp-stack llvm-context)) saved-setjmp-offset)))
              `(locally
                   ,@body))))))

(defmacro sign-extend (value width)
  "Sign extend an value of the specified width."
  (cond
    #+sbcl
    ((member width '(8 16 32))
     `(sb-vm::sign-extend ,value ,width))
    (t
     `(let ((value ,value))
        (if (logbitp (1- ,width) value)
            (logior (ash -1 (1- ,width)) value)
            value)))))

(defmacro define-sized-op (name (lambda-list size &key (restrict-output t)) &body body)
  (flet ((emit (actual-size)
           ;; Use the current package, not (symbol-package name), to avoid
           ;; interning AND.I8 and similar in CL.
           (let ((real-name (intern (format nil "~S.I~D" name actual-size))))
             `(progn
                ,(if restrict-output
                     `(defmacro ,real-name ,lambda-list
                        `(ldb (byte ,',actual-size 0)
                              ,(let ((,size ,actual-size))
                                    (declare (ignorable ,size))
                                    ,@body)))
                     `(defmacro ,real-name ,lambda-list
                        (let ((,size ,actual-size))
                          (declare (ignorable ,size))
                          ,@body)))))))
    `(progn
       ,(emit 1)
       ,(emit 8)
       ,(emit 16)
       ,(emit 32)
       ,(emit 64))))

;;;; LLVM instruction implementation.

;;; Arithmetic operations.
;;; LLVM integer types are represented using Lisp UNSIGNED-BYTE types,
;;; signed operators must sign-extend the input values.
;;; All integer operators must wrap their result to the bit size.

(define-sized-op add ((lhs rhs) size)
  `(+ ,lhs ,rhs))

(defmacro fadd.f32 (lhs rhs)
  `(+ ,lhs ,rhs))

(defmacro fadd.f64 (lhs rhs)
  `(+ ,lhs ,rhs))

(define-sized-op sub ((lhs rhs) size)
  `(- ,lhs ,rhs))

(defmacro fsub.f32 (lhs rhs)
  `(- ,lhs ,rhs))

(defmacro fsub.f64 (lhs rhs)
  `(- ,lhs ,rhs))

(define-sized-op mul ((lhs rhs) size)
  `(* ,lhs ,rhs))

(defmacro fmul.f32 (lhs rhs)
  `(* ,lhs ,rhs))

(defmacro fmul.f64 (lhs rhs)
  `(* ,lhs ,rhs))

(define-sized-op udiv ((lhs rhs) size)
  `(truncate ,lhs ,rhs))

(define-sized-op sdiv ((lhs rhs) size)
  `(truncate (sign-extend ,lhs ,size)
             (sign-extend ,rhs ,size)))

(defmacro fdiv.f32 (lhs rhs)
  `(/ ,lhs ,rhs))

(defmacro fdiv.f64 (lhs rhs)
  `(/ ,lhs ,rhs))

(define-sized-op urem ((lhs rhs) size)
  `(rem ,lhs ,rhs))

(define-sized-op srem ((lhs rhs) size)
  `(rem (sign-extend ,lhs ,size)
        (sign-extend ,rhs ,size)))

(defmacro frem.f32 (lhs rhs)
  `(rem ,lhs ,rhs))

(defmacro frem.f64 (lhs rhs)
  `(rem ,lhs ,rhs))

(define-sized-op shl ((lhs rhs) size)
  `(ash ,lhs (logand ,rhs ,(1- size))))

(define-sized-op lshr ((lhs rhs) size)
  `(ash ,lhs (- (logand ,rhs ,(1- size)))))

(define-sized-op ashr ((lhs rhs) size)
  `(ash (sign-extend ,lhs ,size) (- (logand ,rhs ,(1- size)))))

(define-sized-op and ((lhs rhs) size)
  `(logand ,lhs ,rhs))

(define-sized-op or ((lhs rhs) size)
  `(logior ,lhs ,rhs))

(define-sized-op xor ((lhs rhs) size)
  `(logxor ,lhs ,rhs))

;;; Comparison operators.

(defmacro define-comparison-op (name ((lhs rhs) size) &body body)
  `(progn
     (define-sized-op ,name ((,lhs ,rhs) ,size :restrict-output nil)
       `(if (locally ,,@body)
            1
            0))
     (define-sized-op ,(intern (format nil "~A.FUSED" name)) ((,lhs ,rhs) ,size :restrict-output nil)
       ,@body)))

(define-comparison-op icmp.eq ((lhs rhs) size)
  `(eql ,lhs ,rhs))

(define-comparison-op icmp.ne ((lhs rhs) size)
  `(not (eql ,lhs ,rhs)))

(define-comparison-op icmp.sgt ((lhs rhs) size)
  `(> (sign-extend ,lhs ,size)
      (sign-extend ,rhs ,size)))

(define-comparison-op icmp.sge ((lhs rhs) size)
  `(>= (sign-extend ,lhs ,size)
       (sign-extend ,rhs ,size)))

(define-comparison-op icmp.slt ((lhs rhs) size)
  `(< (sign-extend ,lhs ,size)
      (sign-extend ,rhs ,size)))

(define-comparison-op icmp.sle ((lhs rhs) size)
  `(<= (sign-extend ,lhs ,size)
       (sign-extend ,rhs ,size)))

(define-comparison-op icmp.ult ((lhs rhs) size)
  `(< ,lhs ,rhs))

(define-comparison-op icmp.ule ((lhs rhs) size)
  `(<= ,lhs ,rhs))

(define-comparison-op icmp.ugt ((lhs rhs) size)
  `(> ,lhs ,rhs))

(define-comparison-op icmp.uge ((lhs rhs) size)
  `(>= ,lhs ,rhs))

;; TODO, maybe: Do simething with ordered vs unordered comparisons.

(defmacro define-float-comparison-op (name ((lhs rhs) size) &body body)
  (flet ((emit (ordering actual-size)
           (let ((name (intern (format nil "FCMP.~A~A.F~S" ordering name actual-size)))
                 (fused-name (intern (format nil "FCMP.~A~A.FUSED.F~S" ordering name actual-size))))
             `(progn
                (defmacro ,name (,lhs ,rhs)
                  (let ((,size ,actual-size))
                    (declare (ignorable size))
                    `(if (locally ,,@body)
                         1
                         0)))
                (defmacro ,fused-name (,lhs ,rhs)
                  (let ((,size ,actual-size))
                    (declare (ignorable size))
                    (locally ,@body)))))))
    `(progn
       ,(emit 'o 32)
       ,(emit 'o 64)
       ,(emit 'u 32)
       ,(emit 'u 64))))

(define-float-comparison-op eq ((lhs rhs) size)
  `(eql ,lhs ,rhs))

(define-float-comparison-op gt ((lhs rhs) size)
  `(> ,lhs ,rhs))

(define-float-comparison-op ge ((lhs rhs) size)
  `(>= ,lhs ,rhs))

(define-float-comparison-op lt ((lhs rhs) size)
  `(< ,lhs ,rhs))

(define-float-comparison-op le ((lhs rhs) size)
  `(<= ,lhs ,rhs))

(define-float-comparison-op ne ((lhs rhs) size)
  `(not (eql ,lhs ,rhs)))

;;; Conversion operators.

(defmacro define-integer-conversion-op (name (value size) &body body)
  (flet ((emit (actual-size)
           `(define-sized-op ,(intern (format nil "~S.I~D" name actual-size))
                ((,value) ,(gensym))
              (let ((,size ,actual-size))
                (declare (ignorable ,size))
                ,@body))))
    `(progn
       ,(emit 1)
       ,(emit 8)
       ,(emit 16)
       ,(emit 32)
       ,(emit 64))))

(define-integer-conversion-op sext (value size)
  `(sign-extend ,value ,size))

(define-integer-conversion-op zext (value size)
  value)

(define-integer-conversion-op trunc (value size)
  value)

(defmacro bitcast.i32.i32 (value)
  value)

(defmacro bitcast.i64.i64 (value)
  value)

(defmacro bitcast.f32.i32 (value)
  #+sbcl
  `(ldb (byte 32 0) (sb-kernel:single-float-bits ,value))
  #+mezzano
  `(mezzano.extensions:single-float-to-ieee-binary32 ,value)
  #-(or sbcl mezzano)
  `(let ((tmp (make-array 4 :element-type '(unsigned-byte 8))))
     (declare (dynamic-extent tmp))
     (setf (nibbles:ieee-single-ref/le tmp 0) ,value)
     (nibbles:ub32ref/le tmp 0)))

(defmacro bitcast.i32.f32 (value)
  #+sbcl
  `(sb-kernel:make-single-float (sign-extend ,value 32))
  #+mezzano
  `(mezzano.extensions:ieee-binary32-to-single-float ,value)
  #-(or sbcl mezzano)
  `(let ((tmp (make-array 4 :element-type '(unsigned-byte 8))))
     (declare (dynamic-extent tmp))
     (setf (nibbles:ub32ref/le tmp 0) ,value)
     (nibbles:ieee-single-ref/le tmp 0)))

(defmacro bitcast.f64.i64 (value)
  #+sbcl
  `(ldb (byte 64 0)
        (logior (sb-kernel:double-float-low-bits ,value)
                (ash (sb-kernel:double-float-high-bits ,value) 32)))
  #+mezzano
  `(mezzano.extensions:double-float-to-ieee-binary64 ,value)
  #-(or sbcl mezzano)
  `(let ((tmp (make-array 8 :element-type '(unsigned-byte 8))))
     (declare (dynamic-extent tmp))
     (setf (nibbles:ieee-double-ref/le tmp 0) ,value)
     (nibbles:ub64ref/le tmp 0)))

(defmacro bitcast.i64.f64 (value)
  #+sbcl
  `(sb-kernel:make-double-float (sign-extend (ldb (byte 32 32) ,value) 32)
                                (ldb (byte 32 0) ,value))
  #+mezzano
  `(mezzano.extensions:ieee-binary64-to-double-float ,value)
  #-(or sbcl mezzano)
  `(let ((tmp (make-array 8 :element-type '(unsigned-byte 8))))
     (declare (dynamic-extent tmp))
     (setf (nibbles:ub64ref/le tmp 0) ,value)
     (nibbles:ieee-double-ref/le tmp 0)))

(define-sized-op fptosi.f32 ((value) size)
  `(truncate ,value))

(define-sized-op fptosi.f64 ((value) size)
  `(truncate ,value))

(define-sized-op fptoui.f32 ((value) size)
  `(truncate ,value))

(define-sized-op fptoui.f64 ((value) size)
  `(truncate ,value))

(defmacro fptrunc.f64.f64 (value)
  value)

(defmacro fptrunc.f64.f32 (value)
  `(float ,value 0.0f0))

(defmacro fptrunc.f32.f32 (value)
  value)

(defmacro fpext.f64.f64 (value)
  value)

(defmacro fpext.f32.f64 (value)
  `(float ,value 0.0d0))

(defmacro fpext.f32.f32 (value)
  value)

(defmacro sitofp.i1.f64 (value)
  `(float (sign-extend ,value 8) 0.0d0))

(defmacro sitofp.i8.f64 (value)
  `(float (sign-extend ,value 8) 0.0d0))

(defmacro sitofp.i16.f64 (value)
  `(float (sign-extend ,value 16) 0.0d0))

(defmacro sitofp.i32.f64 (value)
  `(float (sign-extend ,value 32) 0.0d0))

(defmacro sitofp.i64.f64 (value)
  `(float (sign-extend ,value 64) 0.0d0))

(defmacro sitofp.i1.f32 (value)
  `(float (sign-extend ,value 1) 0.0f0))

(defmacro sitofp.i8.f32 (value)
  `(float (sign-extend ,value 8) 0.0f0))

(defmacro sitofp.i16.f32 (value)
  `(float (sign-extend ,value 16) 0.0f0))

(defmacro sitofp.i32.f32 (value)
  `(float (sign-extend ,value 32) 0.0f0))

(defmacro sitofp.i64.f32 (value)
  `(float (sign-extend ,value 64) 0.0f0))

(defmacro uitofp.i1.f64 (value)
  `(float ,value 0.0d0))

(defmacro uitofp.i8.f64 (value)
  `(float ,value 0.0d0))

(defmacro uitofp.i16.f64 (value)
  `(float ,value 0.0d0))

(defmacro uitofp.i32.f64 (value)
  `(float ,value 0.0d0))

(defmacro uitofp.i64.f64 (value)
  `(float ,value 0.0d0))

(defmacro uitofp.i1.f32 (value)
  `(float ,value 0.0f0))

(defmacro uitofp.i8.f32 (value)
  `(float ,value 0.0f0))

(defmacro uitofp.i16.f32 (value)
  `(float ,value 0.0f0))

(defmacro uitofp.i32.f32 (value)
  `(float ,value 0.0f0))

(defmacro uitofp.i64.f32 (value)
  `(float ,value 0.0f0))

;;; Memory access.
;;;
;;; These macros call inlined functions to make it easier to trace memory accesses.
;;; The actual access functions can be marked notinline & traced.

(declaim (inline %load.i8))
(defun %load.i8 (llvm-context address)
  (aref (the octet-vector (llvm-context-memory llvm-context)) (- address +null-avoidance-offset+)))

(defmacro load.i8 (address)
  `(%load.i8 llvm-context ,address))

(declaim (inline %load.i16))
(defun %load.i16 (llvm-context address)
  (nibbles:ub16ref/le (the octet-vector (llvm-context-memory llvm-context)) (- address +null-avoidance-offset+)))

(defmacro load.i16 (address)
  `(%load.i16 llvm-context ,address))

(declaim (inline %load.i32))
(defun %load.i32 (llvm-context address)
  (nibbles:ub32ref/le (the octet-vector (llvm-context-memory llvm-context)) (- address +null-avoidance-offset+)))

(defmacro load.i32 (address)
  `(%load.i32 llvm-context ,address))

(declaim (inline %load.i64))
(defun %load.i64 (llvm-context address)
  (nibbles:ub64ref/le (the octet-vector (llvm-context-memory llvm-context)) (- address +null-avoidance-offset+)))

(defmacro load.i64 (address)
  `(%load.i64 llvm-context ,address))

(declaim (inline %load.f32))
(defun %load.f32 (llvm-context address)
  (nibbles:ieee-single-ref/le (the octet-vector (llvm-context-memory llvm-context)) (- address +null-avoidance-offset+)))

(defmacro load.f32 (address)
  `(%load.f32 llvm-context ,address))

(declaim (inline %load.f64))
(defun %load.f64 (llvm-context address)
  (nibbles:ieee-double-ref/le (the octet-vector (llvm-context-memory llvm-context)) (- address +null-avoidance-offset+)))

(defmacro load.f64 (address)
  `(%load.f64 llvm-context ,address))

(declaim (inline %store.i8))
(defun %store.i8 (llvm-context value address)
  (setf (aref (the octet-vector (llvm-context-memory llvm-context)) (- address +null-avoidance-offset+)) value))

(defmacro store.i8 (value address)
  `(%store.i8 llvm-context ,value ,address))

(declaim (inline %store.i16))
(defun %store.i16 (llvm-context value address)
  (setf (nibbles:ub16ref/le (the octet-vector (llvm-context-memory llvm-context)) (- address +null-avoidance-offset+)) value))

(defmacro store.i16 (value address)
  `(%store.i16 llvm-context ,value ,address))

(declaim (inline %store.i32))
(defun %store.i32 (llvm-context value address)
  (setf (nibbles:ub32ref/le (the octet-vector (llvm-context-memory llvm-context)) (- address +null-avoidance-offset+)) value))

(defmacro store.i32 (value address)
  `(%store.i32 llvm-context ,value ,address))

(declaim (inline %store.i64))
(defun %store.i64 (llvm-context value address)
  (setf (nibbles:ub64ref/le (the octet-vector (llvm-context-memory llvm-context)) (- address +null-avoidance-offset+)) value))

(defmacro store.i64 (value address)
  `(%store.i64 llvm-context ,value ,address))

(declaim (inline %store.f32))
(defun %store.f32 (llvm-context value address)
  (setf (nibbles:ieee-single-ref/le (the octet-vector (llvm-context-memory llvm-context)) (- address +null-avoidance-offset+)) value))

(defmacro store.f32 (value address)
  `(%store.f32 llvm-context ,value ,address))

(declaim (inline %store.f64))
(defun %store.f64 (llvm-context value address)
  (setf (nibbles:ieee-double-ref/le (the octet-vector (llvm-context-memory llvm-context)) (- address +null-avoidance-offset+)) value))

(defmacro store.f64 (value address)
  `(%store.f64 llvm-context ,value ,address))

;;; Alloca.

(defmacro alloca (size)
  (incf size 15)
  (setf size (logand size (lognot 15)))
  `(decf (llvm-context-stack-pointer llvm-context) ,size))

;;; Select.

(defmacro select (condition true false)
  `(if (not (eql ,condition 0))
       ,true
       ,false))

;;; Setjmp support.

;; A SETJMP.PREPARE form these is inserted at the start of each function that calls setjmp,
;; with one per each unique setjmp call.
;; TARGET is the label that longjmp should resume at.
;; This evaluates to the index of the trampoline function.
(defmacro setjmp.prepare (target)
  `(vector-push-extend (lambda (value)
                         ;; Value is the value passed to longjmp.
                         ;; FIXME: Need to actually do something with it.
                         (declare (ignore value))
                         (go ,target))
                       (llvm-context-setjmp-stack llvm-context)))

;; The setjmp function simply stores the index of the trampoline into the jmp_buf.
;; Calls to setjmp are augmented with the trampoline index by the translator.
;; Indirect calls to setjmp aren't supported.
(define-llvm-function |setjmp| ((trampoline-index jmpbuf))
  (store.i32 trampoline-index jmpbuf))

;; Longjmp fetches the trampoline index, loads the trampoline out of the setjmp stack
;; and invokes it.
(define-llvm-function |longjmp| ((jmpbuf value))
  ;; If longjmp is invoked with a value of zero, 1 will be used instead.
  (when (zerop value)
    (setf value 1))
  (funcall (aref (llvm-context-setjmp-stack llvm-context)
                 (load.i32 jmpbuf))
           value))

;;; Call operations.

;; Direct calls call the named function with the context.
(defmacro call-direct (name &rest arguments)
  `(,name llvm-context ,@arguments))

;; Indirect calls convert the function pointer to a Lisp function, then call it with the context.
(defmacro call-indirect (name &rest arguments)
  `(funcall (aref (llvm-context-function-table llvm-context) (1- ,name)) llvm-context ,@arguments))

;;;; End of LLVM instructions.

;;; LLVM intrinsics.

(declaim (inline |llvm.lifetime.start| |llvm.lifetime.end|))
(define-llvm-function |llvm.lifetime.start| ((size ptr)))
(define-llvm-function |llvm.lifetime.end| ((size ptr)))

(define-llvm-function |llvm.bswap.i32| ((value))
  (logior (ash (ldb (byte 8 0) value) 24)
          (ash (ldb (byte 8 8) value) 16)
          (ash (ldb (byte 8 16) value) 8)
          (ldb (byte 8 24) value)))

(define-llvm-function |llvm.flt.rounds| (())
  ;; Rounding mode 1, round to nearest.
  1)

(defun real-memset (llvm-context dst val cnt)
  (when (not (zerop cnt))
    (fill (the octet-vector (llvm-context-memory llvm-context))
          (ldb (byte 8 0) val)
          :start (- dst +null-avoidance-offset+)
          :end (+ (- dst +null-avoidance-offset+) cnt)))
  dst)

(define-llvm-function |llvm.memset.p0i8.i32| ((dst val cnt align is-volatile))
  (real-memset llvm-context dst val cnt))

(define-llvm-function |llvm.memset.p0i8.i64| ((dst val cnt align is-volatile))
  (real-memset llvm-context dst val cnt))

(defun real-memmove (llvm-context d s n)
  (when (not (zerop n))
    (replace (the octet-vector (llvm-context-memory llvm-context))
             (the octet-vector (llvm-context-memory llvm-context))
             :start1 (- d +null-avoidance-offset+)
             :start2 (- s +null-avoidance-offset+)
             :end1 (+ (- d +null-avoidance-offset+) n)))
  d)

(define-llvm-function |memmove| ((d s n))
  (real-memmove llvm-context d s n))

(define-llvm-function |llvm.memmove.p0i8.p0i8.i32| ((d s n align is-volatile))
  (real-memmove llvm-context d s n))

(define-llvm-function |memcpy| ((d s n))
  (real-memmove llvm-context d s n))

(define-llvm-function |llvm.memcpy.p0i8.p0i8.i32| ((d s n align is-volatile))
  (real-memmove llvm-context d s n))

;;; It's a UNIX system! I know this!

(defun align-stack (context)
  "Realign the stack pointer to a 16-octet boundary."
  (setf (llvm-context-stack-pointer context) (logand (llvm-context-stack-pointer context)
                                                     (lognot 15))))

(defun main-1 (context arguments environment)
  "Call the context's entry point with ARGUMENTS and ENVIRONMENT as the contents of argc/argv/envp.
Assumes the context is freshly created."
  ;; Copy strings onto the stack.
  (let ((environment-addresses
         (append
          (loop
             for arg in environment
             collect (let* ((bytes (babel:string-to-octets arg))
                            (len (length bytes)))
                       (decf (llvm-context-stack-pointer context) (1+ len))
                       (align-stack context)
                       (replace (llvm-context-memory context)
                                bytes
                                :start1 (- (llvm-context-stack-pointer context) +null-avoidance-offset+))
                       (setf (aref (llvm-context-memory context)
                                   (+ (- (llvm-context-stack-pointer context) +null-avoidance-offset+)
                                      len))
                             0)
                       (llvm-context-stack-pointer context)))
          ;; Include a final NULL entry.
          (list 0)))
        (argument-addresses
         (append
          (loop
             for arg in arguments
             collect (let* ((bytes (babel:string-to-octets arg))
                            (len (length bytes)))
                       (decf (llvm-context-stack-pointer context) (1+ len))
                       (align-stack context)
                       (replace (llvm-context-memory context)
                                bytes
                                :start1 (- (llvm-context-stack-pointer context) +null-avoidance-offset+))
                       (setf (aref (llvm-context-memory context)
                                   (+ (- (llvm-context-stack-pointer context) +null-avoidance-offset+)
                                      len))
                             0)
                       (llvm-context-stack-pointer context)))
          ;; Include a final NULL entry.
          (list 0)))
        (envp nil)
        (argv nil)
        (file-table (unix-personality-file-table (llvm-context-personality context))))
    (dolist (sym '(*standard-input* *standard-output* *error-output*))
      (vector-push-extend sym file-table))
    ;; Copy environment addresses the stack.
    ;; This forms the envp array.
    (decf (llvm-context-stack-pointer context) (* (length environment)
                                                  (if (llvm-context-is-32-bit context)
                                                      4
                                                      8)))
    (align-stack context)
    (loop
       for i from (llvm-context-stack-pointer context) by (if (llvm-context-is-32-bit context)
                                                              4
                                                              8)
       for addr in environment-addresses
       do
         (if (llvm-context-is-32-bit context)
             (setf (nibbles:ub32ref/le (llvm-context-memory context) (- i +null-avoidance-offset+)) addr)
             (setf (nibbles:ub64ref/le (llvm-context-memory context) (- i +null-avoidance-offset+)) addr)))
    (setf envp (llvm-context-stack-pointer context))
    ;; And copy argument addresses the stack.
    ;; This forms the argv array.
    (decf (llvm-context-stack-pointer context) (* (length arguments)
                                                  (if (llvm-context-is-32-bit context)
                                                      4
                                                      8)))
    (align-stack context)
    (loop
       for i from (llvm-context-stack-pointer context) by (if (llvm-context-is-32-bit context)
                                                              4
                                                              8)
       for addr in argument-addresses
       do
         (if (llvm-context-is-32-bit context)
             (setf (nibbles:ub32ref/le (llvm-context-memory context) (- i +null-avoidance-offset+)) addr)
             (setf (nibbles:ub64ref/le (llvm-context-memory context) (- i +null-avoidance-offset+)) addr)))
    (setf argv (llvm-context-stack-pointer context))
    (unwind-protect
         (catch 'exit
           (call-with-graphics-support
            (lambda ()
              (funcall (llvm-context-entry-point context) context (length arguments) argv envp))))
      (dotimes (i (length file-table))
        (when (not (symbolp (aref file-table i)))
          (close (aref file-table i))
          (setf (aref file-table i) nil))))))

(defun main (context &rest arguments)
  (main-1 context arguments (list (format nil "HOME=~A" (user-homedir-pathname)))))

(defun read-c-string (llvm-context address &optional max-len)
  (cond ((zerop address)
         nil)
        (t
         (let ((end (position 0 (llvm-context-memory llvm-context)
                              :start (- address +null-avoidance-offset+)
                              :end (if max-len
                                       (+ (- address +null-avoidance-offset+)
                                          max-len)
                                       nil))))
           (babel:octets-to-string (subseq (llvm-context-memory llvm-context)
                                           (- address +null-avoidance-offset+)
                                           end))))))

(defun resolve-fd (unix fd)
  (let ((table (unix-personality-file-table unix)))
    (cond ((< fd 0) nil)
          ((>= fd (length table)) nil)
          (t (aref table fd)))))

(defun parse-unix-path (unix path &key ensure-directory)
  (merge-pathnames
   (uiop/pathname:parse-unix-namestring
    path
    :defaults (or (unix-personality-current-directory unix)
                  *default-pathname-defaults*)
    :ensure-directory ensure-directory)
   (or (unix-personality-current-directory unix)
       *default-pathname-defaults*)))

(define-llvm-function |_exit| ((status))
  (throw 'exit status))

(define-llvm-function |sbrk| ((increment) :personality unix :context context)
  (prog1
      ;; sbrk returns the previous brk.
      (unix-personality-brk unix)
    (let ((new-brk (+ (unix-personality-brk unix)
                      (sign-extend increment (if (llvm-context-is-32-bit context)
                                                 32
                                                 64))))
          (actual-size (+ (length (llvm-context-memory context))
                          +null-avoidance-offset+)))
      (when (> new-brk actual-size)
        ;; Resize the memory in 16MB chunks, to avoid excessive consing & copying.
        (let* ((new-true-size (align-up new-brk (* 16 1024 1024)))
               (new-array (make-array (- new-true-size +null-avoidance-offset+) :element-type '(unsigned-byte 8))))
          (declare (optimize speed (safety 0)))
          (replace (the octet-vector new-array)
                   (the octet-vector (llvm-context-memory context)))
          (setf (llvm-context-memory context) new-array)))
      (setf (unix-personality-brk unix) new-brk))))

;; Not defined using define-llvm-function due to the optional mode argument.
;; Technically open is a variadic function.
(defun |open| (llvm-context pathname flags &rest blah)
  (declare (ignore blah))
  (let ((unix (llvm-context-personality llvm-context))
        (path (read-c-string llvm-context pathname))
        (direction (ecase (logand flags 3)
                     (0 :input)
                     (1 :output)
                     (2 :io)))
        (append (logtest flags #x0008))
        (create (logtest flags #x0200))
        (trunc  (logtest flags #x0400))
        (excl   (logtest flags #x0800)))
    (let ((stream (open (parse-unix-path unix path)
                        :direction direction
                        :element-type '(unsigned-byte 8)
                        :if-exists (cond (excl
                                          nil)
                                         (append
                                          :append)
                                         (trunc
                                          :supersede)
                                         (t
                                          :overwrite))
                        :if-does-not-exist (cond (create
                                                  :create)
                                                 (t
                                                  nil)))))
      (cond (stream
             ;; Find a free file descriptor.
             (let ((fd (position nil (unix-personality-file-table unix))))
               (when (not fd)
                 (setf fd (vector-push-extend nil (unix-personality-file-table unix))))
               (setf (aref (unix-personality-file-table unix) fd) stream)
               fd))
            (t
             #xFFFFFFFF)))))

(define-llvm-function |close| ((fd) :personality unix)
  (let ((file (resolve-fd unix fd)))
    (cond (file
           (when (not (symbolp file))
             (close file))
           (setf (aref (unix-personality-file-table unix) fd) nil)
           0)
          (t
           #xFFFFFFFF))))

(define-llvm-function |lseek| ((fd offset whence) :personality unix)
  (let ((file (resolve-fd unix fd)))
    (cond ((not file) #xFFFFFFFF)
          ((symbolp file) 0)
          (t
           (file-position file (ecase whence
                                 (0 ; SEEK_SET
                                  offset)
                                 (1 ; SEEK_CUR
                                  (add.i64 (file-position file)
                                           offset))
                                 (2 ; SEEK_END
                                  (add.i64 (file-length file)
                                           offset))))
           (file-position file)))))

(define-llvm-function |write| ((fd buf count) :personality unix :context context)
  (let ((file (resolve-fd unix fd)))
    (cond ((not file) #xFFFFFFFF)
          ((symbolp file)
           ;; One of the interactive streams.
           (write-string (babel:octets-to-string (llvm-context-memory context)
                                                 :start (- buf +null-avoidance-offset+)
                                                 :end (+ (- buf +null-avoidance-offset+) count))
                         (symbol-value file))
           count)
          (t
           (write-sequence (llvm-context-memory context)
                           file
                           :start (- buf +null-avoidance-offset+)
                           :end (+ (- buf +null-avoidance-offset+) count))
           count))))

(define-llvm-function |read| ((fd buf count) :personality unix :context context)
  (let ((file (resolve-fd unix fd)))
    (cond ((not file) #xFFFFFFFF)
          ((symbolp file)
           ;; One of the interactive streams.
           ;; Somewhat tricky, need to read & convert characters, but limited by number of bytes.
           (error "TODO: Read interactive stream ~S" file))
          (t
           (- (read-sequence (llvm-context-memory context)
                             file
                             :start (- buf +null-avoidance-offset+)
                             :end (+ (- buf +null-avoidance-offset+) count))
              (- buf +null-avoidance-offset+))))))

(define-llvm-function |fstat| ((fd buf) :personality unix)
  (let ((file (resolve-fd unix fd)))
    (if file
        0
        #xFFFFFFFF)))

(define-llvm-function |isatty| ((fd) :personality unix)
  (let ((file (resolve-fd unix fd)))
    (cond ((not file) #xFFFFFFFF)
          ((symbolp file)
           1)
          (t
           0))))

(define-llvm-function |access| ((pathname mode) :personality unix :context context)
  (let ((path (read-c-string context pathname)))
    (let ((stream (open (parse-unix-path unix path)
                        :direction :probe
                        :element-type '(unsigned-byte 8)
                        :if-does-not-exist nil)))
      (cond (stream
             0)
            (t
             #xFFFFFFFF)))))

(define-llvm-function |mkdir| ((pathname mode) :personality unix :context context)
  (let ((path (read-c-string llvm-context pathname)))
    (ensure-directories-exist (parse-unix-path unix path :ensure-directory t))
    0))

(define-llvm-function |gettimeofday| ((tv tz))
  (let* ((unix-epoch (encode-universal-time 0 0 0 1 1 1970 0))
         (current-time (/ (get-internal-real-time)
                          internal-time-units-per-second))
         (unix-time (- current-time unix-epoch)))
    (multiple-value-bind (seconds fractions)
        (truncate unix-time)
      (store.i32 (ldb (byte 32 0) seconds) tv)
      (store.i32 (ldb (byte 32 0) (truncate (* fractions 1000 1000)))
                 (+ tv 4))))
  0)

(define-llvm-function |nanosleep| ((req rem))
  (let* ((seconds (load.i32 req))
         (nanoseconds (load.i32 (+ req 4)))
         (delay (+ seconds (/ (float nanoseconds)
                              1000000000.0))))
    (sleep delay))
  (when (not (zerop rem))
      (store.i32 0 rem)
      (store.i32 0 (+ rem 4)))
  0)
