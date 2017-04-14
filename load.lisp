(in-package :cl-user)

(ql:quickload :iota)

;; It's possible to directly load the translated code, but that gets really
;; tedious. Loading a compiled file is much faster.
(defun compile-and-load (path &key force)
  "Compile and load PATH.
The file will only be recompiled if the source is newer than the output file, or if FORCE is true."
  (let ((compiled (compile-file-pathname path)))
    (when (or force
              (not (probe-file compiled))
              (<= (file-write-date compiled) (file-write-date path)))
      (format t "; Compiling ~S~%" path)
      (ignore-errors (delete-file compiled))
      (compile-file path))
    (format t "; Loading ~S~%" compiled)
    (load compiled)))

(defpackage :prboom
  (:use :cl :llvm-runtime)
  (:export #:make-context))

(compile-and-load "prboom.lisp")

(defun run-prboom (&rest arguments)
  (llvm-runtime:main-1
   (prboom:make-context)
   (list* "prboom" arguments)
   (list (format nil "HOME=~A" (namestring (user-homedir-pathname))))))

(defpackage :sdlquake
  (:use :cl :llvm-runtime)
  (:export #:make-context))

(compile-and-load "sdlquake.lisp")

(defun run-sdlquake (&rest arguments)
  (llvm-runtime:main-1
   (sdlquake:make-context)
   (list* "sdlquake" arguments)
   (list (format nil "HOME=~A" (namestring (user-homedir-pathname))))))

;; Disable floating point traps for Quake.
#+sbcl
(sb-int:set-floating-point-modes :traps '())
#+ccl
(ccl:set-fpu-mode :overflow nil
                  :underflow nil
                  :division-by-zero nil
                  :invalid nil
                  :inexact nil)
