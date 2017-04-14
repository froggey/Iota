;;;; Copyright (c) 2017 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :llvm-runtime)

(defvar *graphics-enabled* nil)

(defvar *graphics-width*)
(defvar *graphics-height*)
(defvar *graphics-fifo*)
(defvar *graphics-window*)
(defvar *graphics-frame*)

(defun call-with-graphics-support (fn)
  (let ((*graphics-enabled* t)
        (*graphics-width* 0)
        (*graphics-height* 0)
        (*graphics-fifo* (mezzano.supervisor:make-fifo 50))
        (*graphics-window* nil)
        (*graphics-frame* nil))
    (unwind-protect
         (funcall fn)
      (when *graphics-window*
        (mezzano.gui.compositor:close-window *graphics-window*)))))

(define-llvm-function |_iota_video_init| (())
  (if *graphics-enabled*
      0
      1))

(define-llvm-function |_iota_video_quit| (())
  0)

(defun compute-window-size (width height)
  ;; Make a fake frame to get the frame size.
  (multiple-value-bind (left right top bottom)
      (mezzano.gui.widgets:frame-size (make-instance 'mezzano.gui.widgets:frame))
    (values (+ left (max 32 width) right)
            (+ top (max 32 height) bottom))))

(define-llvm-function |_iota_set_video_mode| ((width height))
  (cond (*graphics-enabled*
         (multiple-value-bind (real-width real-height)
             (compute-window-size width height)
           (when *graphics-window*
             (mezzano.gui.compositor:close-window *graphics-window*)
             (setf *graphics-window* nil))
           (setf *graphics-window* (mezzano.gui.compositor:make-window
                                    *graphics-fifo*
                                    real-width
                                    real-height)
                 *graphics-width* width
                 *graphics-height* height)
           (setf *graphics-frame*
                 (make-instance 'mezzano.gui.widgets:frame
                                :framebuffer (mezzano.gui.compositor:window-buffer *graphics-window*)
                                :title "LLVM host"
                                :close-button-p t
                                :damage-function (mezzano.gui.widgets:default-damage-function *graphics-window*)))
           (mezzano.gui.widgets:draw-frame *graphics-frame*)
         0))
        (t
         1)))

(define-llvm-function |_iota_video_update| ((buf))
  (declare (optimize (speed 3) (safety 0)))
  (when (not (zerop buf))
    (multiple-value-bind (left right top bottom)
        (mezzano.gui.widgets:frame-size *graphics-frame*)
      (declare (ignore right bottom))
      (loop
         with framebuffer = (mezzano.gui.compositor:window-buffer *graphics-window*)
         with pixels = (the (simple-array (unsigned-byte 32) (*))
                            (sys.int::%complex-array-storage
                             (mezzano.gui:surface-pixels framebuffer)))
         with win-width = (mezzano.gui:surface-width framebuffer)
         with win-height = (mezzano.gui:surface-height framebuffer)
         with g-width = *graphics-width*
         with g-height = *graphics-height*
         with memory = (the octet-vector (llvm-context-memory llvm-context))
         with adjusted-buf = (- buf +null-avoidance-offset+)
         for y below g-height
         do
           (loop
              for x below g-width
              do (setf (aref pixels (+ left x (* (+ top y) win-width)))
                       (logior #xFF000000
                               (load.i32 (+ buf (* (+ x (* y g-width)) 4)))))))
      (mezzano.gui.compositor:damage-window *graphics-window*
                                            left top
                                            *graphics-width* *graphics-height*)))
  0)

(defun translate-sdl-keysym (translated-key original-key)
  (case original-key
    (#\Newline        13)
    (#\F1            282)
    (#\F2            283)
    (#\F3            284)
    (#\F4            285)
    (#\F5            286)
    (#\F6            287)
    (#\F7            288)
    (#\F8            289)
    (#\F9            290)
    (#\F10           291)
    (#\F11           292)
    (#\F12           293)
    (#\F13           294)
    (#\F14           295)
    (#\F15           296)
    (#\Insert        277)
    (#\Delete        127)
    (#\Home          278)
    (#\End           279)
    (#\Page-Up       280)
    (#\Page-Down     281)
    (#\Left-Arrow    276)
    (#\Right-Arrow   275)
    (#\Up-Arrow      273)
    (#\Down-Arrow    274)
    (#\Menu          319)
    (#\Print-Screen  317)
    (#\Pause          19)
    (#\Break         318)
    (#\Caps-Lock     301)
    (#\Left-Shift    305)
    (#\Right-Shift   304)
    (#\Left-Control  306)
    (#\Right-Control 305)
    (#\Left-Meta     308)
    (#\Right-Meta    307)
    (#\Left-Super    312)
    (#\Right-Super   311)
    (#\KP-0          256)
    (#\KP-1          257)
    (#\KP-2          258)
    (#\KP-3          259)
    (#\KP-4          260)
    (#\KP-5          261)
    (#\KP-6          262)
    (#\KP-7          263)
    (#\KP-8          264)
    (#\KP-9          265)
    (#\KP-Period     266)
    (#\KP-Divide     267)
    (#\KP-Multiply   268)
    (#\KP-Minus      269)
    (#\KP-Plus       270)
    (#\KP-Enter      271)
    (t (char-code translated-key))))

(define-llvm-function |_iota_poll_event| ((buf))
  (loop
     (let ((evt (mezzano.supervisor:fifo-pop *graphics-fifo* nil)))
       (when (not evt) (return 0))
       (typecase evt
         (mezzano.gui.compositor:window-close-event
          (store.i32 0 buf) ; quit
          (return 1))
         (mezzano.gui.compositor:quit-event
          (store.i32 0 buf) ; quit
          (return 1))
         (mezzano.gui.compositor:mouse-event
          (handler-case
              (progn
                (mezzano.gui.widgets:frame-mouse-event *graphics-frame* evt)
                ;; FIXME: This needs to send multiple events...
                (cond ((not (zerop (mezzano.gui.compositor:mouse-button-change evt)))
                       ;; Find the changed button. Hope only one changed.
                       (let ((button (loop
                                        for i from 0
                                        until (logbitp i (mezzano.gui.compositor:mouse-button-change evt))
                                        finally (return i))))
                          (store.i32 (if (logbitp button (mezzano.gui.compositor:mouse-button-state evt))
                                         4 ; mouse-button-down
                                         5) ; mouse-button-up
                                     buf)
                          (store.i32 (1+ button) (+ buf 4))))
                      (t
                       ;; No changes, send a motion event.
                       (store.i32 3 buf) ; mouse-motion
                       (store.i32 (ldb (byte 32 0) (mezzano.gui.compositor:mouse-x-motion evt)) (+ buf 4))
                       (store.i32 (ldb (byte 32 0) (mezzano.gui.compositor:mouse-y-motion evt)) (+ buf 8))))
                (return 1))
            (mezzano.gui.widgets:close-button-clicked ()
              (store.i32 0 buf) ; quit
              (return 1))))
         (mezzano.gui.compositor:key-event
          (store.i32 (if (mezzano.gui.compositor:key-releasep evt)
                         2 ; key-up
                         1) ; key-down
                     buf)
          (store.i32 (char-code (mezzano.gui.compositor:key-scancode evt)) (+ buf 4))
          (store.i32 0 (+ buf 8)) ;; FIXME
          (store.i32 (translate-sdl-keysym (mezzano.gui.compositor:key-key evt)
                                           (mezzano.gui.compositor:key-scancode evt))
                     (+ buf 12))
          (return 1))
         (mezzano.gui.compositor:window-activation-event
          (setf (mezzano.gui.widgets:activep *graphics-frame*) (mezzano.gui.compositor:state evt))
          (mezzano.gui.widgets:draw-frame *graphics-frame*)
          (store.i32 6 buf) ; Activation event
          (store.i32 (if (mezzano.gui.compositor:state evt)
                         1
                         0)
                     (+ buf 4))
          (store.i32 #x7 (+ buf 8))))))) ; mouse, input, and active.

(defun set-input-grab (grabp)
  (multiple-value-bind (left right top bottom)
      (mezzano.gui.widgets:frame-size *graphics-frame*)
    (declare (ignore right bottom))
    ;; Clamp the grab region to the interior of the frame, not the whole window.
    (mezzano.gui.compositor:grab-cursor *graphics-window* grabp
                                        left top
                                        *graphics-width* *graphics-height*)))

(define-llvm-function |_iota_grab_input| ((mode))
  (set-input-grab (not (zerop mode)))
  mode)

(define-llvm-function |_iota_warp_cursor| ((x y))
  )

(define-llvm-function |_iota_show_cursor| ((toggle))
  (set-input-grab (zerop toggle))
  (mezzano.gui.compositor:set-window-data
   *graphics-window*
   :cursor (if (not (eql toggle 0))
               :default
               :none)))

(define-llvm-function |_iota_set_caption| ((title icon))
  (let ((title-text (read-c-string llvm-context title)))
    (setf (mezzano.gui.widgets:frame-title *graphics-frame*) title-text)
    (mezzano.gui.widgets:draw-frame *graphics-frame*)
    (mezzano.gui.compositor:set-window-data *graphics-window* :title title-text)))
