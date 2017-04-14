;;;; Copyright (c) 2017 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :llvm-runtime)

(defparameter *allow-grab* nil
  "When false, attempts to grab input will be ignored.
Some window managers handle this badly and make it impossible to ungrab
input from a frozen or otherwise uncooperative program.")

(defvar *sdl-enabled* nil)

(defvar *sdl-width*)
(defvar *sdl-height*)
(defvar *sdl-event*)

(defun call-with-graphics-support (fn)
  (sdl:with-init ()
    (let ((*sdl-enabled* t)
          (*sdl-width* 0)
          (*sdl-height* 0)
          (*sdl-event* (sdl:new-event)))
      (unwind-protect
           (funcall fn)
        (sdl:free-event *sdl-event*)))))

(define-llvm-function |_iota_video_init| (())
  (if *sdl-enabled*
      0
      1))

(define-llvm-function |_iota_video_quit| (())
  0)

(define-llvm-function |_iota_set_video_mode| ((width height))
  ;; FIXME: This should make sure a 32-bit XRGB surface is created.
  (cond (*sdl-enabled*
         (sdl:window width height)
         (setf *sdl-width* width
               *sdl-height* height)
         0)
        (t
         1)))

(define-llvm-function |_iota_video_update| ((buf))
  (when (not (zerop buf))
    (sdl:with-pixel (p (sdl:fp sdl:*default-display*))
      (loop
         with data = (sdl:pixel-data p)
         with memory = (the octet-vector (llvm-context-memory llvm-context))
         with adjusted-buf = (- buf +null-avoidance-offset+)
         for i below (* *sdl-width* *sdl-height* 4)
         do (setf (cffi:mem-ref data :unsigned-char i)
                  (aref memory (+ adjusted-buf i)))))
    (sdl:update-display))
  0)

;; See SDL-1.2.15/src/video/iota/SDL_iotaevents.c for event definitions.
(define-llvm-function |_iota_poll_event| ((buf))
  (loop
     until (zerop (sdl-cffi::SDL-Poll-Event *sdl-event*))
     do
       (case (sdl:event-type *sdl-event*)
         (:quit-event
          (store.i32 0 buf)
          (return 1))
         (:key-down-event
          (store.i32 1 buf)
          (store.i32 (sdl::key-scancode *sdl-event*) (+ buf 4))
          (store.i32 (sdl::key-mod *sdl-event*) (+ buf 8))
          (store.i32 (cffi:foreign-enum-value 'sdl-cffi::SDL-Key (sdl::key-key *sdl-event*)) (+ buf 12))
          (return 1))
         (:key-up-event
          (store.i32 2 buf)
          (store.i32 (sdl::key-scancode *sdl-event*) (+ buf 4))
          (store.i32 (sdl::key-mod *sdl-event*) (+ buf 8))
          (store.i32 (cffi:foreign-enum-value 'sdl-cffi::SDL-Key (sdl::key-key *sdl-event*)) (+ buf 12))
          (return 1))
         (:mouse-motion-event
          (store.i32 3 buf)
          (store.i32 (ldb (byte 32 0) (sdl::mouse-motion-x-rel *sdl-event*)) (+ buf 4))
          (store.i32 (ldb (byte 32 0) (sdl::mouse-motion-y-rel *sdl-event*)) (+ buf 8))
          (return 1))
         (:mouse-button-down-event
          (store.i32 4 buf)
          (store.i32 (sdl::mouse-button-button *sdl-event*) (+ buf 4))
          (return 1))
         (:mouse-button-up-event
          (store.i32 5 buf)
          (store.i32 (sdl::mouse-button-button *sdl-event*) (+ buf 4))
          (return 1))
         (:active-event
          (store.i32 6 buf)
          (store.i32 (sdl::active-gain *sdl-event*) (+ buf 4))
          (store.i32 (sdl::active-state *sdl-event*) (+ buf 8))
          (return 1))
         (t
          (format t "Ignoring SDL event ~S~%" (sdl:event-type *sdl-event*))))
     finally (return 0)))

(define-llvm-function |_iota_grab_input| ((mode))
  (cond (*allow-grab*
         (cffi:foreign-enum-value
          'sdl-cffi::SDL-Grab-Mode
          (sdl:sdl-wm-grab-input (if (zerop mode)
                                     :sdl-grab-off
                                     :sdl-grab-on))))
        (t
         (if (zerop mode)
             0
             1))))

(define-llvm-function |_iota_warp_cursor| ((x y))
  (sdl-cffi::sdl-warp-mouse x y))

(define-llvm-function |_iota_show_cursor| ((toggle))
  (sdl:show-cursor (not (eql toggle 0))))

(define-llvm-function |_iota_set_caption| ((title icon))
  (sdl:set-caption (read-c-string llvm-context title)
                   (read-c-string llvm-context icon)))
