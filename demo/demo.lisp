;; (require 'glop)
(defpackage #:gesture-demo
  (:use :cl))
(in-package :gesture-demo)

(defparameter *gdp11*
  (recognize::train-classifier
   (recognize::make-classifier)
   (with-open-file (s (asdf:system-relative-pathname
                       'recognize-demo "demo/gdp-training-data.lisp"))
     (read s))))

(defun now ()
  (float (/ (get-internal-real-time) internal-time-units-per-second) 1d0))

(defclass gesture-demo (glop:window)
  ((dragging :initform nil :accessor dragging)
   #++(gesture-points :initform (make-array 32 :adjustable t :fill-pointer 0)
                      :accessor gesture-points)
   (gesture-points :initform nil
                   :accessor gesture-points)
   (start-time :accessor start-time)))

(defmethod glop:on-event ((window gesture-demo) (event glop:key-event))
  (when (eq (glop:keysym event) :escape)
    (glop:push-close-event window)))

(defmethod glop:on-event ((window gesture-demo) (event glop:button-press-event))
  (format t "click ~s...~%" (glop:button event))
  (when (= 1 (glop:button event))

    (setf ;;(fill-pointer (gesture-points window)) 0
     (dragging window) t
     (gesture-points window) nil
     (start-time window) (now))))

(defmethod glop:on-event ((window gesture-demo) (event glop:button-release-event))
  (when (= 1 (glop:button event))
    (setf (dragging window) nil)
    (format t "path = ~:{~s ~s ~s ~4,2f~%~}" (reverse (gesture-points window)))
    (format t "recognized path (len ~s) as ~s~%"
            (length (gesture-points window))
            (recognize::name (recognize::classify-path *gdp11* (nreverse (gesture-points window)))))))

(defmethod glop:on-event ((window gesture-demo) (event glop:mouse-motion-event))
  (when (dragging window)
    (push (list 0 (glop:x event)
                (- (glop:window-height window) (glop:y event))
                (- (now) (start-time window)))
          (gesture-points window))))

(defmethod glop:on-event ((window gesture-demo) (event glop:resize-event))
  (gl:viewport 0 0 (glop:width event) (glop:height event)))

(defmethod glop:on-event ((window gesture-demo) (event glop:expose-event))
  (declare (ignore event)))

(defmethod glop:on-event ((window gesture-demo) (event glop:close-event))
  (declare (ignore event))
  (format t "Close~%"))

(defmacro with-continue-restart (&body body)
  `(restart-case
       (progn ,@body)
     (continue () :report "Continue"  )))

(defun draw (win)
  (gl:enable :blend :line-smooth)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:clear :color-buffer)
  (gl:color 0 1 1 0.2)
  (gl:with-primitive :polygon
    (gl:vertex 0.25 0.25 0)
    (gl:vertex 0.75 0.25 0)
    (gl:vertex 0.75 0.75 0)
    (gl:vertex 0.25 0.75 0))
  (when (dragging win)
    (gl:color 0 0 0 0.5)
    (gl:line-width 3)
    (gl:with-primitive :line-strip
      (loop for (nil x y) in (gesture-points win)
         do (gl:vertex (/ x (glop:window-width win))
                       (/ y (glop:window-height win)))))
    (gl:color 1 1 1 1)
    (gl:line-width 1.5)
    (gl:with-primitive :line-strip
      (loop for (nil x y) in (gesture-points win)
         do (gl:vertex (/ x (glop:window-width win))
                       (/ y (glop:window-height win))))))

  (gl:flush)
  (glop:swap-buffers win)
  (sleep 0.001))

(defun gesture-demo ()
  (glop:with-window (win "gesture demo" 800 600 :win-class 'gesture-demo)
    (gl:clear-color 0.3 0.3 1.0 0)
    (gl:matrix-mode :projection)
    (gl:load-identity)
    (gl:ortho 0 1 0 1 -1 1)
    (gl:matrix-mode :modelview)
    (loop while (restart-case
                    (glop:dispatch-events win :blocking nil :on-foo nil)
                  (continue () :report "skip event" t)
                  (exit-main-loop () :report "exit main loop" nil))
       do
         (with-continue-restart (draw win)))))


#++
(gesture-demo)
