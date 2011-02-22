(in-package :recognize)

(defparameter *feature-definitions* (make-hash-table))

(defmacro with-feature-helpers ((count) &body body)
  `(labels ((start (param)
              (aref param 0))
            (current (param)
              (aref param ,count))
            (prev (param &optional (d 1))
              (aref param (max 0 (- ,count d))))
            (delta (param &optional (d 1))
              (- (current param) (prev param d)))
            (deltan (param d1 d2)
              (- (prev param d1) (prev param d2)))
            (start-delta (param)
              (- (current param) (start param)))
            (mag-sq (a b)
              (+ (expt a 2) (expt b 2)))
            (mag (a b)
              (sqrt (+ (expt a 2) (expt b 2)))))
     (declare (ignorable #'start #'current #'prev #'delta #'start-delta #'mag #'mag-sq #'deltan))
     ,@body))

(defmacro define-feature (name (prev count &key deps axes) &body body)
  (let ((prev (if (consp prev) (first prev) prev))
        (init (if (consp prev) (second prev) 0d0))
        (deps (alexandria:ensure-list deps))
        (axes (alexandria:ensure-list axes)))
    (print `(setf (gethash ',name *feature-definitions*)
                  (list ',axes ',deps ,init
                        '(lambda (,prev ,count ,@deps ,@axes)
                          (declare (type (array double-float (*))
                                    ,@axes)
                           (double-float ,prev ,@deps)
                           (fixnum ,count)
                           (ignorable ,prev ,count))
                          ,@body))))))

(define-feature initial-angle-cos (p c :axes (x y))
  (cond
    ((> c 2)
     p)
    ((> c 0)
     (let* ((dx (start-delta x))
                    (d (mag dx (start-delta y))))
               (if #++(zerop d) (< d 3)
                   0d0
                   (/ dx d))))
    (t 0d0)))

(define-feature initial-angle-sin (p c :axes (x y))
  (cond
    ((> c 2) p)
    ((> c 0) (let* ((dy (start-delta y))
                        (d (mag (start-delta x) dy)))
               (if #++(zerop d) (< d 3)
                   0d0
                   (/ dy d))))
    (t 0d0)))

(define-feature xmin (p c :axes (x))
  (if (zerop c)
      (current x)
      (min p (current x))))

(define-feature xmax (p c :axes (x))
  (if (zerop c)
      (current x)
      (max p (current x))))

(define-feature ymin (p c :axes (y))
  (if (zerop c)
      (current y)
      (min p (current y))))

(define-feature ymax (p c :axes (y))
  (if (zerop c)
      (current y)
      (max p (current y))))

(define-feature bounding-box-diagonal (p c :deps (xmin xmax ymin ymax))
  (mag (- xmax xmin) (- ymax ymin)))

(define-feature bounding-box-angle (p c :deps (xmin xmax ymin ymax))
  (atan (- ymax ymin) (- xmax xmin)))

(define-feature start-end-distance (p c :axes (x y))
  (mag (start-delta x) (start-delta y)))

(defparameter *dist-threshhold* 3)
(defparameter *se-angle-rolloff* 4)

(define-feature start-end-angle-cos (p c :deps start-end-distance
                                       :axes x)
  ;; original code scales value down when distance from start to end
  ;; is less than 4 pixels, since angles jump around a lot from a
  ;; single pixel move
  (cond
    ((zerop start-end-distance) 0.0)
    ((< start-end-distance *se-angle-rolloff*)
     (* (/ (start-delta x) start-end-distance)
        (/ (expt start-end-distance 2) (expt *se-angle-rolloff* 2))))
    (t (/ (start-delta x) start-end-distance))))

(define-feature start-end-angle-sin (p c :deps start-end-distance
                                          :axes y)
  ;; original code scales value down when distance from start to end
  ;; is less than 4 pixels, since angles jump around a lot from a
  ;; single pixel move
  (cond
    ((zerop start-end-distance) 0.0)
    ((< start-end-distance *se-angle-rolloff*)
     (* (/ (start-delta y) start-end-distance)
        (/ (expt start-end-distance 2) (expt *se-angle-rolloff* 2))))
    (t (/ (start-delta y) start-end-distance))))

(define-feature path-length (p c :axes (x y))
  (+ p (mag (delta x) (delta y))))

(define-feature current-angle (p c :axes (x y))
  (if (< c 2)
      0.0
      (let ((dx (delta x))
            (dx-1 (deltan x 1 2))
            (dy (delta y))
            (dy-1 (deltan y 1 2)))
        (atan (- (* dx dy-1) (* dx-1 dy))
              (+ (* dx dx-1) (* dy dy-1))))))


(define-feature total-angle (p c :deps current-angle)
  (+ p current-angle))

(define-feature total-abs-angle (p c :deps current-angle)
  (+ p (abs current-angle)))

(define-feature total-angle-squared (p c :deps current-angle)
  (+ p (expt current-angle 2)))

(define-feature max-speed-squared (p c :axes (x y time))
  (if (zerop (delta time))
      p
      (max p (/ (mag (delta x) (delta y))
                (expt (delta time) 2)))))

(define-feature path-duration (p c :axes time)
  (start-delta time))


;; pressure stuff?

;; average angle from start?
;;   (adjusted for segment length, not just at samples)
;;    or maybe xproducts of p..start, p-1..start?
;;   might just be equivalent to pos of centroid relative to start?