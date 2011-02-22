(in-package :recognize)

(eval-when (:execute :load-toplevel :compile-toplevel)
  (defun collect-features (features feature-defs &key (feature-vector-name 'fv)
                           (extra-vector-name 'ev))
    (let ((used-features (make-array (length features)
                                     :adjustable t :fill-pointer 0))
          (index-map (make-hash-table)))
      (labels ((add-deps (name)
                 (unless (gethash name index-map)
                   (let ((deps (second (gethash name feature-defs))))
                     (loop for i in deps
                        do (add-deps i))
                     (setf (gethash name index-map)
                           (vector-push-extend (list name :indirect)
                                               used-features))))))
        (loop for f in features
           do (add-deps f)
           (setf (second (aref used-features (gethash f index-map)))
                 :direct)))
      (loop for i below (length used-features)
         with direct = 0
         with indirect = 0
         for (n usage) = (aref used-features i)
         for accessor = (if (eq usage :direct)
                            `(aref ,feature-vector-name ,(1- (incf direct)))
                            `(aref ,extra-vector-name ,(1- (incf indirect))))
         do (setf (aref used-features i) (list n usage accessor)))
      used-features)))

(defmacro define-feature-set (name () &body features)
  (let* ((features (collect-features features *feature-definitions*))
         (count (gensym))
         (double-array '(make-array 0 :element-type 'double-float
                         :adjustable t :fill-pointer 0))
         (feature-init (loop for (name usage) across features
                          when (eq usage :direct)
                          collect (third (gethash name *feature-definitions*))))
         (temp-init (loop for (name usage) across features
                       when (not (eq usage :direct))
                       collect (third (gethash name *feature-definitions*)))))
    `(progn
       (defclass ,name ()
         ((x :accessor x :initform ,double-array)
          (y :accessor y :initform ,double-array)
          (time :accessor times :initform ,double-array)
          (feature-vector :accessor feature-vector
                          :initform (make-array '(,(length feature-init))
                                                :element-type 'double-float
                                                :initial-contents ',feature-init))
          (temp-vector :accessor temp-vector
                       :initform (make-array '(,(length temp-init))
                                             :element-type 'double-float
                                             :initial-contents ',temp-init))))
       (defmethod feature-set-size ((set ,name))
         ,(length feature-init))
       (defmethod feature-set-size ((set (eql ',name)))
         ,(length feature-init))
       (defmethod reset-feature-set ((set ,name))
         (map-into (feature-vector set) #'identity ',feature-init)
         (map-into (temp-vector set) #'identity ',temp-init))

       (defmethod update-feature-set ((set ,name) nx ny ntime)
         (let ((,count (length (x set)))
               (x (x set))
               (y (y set))
               (time (times set))
               (fv (feature-vector set))
               (ev (temp-vector set)))
           (declare (ignorable ev))
           (vector-push-extend (float nx 1d0) x)
           (vector-push-extend (float ny 1d0) y)
           (vector-push-extend (float ntime 1d0) time)
           (with-feature-helpers (,count)
             ,@(loop for (name nil accessor) across features
                  for (axes deps init update) = (gethash name *feature-definitions*)
                  collect `(setf ,accessor
                                 (float
                                  (,update
                                   ,accessor
                                   ,count
                                   ,@(loop for i in deps
                                        collect (third (find i features :key 'car)))
                                   ,@axes)
                                  1d0)))))))))

(define-feature-set foo0 () initial-angle-cos)

(define-feature-set rubine11 ()
  initial-angle-cos initial-angle-sin
  bounding-box-diagonal bounding-box-angle
  start-end-distance start-end-angle-cos start-end-angle-sin
  path-length total-angle total-abs-angle total-angle-squared)

(define-feature-set rubine12 ()
  initial-angle-cos initial-angle-sin
  bounding-box-diagonal bounding-box-angle
  start-end-distance start-end-angle-cos start-end-angle-sin
  path-length total-angle total-abs-angle total-angle-squared
  max-speed-squared)

(define-feature-set rubine13 ()
  initial-angle-cos initial-angle-sin
  bounding-box-diagonal bounding-box-angle
  start-end-distance start-end-angle-cos start-end-angle-sin
  path-length total-angle total-abs-angle total-angle-squared
  max-speed-squared path-duration)
