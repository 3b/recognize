(in-package #:recognize)

(defclass classifier-class ()
  ((name :accessor name :initarg :name)
   (examples :accessor examples :initform nil)
   (average :accessor average)
   (covariance-matrix :accessor covariance-matrix)))

(defclass classifier ()
  ((feature-set :initarg :feature-set :accessor feature-set)
   (class-index :initform (make-hash-table :test 'equal) :accessor class-index)
   (classes :initform (make-array 0 :adjustable t :fill-pointer 0) :accessor classes)
   (constants :accessor constants)
   (weights :accessor weights)
   (inverse-covariance-matrix :accessor inverse-covariance-matrix)))

(defun make-classifier (&key (feature-set 'rubine11))
  (make-instance 'classifier :feature-set feature-set))

(defun filter-example (example)
  (loop with prev-x = nil
     with prev-y = nil
     for (p x y time) in example
     when (or (not prev-x)
              (> (+ (expt (- x prev-x) 2)
                    (expt (- y prev-y) 2))
                 9))
     do (setf prev-x x prev-y y)
     and collect (list p x y time)))

(defun add-example (classifier class-name example-name example)
  (let* ((v (make-instance (feature-set classifier)))
         (class-index (gethash class-name (class-index classifier)))
         (class (if class-index (aref (classes classifier) class-index))))
    (unless class
      (setf class (make-instance 'classifier-class :name class-name))
      (setf (average class)
            (dvector* (feature-set-size v)))
      (setf (covariance-matrix class)
            (dmatrix* (feature-set-size v) (feature-set-size v)))
      (setf (gethash class-name (class-index classifier))
            (vector-push-extend class (classes classifier))))
    (reset-feature-set v)
    (loop for (p x y time) in (filter-example example)
       do (update-feature-set v x y time))
    (push (list example-name v example) (examples class))
    ;; update covariance matrix
    (loop
       with fv = (feature-vector v)
       with nfv = (sub-vector fv (average class))
       with n-1/n = (float (/ (1- (length (examples class)))
                              (length (examples class)))
                           1d0)
       for i below (length fv)
       do
       (loop for j from i below (length fv)
          do (incf (aref (covariance-matrix class) i j)
                   (* n-1/n (aref nfv i) (aref nfv j)))))
    ;; update mean vector
    (average-vector (average class)
                    (float (1- (length (examples class))) 1d0)
                    (feature-vector v)
                    1d0
                    (average class))))

(defun train-classifier (classifier training-data)
  (let ((nfeatures (feature-set-size (feature-set classifier))))
    (loop for (class example-name example) in training-data
       do (add-example classifier class example-name example))
    (setf (inverse-covariance-matrix classifier)
          (dmatrix* nfeatures nfeatures))
    (setf (weights classifier)
          (coerce (loop for i below (length (classes classifier))
                     collect (dvector* nfeatures))
                  'vector))
    (setf (constants classifier)
          (dvector* (length (classes classifier))))
    ;; calculate inverse of average of covariance matrices
    (loop for class across (classes classifier)
       sum (1- (length (examples class))) into count
       do (matrix-add (inverse-covariance-matrix classifier)
                      (covariance-matrix class)
                      (inverse-covariance-matrix classifier))
       finally
         (progn
           (loop for i below nfeatures
              do
                (loop for j from i below nfeatures
                   for v = (/ (aref (inverse-covariance-matrix classifier) i j )
                              (float count 1d0))
                   do
                     (setf (aref (inverse-covariance-matrix classifier) i j )
                           v
                           (aref (inverse-covariance-matrix classifier) j i )
                           v)))
           (multiple-value-bind (inv det)
               (invert-matrix (inverse-covariance-matrix classifier))
             (when (< (abs det) 0.0001)
               ;; todo: fix classifier
               (error "no inverse matrix ~s" det))
             (setf (inverse-covariance-matrix classifier) inv))))
    ;; calculate discriminating functions
    (loop for class across (classes classifier)
       for weights across (weights classifier)
       for i from 0
       do
         (vector*matrix (average class) (inverse-covariance-matrix classifier)
                        weights)
         (setf (aref (constants classifier) i)
               (* -0.5d0 (inner-product weights (average class))))))
  classifier)


(defun classify-path (classifier path)
  (let* ((v (make-instance (feature-set classifier)))
         (disc (dvector* (length (classes classifier))))
         best best-i)
    (reset-feature-set v)
    (loop for (p x y time) in (filter-example path)
       do (update-feature-set v x y time))
    (loop for class across (classes classifier)
       for i from 0
       for d = (+ (inner-product (aref (weights classifier) i)
                                 (feature-vector v))
                  (aref (constants classifier) i))
       do (setf (aref disc i) d)
       when (or (not best)
                (> d best))
       do (setf best d best-i i))
    ;; todo: ambiguity, distance calcs
    (values (aref (classes classifier) best-i) best nil nil)))


