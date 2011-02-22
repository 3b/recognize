(in-package :recognize)

;;; N-dimensional matrix/vector libs, operate on vectors/2d arrays of
;;; double-flooats

(deftype dvector () '(simple-array double-float (*)))
(deftype dmatrix () '(simple-array double-float (* *)))

(declaim (inline dvector dvector*))

(defun dvector (&rest values)
  (map 'dvector
       (lambda (x) (float x 1d0))
       values))

(defun dvector* (length &key (init 0d0))
  (make-array (list length) :element-type 'double-float
              :initial-element (float init 0d0)))

(defun inner-product (v1 v2)
  (declare (type dvector v1 v2))
  (assert (= (length v1) (length v2)))
  (loop
     for a across v1
     for b across v2
     sum (* a b)))

(defun scale-vector (v s &optional dest)
  (if dest
      (progn (map-into dest (lambda (a) (* a s)) v) dest)
      (map 'dvector (lambda (a) (* a s)) v)))

(defun add-vector (v1 v2 &optional dest)
  (declare (dvector v1 v2))
  (if dest
      (progn (map-into dest #'+ v1 v2) dest)
      (map 'dvector #'+ v1 v2)))

(defun sub-vector (v1 v2 &optional dest)
  (declare (dvector v1 v2))
  (if dest
      (progn (map-into dest #'- v1 v2) dest)
      (map 'dvector #'- v1 v2)))

(defun average-vector (v1 w1 v2 w2 &optional dest)
  (flet ((avg1 (a b) (/ (+ (* a w1) (* b w2)) (+ w1 w2))))
    (declare (dvector v1 v2))
    (if dest
        (progn (map-into dest #'avg1 v1 v2) dest)
        (map 'dvector #'avg1 v1 v2))))

(defun slice-vector (v mask)
  (apply 'dvector (loop for i across mask
                     for ve across v
                     unless (zerop i)
                     collect ve)))




;;; matrix ops

(defun dmatrix (values)
  "make a 2d double-float array from values, which is either a
  sequence of (equal length) sequences or a 2d array"
  (cond
    ((and (arrayp values) (= 2 (array-rank values)))
     (let ((a (make-array (array-dimensions values) :element-type 'double-float)))
       (loop for i below (array-dimension values 0)
          do (loop for j below (array-dimension values 1)
                do (setf (aref a i j) (aref values i j))))
       a))
    (t (make-array (list (length values) (length (elt values 1)))
                   :element-type 'double-float
                   :initial-contents (map 'list
                                          (lambda (a)
                                            (map 'list
                                                 (lambda (x) (float x 1d0))
                                                 a))
                                          values)))))
;; | 1 2 |
;; | 3 4 | = (dmatrix '((1 2) (3 4) (5 6)))
;; | 5 6 |
(defun dmatrix* (rows cols &key (init 0d0))
  (make-array (list rows cols) :element-type 'double-float
              :initial-element (float init 1d0)))

(defun scale-matrix (m s &optional dest)
  (let ((dest (or dest (dmatrix* (array-dimension m 0)
                                 (array-dimension m 1)))))
    (loop for j below (array-dimension m 1)
       do (loop for i below (array-dimension m 0)
             do (setf (aref dest i j)
                      (* (aref m i j) s))))
    dest))

(defun slice-matrix (m row-mask col-mask)
  (let ((dest (dmatrix* (count 1 row-mask) (count 1 col-mask))))
    (loop
       for i below (array-dimension m 0)
       for di = 0 then (if (zerop rm) di (1+ di))
       for rm across row-mask
       unless (zerop rm)
       do (loop for j below (array-dimension m 1)
             for dj = 0 then (if (zerop cm) dj (1+ dj))
             for cm across col-mask
             unless (zerop cm)
             do (setf (aref dest di dj) (aref m i j))))
    dest))

#++
(slice-matrix (dmatrix '((1 2 3)
                         (4 5 6)
                         (7 8 9)))
              #*101 #*110)

(defun deslice-matrix (m fill row-mask col-mask &optional dest)
  (let ((dest (or dest (dmatrix* (length row-mask)
                                 (length col-mask) :init fill))))
    (loop for di below (length row-mask)
       for i = 0 then (if (zerop rm) i (1+ i))
       for rm across row-mask
       unless (zerop rm)
       do (loop for dj below (length col-mask)
             for j = 0 then (if (zerop cm) j (1+ j))
             for cm across col-mask
             unless (zerop cm)
             do (setf (aref dest di dj) (aref m i j))))
    dest))

#++
(deslice-matrix (slice-matrix (dmatrix '((1 2 3)
                                         (4 5 6)
                                         (7 8 9)))
                              #*101 #*110)
                9.5d7 #*1001 #*1001)


(defun matrix-multiply (m1 m2 &optional dest)
  (declare (type dmatrix m1 m2))
  (assert (= (array-dimension m1 1) (array-dimension m2 0)))
  (let ((dest (or dest (dmatrix* (array-dimension m1 0)
                                 (array-dimension m2 1)))))
    (declare (type dmatrix dest))
    (loop for i below (array-dimension m1 0)
       do (loop for j below (array-dimension m2 1)
             do (setf (aref dest i j)
                      (loop for k below (array-dimension m1 1)
                         sum (* (aref m1 i k) (aref m2 k j))))))
    dest))

(defun matrix-add (m1 m2 &optional dest)
  (declare (type dmatrix m1 m2))
  (assert (= (array-dimension m1 1) (array-dimension m2 0)))
  (let ((dest (or dest (dmatrix* (array-dimension m1 0)
                                 (array-dimension m2 1)))))
    (declare (type dmatrix dest))
    (loop for i below (array-dimension m1 0)
       do (loop for j below (array-dimension m2 1)
             do (setf (aref dest i j)
                      (+ (aref m1 i j) (aref m2 i j)))))
    dest))

#++(matrix-multiply (dmatrix '((14 2 0 5)
                            (9 11 12 2)
                            (3 15 17 3)))
                 (dmatrix '((12 9 8)
                            (25 10 5))))
#++
(matrix-multiply (dmatrix '((14 9 3)
                            (2 11 15)
                            (0 12 17)
                            (5 2 3)))
                 (dmatrix '((12 25)
                            (9 10)
                            (8 5))))
(aref                  (dmatrix '((12 25)
                            (9 10)
                            (8 5))) 2 0)

;;#2A((273.0d0 455.0d0)
;;    (243.0d0 235.0d0)
;;    (244.0d0 205.0d0)
;;    (102.0d0 160.0d0))


(defun vector*matrix (v m &optional dest)
  (declare (type dmatrix m) (type dvector v))
  (assert (= (length v) (array-dimension m 0)))
  (let ((dest (or dest (make-array (array-dimension m 1)
                                   :element-type 'double-float))))
    (declare (type dvector dest))
    (loop for j below (array-dimension m 1)
       do (setf (aref dest j)
                (loop for k below (length v)
                   sum (* (aref v k) (aref m k j)))))
    dest))


#++
(vector*matrix (dvector 1 2 3)
               (dmatrix '((12 25)
                          (9 10)
                          (8 5))))

(defun quadratic-form (v m)
  "compute v'mv"
  (declare (type dmatrix m) (type dvector v))
    (loop for j below (length v)
       sum (loop for i below (length v)
              sum (* (aref v j) (aref v i) (aref m i j)))))

#++
(quadratic-form (dvector 1 2 3)
                (dmatrix '((12 25 3)
                           (9 10 4)
                           (8 5 5))))

(defun invert-matrix (m &optional dest)
  (declare (optimize (debug 3)))
  (assert (= (array-dimension m 0) (array-dimension m 1)))
  (let* ((n (array-dimension m 0))
         (m (dmatrix m))
         (dest (or dest (dmatrix* n n)))
         (det 1d0))
    (declare (type dmatrix m dest))
    (loop for j below n
       do (loop for i below n
             when (= i j) do (setf (aref dest i j) 1d0)
             else do (setf (aref dest i j) 0d0)))
    (flet ((select-pivot (col)
             (loop with max = (aref m col col)
                with maxi = col
                for i from col below n
                for v = (aref m i col)
                when (> v max) do (setf maxi i max v)
                finally (return (list maxi max))))
           (swap-row (r1 r2)
             (loop for j below n
                do (rotatef (aref m r1 j) (aref m r2 j))
                  (rotatef (aref dest r1 j) (aref dest r2 j))))
           (scale-row (r1 s)
             (loop for j below n
                do (setf (aref m r1 j) (* (aref m r1 j) s)
                         (aref dest r1 j) (* (aref dest r1 j) s))))
           (add-scaled-row (rd r s)
             (loop for j below n
                do (incf (aref m rd j) (* (aref m r j) s))
                  (incf (aref dest rd j) (* (aref dest r j) s)))))
      (loop for j below n
         for (pivot-row pivot-value) = (select-pivot j)
         when (zerop pivot-value)
         do (return-from invert-matrix (values nil 0d0))
         unless (= pivot-row j)
         do (swap-row pivot-row j)
         do
           (setf det (* det pivot-value))
           (scale-row j (/ pivot-value))
           (loop for i below n
              for v = (aref m i j)
              unless (or (= i j) (zerop v))
              do (add-scaled-row i j (- v))))
      (values dest det))))

#++
(invert-matrix (dmatrix '((0 2 0)
                          (1 0 0)
                          (0 0 1))))

#++
(invert-matrix (dmatrix '((1 -1)
                          (-1 -1))))

#++
(matrix-multiply (dmatrix #1='((2 3 4)
                            (5 6 7)
                            (8 9 1)))
                 (invert-matrix (dmatrix #1#)))

(defun invert-singular-matrix (m)
  (let* ((n (array-dimension m 0))
         (mask (make-array n :element-type 'bit :initial-element 0)))
    (labels ((fill-mask (&rest off)
               (fill mask 1)
               (loop for i in off do (setf (aref mask i) 0))
               mask)
             (try (start masked)
               (loop
                  with max-det = 0d0
                  with max-i = -1
                  with best = nil
                  for i from start below n
                  for mask-i = (apply #'fill-mask i masked)
                  for tmp = (slice-matrix m mask-i mask-i)
                  for (inv det) = (multiple-value-list (invert-matrix tmp))
                  when (and inv (> (abs det) (abs max-det)))
                  do (setf max-det det
                           max-i i
                           best inv)
                  finally (return (values inv max-det (cons max-i masked)))))
             (check (inv det masked)
               (when (> (abs det) 1.0d-6)
                 (let ((m (apply #'fill-mask masked)))
                   (return-from invert-singular-matrix
                     (deslice-matrix inv 0 m m))))))
      ;; try removing 1 row/column
      (multiple-value-call #'check (try 0 nil))
      ;; 2 rows/columns
      (loop for i below n
         do (multiple-value-call #'check (try (1+ i) (list i))))
      ;; 3
      (loop for i below n
         do (loop for j from (1+ i) below n
               do (multiple-value-call #'check (try (1+ j) (list i j)))))
      ;; original gives up here, possibly should try a few more
      ;; depending on size of matrix?
      (values nil 0d0))))

#++
(invert-singular-matrix (dmatrix '((4 5 6) (7 8 9) (3 2 1))))
#++
(invert-singular-matrix (dmatrix '((4 5 6) (7 8 9) (1 2 3))))
#++
(invert-singular-matrix (dmatrix '((1 2 3) (4 5 6) (7 8 9))))

#++
(matrix-multiply #1=(dmatrix '((1 2 3) (4 5 6) (7 8 9)))
                 (invert-singular-matrix #1#))