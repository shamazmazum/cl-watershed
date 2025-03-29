(in-package :cl-watershed)

(declaim (type (simple-array fixnum (*))
               +sobel-blur+ +sobel-diff+))

(alex:define-constant +sobel-blur+
    (make-array 3 :element-type 'fixnum :initial-contents '(1 2 1))
  :test #'equalp)

(alex:define-constant +sobel-diff+
    (make-array 3 :element-type 'fixnum :initial-contents '(1 0 -1))
  :test #'equalp)

(sera:-> column*row
         ((simple-array fixnum (*))
          (simple-array fixnum (*)))
         (values (simple-array fixnum (* *)) &optional))
(defun column*row (column row)
  "Multiply column by row using matrix multiplication"
  (declare (optimize (speed 3)
                     #+sbcl
                     (sb-c:insert-array-bounds-checks 0)))
  (aops:each-index* 'fixnum (i j)
    (*
     (aref column i)
     (aref row j))))

(sera:-> gradient-norm
         ((simple-array alex:non-negative-fixnum (* *)))
         (values (simple-array alex:non-negative-fixnum (* *)) &optional))
(defun gradient-norm (array)
  (declare (optimize (speed 3)
                     #+sbcl
                     (sb-c:insert-array-bounds-checks 0)))
  (let ((array-height (array-dimension array 0))
        (array-width  (array-dimension array 1))
        (result (make-array (array-dimensions array)
                            :element-type 'alex:non-negative-fixnum
                            :initial-element 0))
        (kernels (list (column*row +sobel-blur+ +sobel-diff+)
                       (column*row +sobel-diff+ +sobel-blur+))))
    (declare (dynamic-extent kernels))
    (dolist (kernel kernels)
      (declare (type (simple-array fixnum (* *)) kernel))
      (let ((kernel-height/2 (ash (array-dimension kernel 0) -1))
            (kernel-width/2  (ash (array-dimension kernel 1) -1)))
      (do-indices (array (i j))
        (let ((x (aops:sum-index (ik jk)
                   (let ((is (- (+ i ik) kernel-height/2))
                         (js (- (+ j jk) kernel-width/2)))
                     (if (and (<= 0 is (1- array-height))
                              (<= 0 js (1- array-width)))
                         (the fixnum
                              (* (aref kernel ik jk)
                                 (aref array is js)))
                         0)))))
          (incf (aref result i j) (expt x 2))))))
    result))
