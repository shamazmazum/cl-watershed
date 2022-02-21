(in-package :cl-watershed)

(declaim (type (simple-array single-float)
               +sobel-blur+ +sobel-diff+))

(alex:define-constant +sobel-blur+
    (make-array 3 :element-type 'single-float :initial-contents '(1.0 2.0 1.0))
  :test #'equalp)

(alex:define-constant +sobel-diff+
    (make-array 3 :element-type 'single-float :initial-contents '(1.0 0.0 -1.0))
  :test #'equalp)

(sera:-> column*row
         ((simple-array single-float (*))
          (simple-array single-float (*)))
         (values (simple-array single-float (* *)) &optional))
(defun column*row (column row)
  "Multiply column by row using matrix multiplication"
  (declare (type (simple-array single-float) column row)
           (optimize (speed 3)))
  (aops:each-index* 'single-float (i j)
    (*
     (aref column i)
     (aref row j))))

(sera:-> convolve
         ((simple-array single-float (* *))
          (simple-array single-float (* *))
          &optional function)
         (values (simple-array single-float (* *)) &optional))
(defun convolve (array kernel &optional (function #'+))
  "Perform generic convolution of a 2D array ARRAY with kernel
KERNEL. Summation in the convolution formula is replaced with
reduction using function FUNCTION."
  (declare (type (simple-array single-float) array kernel))
  (let* ((array-dimensions  (array-dimensions array))
         (kernel-dimensions (array-dimensions kernel))
         (result (make-array array-dimensions
                             :element-type 'single-float
                             :initial-element 0.0)))
    (array-operations/utilities:nested-loop (i j)
        array-dimensions
      (setf (aref result i j)
            (aops:reduce-index function (ik jk)
              (let ((is (- (+ i ik) (floor (first  kernel-dimensions) 2)))
                    (js (- (+ j jk) (floor (second kernel-dimensions) 2))))
                (if (and (<= 0 is (1- (first  array-dimensions)))
                         (<= 0 js (1- (second array-dimensions))))
                    (* (aref kernel ik jk)
                       (aref array is js))
                    0.0)))))
    result))

;; Y/X
(sera:-> gradient
         ((simple-array single-float (* *)))
         (values (simple-array single-float (* *))
                 (simple-array single-float (* *))
                 &optional))
(defun gradient (array)
  (declare (type (simple-array single-float) array)
           (optimize (speed 3)))
  (values
   (convolve array (column*row +sobel-blur+ +sobel-diff+))
   (convolve array (column*row +sobel-diff+ +sobel-blur+))))

(sera:-> gradient-norm
         ((simple-array single-float (* *)))
         (values (simple-array single-float (* *)) &optional))
(defun gradient-norm (array)
  (declare (type (simple-array single-float (* *)) array)
           (optimize (speed 3)))
  (multiple-value-bind (y x)
      (gradient array)
    (aops:vectorize* 'single-float (x y)
      (sqrt (+ (expt x 2)
               (expt y 2))))))
