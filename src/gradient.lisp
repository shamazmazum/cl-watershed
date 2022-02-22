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
  (declare (type (simple-array single-float) array kernel)
           (optimize (speed 3)))
  (let* ((array-dimensions  (array-dimensions array))
         (kernel-dimensions (array-dimensions kernel))

         (array-height (first  array-dimensions))
         (array-width  (second array-dimensions))
         (kernel-height/2 (ash (first  kernel-dimensions) -1))
         (kernel-width/2  (ash (second kernel-dimensions) -1))

         (result (make-array array-dimensions
                             :element-type 'single-float
                             :initial-element 0.0)))
    (declare (type alex:positive-fixnum
                   array-height array-width
                   kernel-width/2 kernel-height/2))
    (dotimes (i array-height)
      (declare (type alex:non-negative-fixnum i))
      (dotimes (j array-width)
        (declare (type alex:non-negative-fixnum i))
        (setf (aref result i j)
              (aops:reduce-index function (ik jk)
                (let ((is (- (+ i (the fixnum ik)) kernel-height/2))
                      (js (- (+ j (the fixnum jk)) kernel-width/2)))
                  (if (and (<= 0 is (1- array-height))
                           (<= 0 js (1- array-width)))
                      (* (aref kernel ik jk)
                         (aref array is js))
                      0.0))))))
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
