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

(sera:-> convolve
         ((simple-array alex:non-negative-fixnum (* *))
          (simple-array fixnum                   (* *)))
         (values (simple-array fixnum (* *)) &optional))
(defun convolve (array kernel)
  "Perform generic convolution of a 2D array ARRAY with kernel
KERNEL. Summation in the convolution formula is replaced with
reduction using function FUNCTION."
  (declare (optimize (speed 3)
                     #+sbcl
                     (sb-c:insert-array-bounds-checks 0)))
  (let* ((array-height (array-dimension array 0))
         (array-width  (array-dimension array 1))
         (kernel-height/2 (ash (array-dimension kernel 0) -1))
         (kernel-width/2  (ash (array-dimension kernel 1) -1)))
    (do-indices/similar (fixnum array (i j))
      (aops:sum-index (ik jk)
        (let ((is (- (+ i ik) kernel-height/2))
              (js (- (+ j jk) kernel-width/2)))
          (if (and (<= 0 is (1- array-height))
                   (<= 0 js (1- array-width)))
              (the fixnum
                   (* (aref kernel ik jk)
                      (aref array is js)))
              0))))))

;; Y/X
(sera:-> gradient
         ((simple-array alex:non-negative-fixnum (* *)))
         (values (simple-array fixnum (* *))
                 (simple-array fixnum (* *))
                 &optional))
(defun gradient (array)
  (declare (optimize (speed 3)))
  (values
   (convolve array (column*row +sobel-blur+ +sobel-diff+))
   (convolve array (column*row +sobel-diff+ +sobel-blur+))))

(sera:-> gradient-norm
         ((simple-array alex:non-negative-fixnum (* *)))
         (values (simple-array alex:non-negative-fixnum (* *)) &optional))
(defun gradient-norm (array)
  "Return squared gradient norm"
  (declare (optimize (speed 3)))
  (multiple-value-bind (y x)
      (gradient array)
    (aops:vectorize* 'alex:non-negative-fixnum (x y)
      (+ (expt x 2)
         (expt y 2)))))
