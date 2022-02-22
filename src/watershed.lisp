(in-package :cl-watershed)

(alex:define-constant +neighbors+
  '((-1  0)
    ( 0 -1)
    ( 0  1)
    ( 1  0))
  :test #'equalp)

(sera:-> clamp (fixnum fixnum fixnum) (values fixnum &optional))
(declaim (inline clamp))
(defun clamp (val min max)
  (declare (type fixnum val min max))
  (min (max val min) max))

(sera:-> add-indices (list list list) (values list &optional))
(declaim (inline add-indices))
(defun add-indices (x y dimensions)
  (declare (optimize (speed 3)))
  (mapcar
   (lambda (x y max)
     (declare (type fixnum x y max))
     (clamp (+ x y) 0 (1- max)))
   x y dimensions))

(declaim (inline copy-array))
(defun copy-array (array)
  (aops:each-index*
      (array-element-type array)
      (i j) (aref array i j)))

(defun check-dimensions (image seeds mask)
  (let ((image-dim (array-dimensions image))
        (seeds-dim (array-dimensions seeds))
        (mask-dim  (array-dimensions mask)))
    (when (not (every #'= image-dim seeds-dim mask-dim))
      (error 'dimensions-mismatch
             :image-dim image-dim
             :seeds-dim seeds-dim
             :mask-dim  mask-dim))))

(defun make-default-mask (array)
  (make-array (array-dimensions array)
              :element-type 'boolean
              :initial-element t))

(defun make-priority-queue ()
  (declare (optimize (speed 3)))
  (make-queue :priority-queue
              :compare (lambda (x y)
                         (let ((priority-x (car x))
                               (priority-y (car y)))
                           (declare (type single-float priority-x priority-y))
                           (> priority-x priority-y)))))

(sera:-> watershed
         ((simple-array single-float (* *))
          (simple-array fixnum       (* *))
          &optional
          (simple-array boolean      (* *)))
         (values (simple-array fixnum (* *)) &optional))
(defun watershed (image seeds &optional (mask (make-default-mask image)))
  "Perform watershed segmentation on IMAGE. IMAGE must be a 2D
grayscale image (2D array with element-type SINGLE-FLOAT). SEEDS is a
2D array with element-type FIXNUM which contains already labeled
regions of the image. The value 0 means no label. The result is a 2D
array of labels (element-type FIXNUM).

An optional argument MASK is a 2D array of BOOLEANs. Pixels with
indices where MASK is NIL are not labeled. By default, all elements in
MASK are T."
  (declare (type (simple-array single-float (* *)) image)
           (type (simple-array fixnum       (* *)) seeds)
           (type (simple-array boolean      (* *)) mask)
           (optimize (speed 3)))
  (check-dimensions image seeds mask)
  (let ((queue (make-priority-queue))
        (gradient (gradient-norm image))
        (result (copy-array seeds))
        (dimensions (array-dimensions image)))
    (flet ((label-and-push (y x)
             ;; Work only with not labeled, not masked pixels
             (when (and (aref mask y x)
                        (zerop (aref result y x)))
               (loop
                  with center = (list y x)
                  for shift in +neighbors+
                  for idx = (add-indices center shift dimensions)
                  for label-idx = (apply #'aref result idx)
                  do
                    ;; Push in the queue all not yet labeled
                    ;; neighbors, assign a label of some neighbor to
                    ;; the pixel at (x, y).
                    (if (zerop label-idx)
                        (qpush queue (cons (apply #'aref gradient idx) idx))
                        (setf (aref result y x) label-idx))))))

      ;; First step: push all neigbors of seed pixels into the queue
      (array-operations/utilities:nested-loop (y x)
          dimensions
        (when (not (zerop (aref seeds y x)))
          (let ((center (list y x)))
            (map nil (lambda (shift)
                       (apply #'label-and-push
                              (add-indices center shift dimensions)))
                 +neighbors+))))

      ;; Pop from and push to the queue until all segments are labeled
      (loop while (nth-value 1 (qtop queue)) do
           (let ((idx (qpop queue)))
             (apply #'label-and-push (cdr idx)))))
    result))
