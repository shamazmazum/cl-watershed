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
  (min (max val min) max))

(sera:-> add-indices (list list list) (values list &optional))
(declaim (inline add-indices))
(defun add-indices (x y dimensions)
  (mapcar
   (lambda (x y max)
     (declare (type fixnum x y max))
     (clamp (+ x y) 0 (1- max)))
   x y dimensions))

(deftype index () '(integer 0 #.array-dimension-limit))

(declaim (inline aref-index))
(defun aref-index (array index)
  (aref array
        (the index (first index))
        (the index (second index))))

(declaim (inline (setf aref-index)))
(defun (setf aref-index) (val array index)
  (setf (aref array
              (the index (first index))
              (the index (second index)))
        val))

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

(sera:-> make-default-mask ((simple-array * (* *)))
         (values (simple-array boolean (* *)) &optional))
(defun make-default-mask (array)
  (make-array (array-dimensions array)
              :element-type 'boolean
              :initial-element t))

;; I do not need damn-fast-priority-queue's functions to be inlined
(defun enqueue (q x p)
  (declare (type (unsigned-byte 32) p))
  (q:enqueue q x (- (ash 1 32) 1 p)))

(defun dequeue (q)
  (q:dequeue q))

(defun make-queue ()
  (q:make-queue))

(sera:-> queue-size (q:queue)
         (values alex:non-negative-fixnum &optional))
(defun queue-size (q)
  (q:size q))

(sera:-> watershed
         ((simple-array alex:non-negative-fixnum  (* *))
          (simple-array alex:non-negative-fixnum  (* *))
          &optional
          (simple-array boolean (* *)))
         (values (simple-array alex:non-negative-fixnum (* *)) &optional))
(defun watershed (image seeds &optional (mask (make-default-mask image)))
  "Perform watershed segmentation on IMAGE. IMAGE must be a 2D
grayscale image (2D array with element-type SINGLE-FLOAT). SEEDS is a
2D array with element-type FIXNUM which contains already labeled
regions of the image. The value 0 means no label. The result is a 2D
array of labels (element-type FIXNUM).

An optional argument MASK is a 2D array of BOOLEANs. Pixels with
indices where MASK is NIL are not labeled. By default, all elements in
MASK are T."
  (declare (optimize (speed 3)
                     #+sbcl (sb-c:insert-array-bounds-checks 0)))
  (check-dimensions image seeds mask)
  (let ((queue (make-queue))
        (gradient (gradient-norm image))
        (result (copy-array seeds))
        (dimensions (array-dimensions image)))
    (flet ((label-and-push (index)
             (declare (type list index))
             ;; Work only with not labeled, not masked pixels
             (when (and (aref-index mask index)
                        (zerop (aref-index result index)))
               (loop for shift in +neighbors+
                     for idx = (add-indices index shift dimensions)
                     for label-idx = (aref-index result idx)
                     do
                     ;; Push in the queue all not yet labeled
                     ;; neighbors, assign a label of some neighbor to
                     ;; the pixel at (x, y).
                     (if (zerop label-idx)
                         (enqueue queue idx (aref-index gradient idx))
                         (setf (aref-index result index) label-idx))))))

      ;; First step: push all neigbors of seed pixels into the queue
      (do-indices (image (y x))
        (when (not (zerop (aref seeds y x)))
          (let ((center (list y x)))
            (map nil (lambda (shift)
                       (label-and-push
                        (add-indices center shift dimensions)))
                 +neighbors+))))

      ;; Pop from and push to the queue until all segments are labeled
      (loop while (not (zerop (queue-size queue))) do
           (let ((idx (dequeue queue)))
             (label-and-push idx))))
    result))
