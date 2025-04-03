(in-package :cl-watershed)

(alex:define-constant +neighbors+
    '((-1  0)
      (+1  0)
      ( 0 -1)
      ( 0 +1))
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

;; Meyer's flooding
(sera:-> watershed
         ((simple-array alex:non-negative-fixnum  (* *))
          (simple-array alex:non-negative-fixnum  (* *))
          &optional
          (simple-array boolean (* *)))
         (values (simple-array fixnum (* *)) &optional))
(defun watershed (image seeds &optional (mask (make-default-mask image)))
  "Perform watershed segmentation on IMAGE. IMAGE must be a 2D
grayscale image (2D array with element type
ALEXANDRIA:NON-NEGATIVE-FIXNUM). SEEDS is a 2D array with the same
element type which contains already labeled regions of the image. The
value 0 means no label. The result is a 2D array of labels (element
type FIXNUM).

An optional argument MASK is a 2D array of BOOLEANs. Pixels with
indices where MASK is NIL are not labeled. By default, all elements in
MASK are T.

Watershed lines are labeled with -1."
  (declare (optimize (speed 3)
                     #+sbcl (sb-c:insert-array-bounds-checks 0)))
  (check-dimensions image seeds mask)
  (let ((queue (make-queue))
        (gradient (gradient-norm image))
        (result (aops:each-index* 'fixnum
                    (i j)
                  (aref seeds i j)))
        (dimensions (array-dimensions image)))
    (flet ((process (index)
             (declare (type list index))
             ;; Work only with not labeled, not masked pixels
             (when (and (aref-index mask index)
                        (zerop (aref-index result index)))
               ;; Collect labels of already labeled neighbors
               (let (first-label (same-neighbors-p t))
                 (declare (type (or null fixnum) first-label))
                 (loop for shift in +neighbors+
                       for idx = (add-indices index shift dimensions)
                       for label = (aref-index result idx)
                       for labeledp = (/= label 0 -1) do
                       (if labeledp
                           (setq first-label
                                 ;; Set the first seen neighbor label (if not already set)
                                 (or first-label label)
                                 same-neighbors-p
                                 ;; Check if all neighbors have the same label
                                 (if (and same-neighbors-p first-label)
                                     (= first-label label)
                                     same-neighbors-p))
                            ;; Add non-labeled neighbord to the queue
                           (enqueue queue idx (aref-index gradient idx)))
                       finally
                       ;; Set -1 for watershed line (if neighbor
                       ;; pixels have different labels)
                       (setf (aref-index result index)
                             (if (and same-neighbors-p first-label)
                                 first-label -1)))))))

      ;; First step: push all neigbors of seed pixels into the queue
      (do-indices (image (y x))
        (let ((idx (list y x)))
          (when (and (zerop (aref seeds y x))
                     (some (lambda (shift)
                             (not (zerop
                                   (aref-index seeds (add-indices idx shift dimensions)))))
                           +neighbors+))
            (enqueue queue idx (aref gradient y x)))))

      ;; Pop from and push to the queue until all segments are labeled
      (loop while (not (zerop (queue-size queue))) do
           (let ((idx (dequeue queue)))
             (process idx))))
    result))
