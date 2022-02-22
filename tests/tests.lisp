(in-package :cl-watershed-tests)

(defparameter *image-path*
  (asdf:system-relative-pathname
   :cl-watershed/tests "docs/disks.pbm"))

(defun binary-segmentation (image seeds-list)
  (do-image-pixels (image color x y)
    (setf color (- 1 color)))

  (let* ((edt (imago:distance-transform image :type :edt))
         (pixels (image-pixels image))
         (seeds (aops:zeros* 'fixnum (array-dimensions pixels))))

    (loop
       for label from 1 by 1
       for seed in seeds-list do
         (setf (apply #'aref seeds seed) label))

    (cl-watershed:watershed
     edt seeds
     (aops:vectorize* 'boolean (pixels) (zerop pixels)))))

(defun run-tests ()
  (explain! (run 'watershed)))

(def-suite watershed :description "Test watershed")
(in-suite watershed)

(test binary-watershed
  (let* ((image (read-image *image-path*))
         (segments (binary-segmentation
                    image '((156 164)
                            (219 201)
                            (237 292)
                            (172 365))))
         (amount (loop for segment from 1 to 4 collect
                      (count segment (aops:flatten segments))))
         (total (reduce #'+ amount)))
    (is-true
     (every
      (lambda (amount)
        (< 0.2 (/ amount total) 0.3))
      amount))))
