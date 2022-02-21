(in-package :cl-watershed)

(define-condition dimensions-mismatch (error)
  ((image-dim :reader  error-image-dim
              :initarg :image-dim)
   (seeds-dim :reader  error-seeds-dim
              :initarg :seeds-dim)
   (mask-dim  :reader  error-mask-dim
              :initarg :mask-dim))
  (:report (lambda (c s)
             (format s "Dimensions mismatch: ~a image vs. ~a seeds vs. ~a mask"
                     (error-image-dim c)
                     (error-seeds-dim c)
                     (error-mask-dim  c)))))
