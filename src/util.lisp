(in-package :cl-watershed)

(defmacro do-indices ((array indices) &body body)
  (let ((%array (gensym)))
    `(let ((,%array ,array))
       ,(first
         (reduce
          (lambda (entry acc)
            (let ((axis  (car entry))
                  (index (cdr entry)))
              `((loop for ,index fixnum below (array-dimension ,%array ,axis) do ,@acc))))
          (loop for index in indices for n from 0 by 1 collect (cons n index))
          :initial-value body
          :from-end t)))))

(defmacro do-indices/similar ((type array indices) form)
  (let ((result (gensym)))
    `(let ((,result (make-array (array-dimensions ,array)
                                :element-type ',type)))
       (do-indices (,array ,indices)
         (setf (aref ,result ,@indices)
               ,form))
       ,result)))
