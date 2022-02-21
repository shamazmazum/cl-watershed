(defpackage cl-watershed
  (:use #:cl #:queues)
  (:local-nicknames (:alex :alexandria)
                    (:sera :serapeum))
  (:export #:convolve ; Just in case
           #:watershed))
