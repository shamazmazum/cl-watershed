(defpackage cl-watershed
  (:use #:cl)
  (:local-nicknames (#:alex #:alexandria)
                    (#:sera #:serapeum)
                    (#:q    #:damn-fast-priority-queue))
  (:export #:convolve ; Just in case
           #:watershed))
