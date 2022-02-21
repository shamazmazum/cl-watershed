(defun do-all()
  (ql:quickload :cl-watershed/tests :verbose t)
  (uiop:quit
   (if (uiop:call-function "cl-watershed-tests:run-tests")
       0 1)))

(do-all)
