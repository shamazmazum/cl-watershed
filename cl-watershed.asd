(defsystem :cl-watershed
  :name :cl-watershed
  :version "0.1"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :description "Priority-flood watershed segmentation algorithm"
  :license "2-clause BSD"
  :serial t
  :pathname "src/"
  :components ((:file "package")
               (:file "conditions")
               (:file "gradient")
               (:file "watershed"))
  :depends-on (:alexandria
               :serapeum
               :array-operations
               :queues
               :queues.priority-queue)
  :in-order-to ((test-op (load-op "cl-watershed/tests")))
  :perform (test-op (op system)
                    (declare (ignore op system))
                    (uiop:symbol-call :cl-watershed-tests '#:run-tests)))

(defsystem :cl-watershed/tests
  :name :cl-watershed/tests
  :license "2-clause BSD"
  :pathname "tests/"
  :components ((:file "package")
               (:file "tests" :depends-on ("package")))
  :depends-on (:cl-watershed :imago :fiveam))
