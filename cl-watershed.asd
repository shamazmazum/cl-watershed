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
               :queues.priority-queue))
