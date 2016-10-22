#|
  This file is a part of restas-copy project.
|#

(in-package :cl-user)
(defpackage restas-copy-test-asd
  (:use :cl :asdf))
(in-package :restas-copy-test-asd)

(defsystem restas-copy-test
  :author ""
  :license ""
  :depends-on (:restas-copy
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "restas-copy"))))
  :description "Test system for restas-copy"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
