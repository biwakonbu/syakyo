#|
  This file is a part of restas-copy project.
|#

(in-package :cl-user)
(defpackage restas-copy-asd
  (:use :cl :asdf))
(in-package :restas-copy-asd)

(defsystem restas-copy
  :description "RESTAS is a Common Lisp web application framework, based
on the Hunchentoot HTTP server. It was developed to simplify development of
web applications following the REST architectural style."
  :version "0.1"
  :author ""
  :license ""
  :depends-on (#:cffi #:hunchentoot #:bordeaux-threads
               #:routes #:alexandria #:data-sift)
  :path-name "src"
  :components ((:file "restas-copy")
               (:file "special" :depends-on ("packages"))
               (:file "declarations" :depends-on ("package"))
               (:file "errors" :depends-on ("special"))
               (:file "render" :depends-on ("special"))
               (:file "context" :depends-on ("special"))
               (:file "module" :depends-on ("context" "declarations"))
               (:file "route" :depends-on ("module" "render"))
               (:file "decorators" :depends-on ("route"))
               (:file "vhost" :depends-on ("special"))
               (:file "hunchentoot" :depends-on ("vhost" "module" "errors"))
               (:file "policy" :depends-on ("packages"))))
