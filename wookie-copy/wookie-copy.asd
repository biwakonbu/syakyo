(asdf:defsystem wookie-copy
  :author "Andrew Danger Lyon <orthecreedence@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :description "An evented webserver for Common Lisp."
  :depends-on (#:alexandria
               #:cl-async
               #+(or :wookie-no-ssl) #:cl-async-ssl
               #:cl-ppcre
               #:babel
               #:chunga
               #:http-parse
               #:cl-fad)
  :components
  ((:file "package")
   (:file "config" :depends-on ("package"))
   (:file "util" :depends-on ("config"))
   (:file "route" :depends-on ("util"))
   (:file "plugin" :depends-on ("util"))
   (:file "hook" :depends-on ("util"))
   (:file "request-response" :depends-on ("util"))
   (:file "acceptor" :depends-on ("request-response" "route" "plugin"))))
