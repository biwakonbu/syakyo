(asdf:defsystem wookie-copy
  :author "Andrew Danger Lyon <orthecreedence@gmail.com>"
  :license "MIT"
  :version "0.2.2"
  :description "An evented webserver for Common Lisp."
  :depends-on (#:alexandria
               #:cl-async
               #-(or :wookie-no-ssl) #:cl-async-ssl
               #:cl-ppcre
               #:babel
               #:chunga
               #:http-parse
               #:puri
               #:do-urlencode
               #:local-time
               #:cl-fad)
  :components
  ((:file "config")
   (:file "util" :depends-on ("config"))
   (:file "package" :depends-on ("util"))
   (:file "error-handler" :depends-on ("package"))
   (:file "route" :depends-on ("config"))
   (:file "plugin" :depends-on ("package"))
   (:file "hook" :depends-on ("package"))
   (:file "request-response" :depends-on ("config"))
   (:file "listener" :depends-on ("request-response" "route" "plugin"))
   #-(or :wookie-no-ssl)
   (:file "listener-ssl" depends-on ("listener"))))
