(asdf:defsystem wookie
  :author "Andrew Danger Lyon <orthecreedence@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :description "An evented webserver for Common Lisp."
  :depends-on (#:cl-async
               #+(or :wookie-no-ssl) #:cl-async-ssl
               #:babel
               #:http-parse)
  :components
  ((:file "package" :depends-on ("util"))
   (:file "util" :depends-on ("config"))
   (:file "route" :depends-on ("error"))
   (:file "plugin" :depends-on ("package" "state"))
   (:file "hook" :depends-on ("package" "state"))
   (:file "acceptor" :depends-on ("route" "hook" "plugin"))))
