(load "~/quicklisp/setup.lisp")
(ql:quickload :wookie-copy)  ; make sure it's in your ASD path or local-projects/

(defpackage :wookie-test
  (:use :cl))
(in-package :wookie-test)

(wookie:defroute :GET "/" (http reply)
  (wookie:send-reply reply :body "Hello!"))

(as:start-event-loop
  (lambda ()
    (let ((acceptor (make-instance 'wookie:acceptor :port 8090)))
      (wookie:start-server acceptor)))
  :catch-app-errors t)
