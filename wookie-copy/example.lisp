(load "~/quicklisp/setup.lisp")
(ql:quickload :wookie-copy)  ; make sure it's in your ASD path or local-projects/

(defpackage :wookie-test
  (:use :cl))
(in-package :wookie-test)

(wookie-copy:clear-routes)

(wookie-copy:defroute (:get "/") (req res)
  (wookie-copy:send-response res :body "Hello!"))

(wookie-copy:defroute (:put "/albums/([0-9]+)") (req res args)
  (wookie-copy:send-response res :body (format nil "Album ~a updated!" (car args))))

(as:start-event-loop
  (lambda ()
    (let ((acceptor (make-instance 'wookie-copy:acceptor :port 8090)))
      (wookie-copy:start-server acceptor)))
  :catch-app-errors t)
