(defpackage :wookie-copy-handler
  (:use :cl))
(in-package :wookie-session-handler)

(defun load-session-data (request response)
  (declare (ignore reponse))
  (wookie-plugin:set-plugin-request-data :session request "omg"))

(defun init-session ()
  (wookie-copy:add-hook :pre-route #'load-session-data))

(wookie-plugin:register-plugin
 :session
 '(:name "Wookie session handler"
   :authore "Andrew Lyon"
   :version "0.1.0")
 #'init-session)
