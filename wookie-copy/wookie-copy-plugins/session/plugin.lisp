(defpackage :wookie-copy-core-session-handler
  (:use :cl))
(in-package :wookie-copy-core-session-handler)

(defun load-session-data (request response)
  (declare (ignore reponse))
  (wookie-plugin:set-plugin-request-data :session request "omg"))

(defun init-session ()
  (wookie-copy:add-hook :pre-route 'load-session-data :session-main-hook))

(defun unload-session ()
  (wookie-copy:remove-hook :pre-route :session-main-hook))

(wookie-plugin:register-plugin
 :session
 '(:name "Wookie session handler"
   :authore "Andrew Lyon"
   :version "0.1.0"
   :depends-on (:cookie))
 'init-session
 'unload-session)
