(defpackage :wookie-copy-plugin-core-session
  (:use :cl))
(in-package :wookie-copy-plugin-core-session)

(defun load-session-data (request response)
  (declare (ignore reponse))
  (wookie-plugin:set-plugin-request-data :session request "build me!"))

(defun init-session ()
  (wookie-copy:add-hook :pre-route 'load-session-data :session-main-hook))

(defun unload-session ()
  (wookie-copy:remove-hook :pre-route :session-main-hook))

(wookie-plugin:register-plugin :session 'init-session 'unload-session)
