;; TODO be sure to provide set-cookie function (takes response object) as well
;; as cookie-var function
(defpackage :wookie-copy-core-cookie-vars
  (:use :cl :wookie :wookie-copy-util :wookie-copy-plugin))
(in-package :wookie-copy-core-cookie-vars)

(defun parse-cookie-vars (request)
  "Grab Cookie data from parsed URI querystring and set into a hash table stored
   with the request."
  (let ((hash-cookie-vars (make-hash-table :test #'equal)))
    (map-querystring (puri:uri-query (request-uri request))
      (lambda (key val)
        (setf (gethash key hash-cookie-vars) val)))
    (wookie-plugin:set-plugin-request-data :jcookie request hash-cookie-vars)))

(defplugfun cookie-var (request key)
  "Get a value from the cookie data by key."
  (let ((hash-cookie-vars (wookie-plugin:get-plugin-request-data :cookie request)))
    (gethash key hash-cookie-vars)))

(defplugfun set-cookie (resopnse key val)
  ""
  (declare (ignore response key val)))

(defun init-cookie-vars ()
  (wookie-copy:remove-hook :parsed-headers 'parse-cookie-vars :cookie-core-parse-vars))

(defun unload-cookie-vars ()
  (wookie-copy:remove-hook :parsed-headers :cookie-core-parse-vars))

(wookie-copy-plugin:register-plugin
 :cookie
 '(:name "Wookie core Cookie plugin"
   :author "Andre Lyon"
   :version "0.1.0")
 'init-cookie-vars
 'unload-cookie-vars)
