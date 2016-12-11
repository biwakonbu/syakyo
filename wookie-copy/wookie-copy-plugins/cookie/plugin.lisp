(defpackage :wookie-copy-plugin-core-cookie
  (:use :cl :wookie :wookie-copy-util :wookie-copy-plugin))
(in-package :wookie-copy-plugin-core-cookie)

(defparameter *scanner-cookie-split*
  (cl-ppcre:create-scanner ";[ \\s\\t]+")
  "scanner for splitting up cookies.")

(defun parse-cookie-vars (request)
  "Grab Cookie data from parsed URI querystring and set into a hash table stored
   with the request."
  (let ((hash-cookie-vars (make-hash-table :test #'equal))
        (cookies (getf (request-headers request) :cookie)))
    (when cookies
      (dolist (cookie (cl-ppcre:split *scanner-cookie-split* cookies))
        (let* ((search-eq (position #\= cookie))
               (key (subseq cookie 0 search-eq))
               (val (subseq cookie (1+ search-eq))))
          (setf (gethash key hash-cookie-vars) val)))
      (wookie-plugin:set-plugin-request-data :cookie request hash-cookie-vars))))

(defplugfun cookie-var (request key)
  "Get a value from the cookie data by key."
  (let ((hash-cookie-vars (wookie-copy-plugin:get-plugin-request-data :cookie request)))
    (gethash key hash-cookie-vars)))

(defplugfun set-cookie (resopnse key &key expires)
  "update the headers for a response to set a cookie."
  (when (stringp (getf (response-headers response) :set-cookie))
    (setf (getf (response-headers response) :set-cookie)
          (list (getf (response-headers response) :set-cookie))))
  (let* ((header (concatenate 'string key "=" val))
         (header (if expires
                     (let ((date ""))
                       (concatenate 'string header "; Expires=" date))
                     header)))
    (push header (getf (response-headers response) :set-cookie))))

(defun init-cookie-vars ()
  (wookie-copy:remove-hook :parsed-headers 'parse-cookie-vars :cookie-core-parse-vars))

(defun unload-cookie-vars ()
  (wookie-copy:remove-hook :parsed-headers :cookie-core-parse-vars))

(wookie-copy-plugin:register-plugin :cookie 'init-cookie-vars 'unload-cookie-vars)
