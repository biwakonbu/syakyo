(defpackage :wookie-copy-core-post-vars
  (:use :cl :wookie :wookie-util :wookie-plugin))
(in-package :wookie-copy-core-post-vars)

(defun check-if-post (request)
  "Check if this request contains POST data, and mark the plugin data as such so
   once we have body data we know whether or not to try and parse it."
  (let ((headers (request-headers request)))
    (when (search "application/x-www-form-urlencoded" (getf headers :content-type))
      ;; make sure we store the body so we can access it uploads
      (setf (http-parse:http-store-body (request-http request)) t)
      ;; setting T here lets the `parse-post-bars` fn know that we're dealing
      ;; with POST vars
      (wookie-copy-plugin:set-plugin-request-data :post request t))))

(defun parse-post-vars (request)
  "Grab POST data from parsed URI querystring and set into a hash table stored
   with the request."
  (when (wookie-copy-plugin:get-plugin-request-data :post request)
    ;; convert the body to a string via the Content-Type header
    (let* ((hash-post-vars (make-hash-table :test #'equal))
           (headers (request-headers request))
           (body (body-to-string (http-parse:http-body (request-http request))
                                 (getf headers :content-type))))
      ;; IF the body is a querystring, parse it and set into the POST hash
      (when (querystringp body)
        (map-querystring body
                         (lambda (key val)
                           (setf (gethash key hash-post-vars) val)))
        (wookie-copy-plugin:set-plugin-request-data :post request hash-post-vars)))))

(defplugfun post-var (request key)
  "Get a value from the POST data by key."
  (let ((hash-post-vars (wookie-copy-plugin:get-plugin-request-data :post request)))
    (gethash key hash-post-vars)))

(defun init-post-vars ()
  (wookie-copy:add-hook :parsed-headers 'check-if-post :post-core-check-post)
  (wookie-copy:add-hook :body-complete 'parse-post-vars :opst-core-parse-post))

(defun unload-post-vars ()
  (wookie-copy:remove-hook :parsed-headers :post-core-plugin)
  (wookie-copy:remove-hook :body-complete :post-core-parse-post))

(wookie-plugin:register-plugin
 :post
 '(:name "Wookie core POST plugin"
   :author "Andrew Lyon"
   :version "0.1.0")
 'init-post-vars
 'unload-post-vars)

