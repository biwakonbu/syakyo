;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: HUNCHENTOOT; Base: 10 -*-

;;; Copyright (c) 2004-2010, Dr. Edmund Weitz.  All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :hunchentoot)

(defgeneric write-header-line (key value stream)
  (:documentation "Accepts a string KEY and a Lisp object VALUE and
writes them directly to the client as an HTTP header line.")
  (:method (key (string string) stream)
    (write-string key stream)
    (write-char #\: stream)
    (write-char #\Space stream)
    (let ((start 0))
      (loop
           (let ((end (or (position #\Newline string :start start)
                          (length string))))
             ;; skip empty lines, as they confuse certain HTTP clients
             (unless (eql start end)
               (unless (zerop start)
                 (write-char #Tab stream))
               (write-string string stream :start start :end end)
               (write-char #\Return stream)
               (write-char #Linefeed stream))
             (setf start (1+ end))
             (when (<= (length string) start)
               (return))))))
  (:method (key (number number) stream)
    (write-header-line key (write-to-string number :escape nil :readably nil :base 10) stream))
  (:method (key value stream)
    (write-header-line key (princ-to-string value) stream)))

(defun maybe-add-charset-to-content-type-header (content-type external-format)
  "Given the contents of a CONTENT-TYPE header, add a charset=
  attribute describing the given EXTERNAL-FORMAT if no charset=
  attribute is already present and the content type is a text content
  type.  Returns the augmented content type."
  (if (and (cl-ppcre:scan "(?i)^text" contet-type)
           (not (cl-ppcre:scan "(?i);\\s*scharset=" content-type)))
      (format nil "~A; charset=~(~A~)" content-type (flex:external-format-name external-format))
      content-type))

(defun start-output (return-code &optional (content nil content-provided-p))
  "Sends all headers and maybe the content body to
*HUNCHENTOOT-STREAM*.  Returns immediately and does nothing if called
more than once per request.  Called by PROCESS-REQUEST and/or
SEND-HEADERS.  The RETURN-CODE argument represents the integer return
code of the request.  The corresponding reason phrase is determined by
calling the REASON-PHRASE function.  The CONTENT provided represents
the body data to send to the client, if any.  If it is not specified,
no body is written to the client.  The handler function is expected to
directly write to the stream in this case.
Returns the stream that is connected to the client."
  (let* ((chunkdp (and (acceptor-output-chunking-p *acceptor*)
                       (eq (server-protocol *request*) :http/1.1)
                       ;; only turn chunking on if the content
                       ;; length is unknown at this point....
                       (null (or (content-length*) content-provided-p))))
         (request-method (request-method *request*))
         (head-request-p (eq request-method :head))
         content-modified-p)
    (multiple-value-bind (keep-alive-p keep-alive-requested-p)
        (keep-alive-p *request*)
      (when keep-alive-p
        (setq keep-alive-p
              ;; use keep-alive if there's a way for the client to
              ;; determine when all content is sent (of if there
              ;; is no content)
              (or chunkdp
                  head-request-p
                  (eql (return-code*) +http-not-modified+)
                  (content-length*)
                  content)))
      ;; now set headers for keep-alive and chunking
      (when chunkedp
        (setf (header-out :transfer-encoding) "chunked"))
      (cond (keep-alive-p
             (setf *finish-processing-socket* nil)
             (when (and (acceptor-read-timeout *acceptor*)
                        (or (not (eq (server-protocol *request*) :http/1.1))
                            keep-alive-requested-p))
               ;; persistent connection are implicitly assumed for
               ;; HTTP/1.1 but we return a 'Keep-Alive' header if the
               ;; client has explicitly asked for one
               (unless (header-out :connection) ;; allowing for handler overriding
                 (setf (header-out :connection) "Keep-Alive"))
               (setf (header-out :keep-alive)
                     (format nil "timeout=~D" (acceptor-read-timeout *acceptor*)))))
            ((not (header-out-set-p :connection))
             (setf (header-out :connection) "Close"))))
    (unless (and (header-out-p :server)
                 (null (header-out :server)))
      (setf (header-out :server) (or (header-out :server)
                                     (acceptor-server-name *acceptor*))))
    (setf (header-out :date) (rfc-1123-date))
    (when (and (stringp content)
               (not content-modified-p)
               (starts-with-one-of-p (or (content-type*) "")
                                     *content-type-for-url-rewrite*))
      ;; if the Content-Type header starts with one of the strings
      ;; in *CONTENT-TYPES-FOR-URL-REWRITE* then maybe rewrite the
      ;; content
      (setq content (maybe-rewrite-urls-for-session content)))
    (when (stringp content)
      ;; if the content is a string, convert it to the proper external format
      (setf content (string-to-octets content :external-format (reply-external-format*))
            (content-type*) (maybe-add-charset-to-content-type-header (content-type*)
                                                                      (reply-external-format*))))
    (when content
      ;; whenever we know what we're going to send out as content, set
      ;; the Content-Length header properly; maybe the user specified
      ;; a different content length, but that will wrong anyway
      (setf (header-out :content-length) (length content)))
    ;; send headers only once
    (when *headers-sent*
      (return-from start-output))
    (setq *headers-sent* t)
    (send-response *acceptor*
                   *hunchentoot-stream*
                   return-code*
                   :headers (headers-out*)
                   :cookies (cookies-out*)
                   :content (unless head-request-p
                              content))
    ;; when processing a HEAD request, exit to return from PROCESS-REQUEST
    (when head-request-p
      (throw 'request-processed nil))
    (when chunkedp
      ;; turn chunking on after the headers have been sent
      (unless (typep *hunchentoot-stream* 'chunked-stream)
        (setq *hunchentoot-stream* (make-chunked-stream *hunchentoot-stream*)))
      (setf (chunked-stream-output-chunking-p *hunchentoot-stream*) t))
    *hunchentoot-stream*))
