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
