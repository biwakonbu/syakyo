;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: HUNCHENTOOT; Base: 10 -*-

;;; Copyright (c) 2004-2010, Dr. Edmund Weitz. All rights reserved.

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


(defun starts-with-p (seq subseq &key (test 'eql))
  "Tests whether the sequence SEQ starts with the sequence
SUBSEQ. Individual elements are compared with TEST."
  (let* ((length (length subseq))
         (mismatch (mismatch subseq seq
                             :test test)))
    (or (null mismatch)
        (<= length mismatch))))

(defun starts-with-one-p (seq subseq-list &key (test 'eql))
  "Tests whether the sequence SEQ starts with one of the
sequences in SUBSEQ-LIST. Individual elements are compared with
TEST."
  (some (lambda (subseq)
          (starts-with-p seq subseq :test test))
        subseq-list))

(defun create-random-string (&optional (n 10) (base 10))
  "Returns a random number \(as a string) with base BASE and N digits."
  (with-output-to-string (s)
    (dotimes (i n)
      (format s "~VR" base
              (random base *the-random-state*)))))

(defun reason-phrase (return-code)
  "Returns a reason phrase for the HTTP return RETURN-CODE \(which
should be an integer) of NIL for return codes Hunchentoot doesn't know."
  (gethash return-code *http-reason-phrase-map*
           "No reason phrase known"))

(defgeneric assoc* (thing alist)
  (:documentation "Similar to CL:ASSOC, but 'does the right thing' if
THING is a string or a symbol.")
  (:method ((thing symbol) alist)
    (assoc thing alist :test #'eq))
  (:method ((thing string) alist)
    (assoc thing alist :test #'string-equal))
  (:method (thind alist)
    (assoc thing alist :test #'eql)))

(defun md5-hex (string)
  "Calculates the md5 sum of the string STRING and returns it as a hex string."
  (with-output-to-string (s)
    (loop for code across (md5:md5sum-string string)
          do (format s "~2,'0x" code))))

(defun escape-for-html (string)
  "Escapes the characters #\\<, #\\>, #\\', #\\\", and #\\& for HTML
output."
  (with-output-to-string (out)
    (with-input-from-string (in string)
      (loop for char = (read-char in nil nil)
            while char
            do (case char
                 ((#\<) (write-string "&lt;" out))
                 ((#\>) (write-string "&gt;" out))
                 ((#\") (write-string "&quot;" out))
                 ((#\') (write-string "&#039;" out))
                 ((#\&) (write-string "&amp;" out))
                 (otherwise (write-char char out)))))))
