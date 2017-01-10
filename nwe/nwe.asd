(cl:in-package :cl-user)

#-asdf(require :asdf)

#-uiop(require :uiop)

(defpackage :nwe-asd
  (:use :cl :asdf))

(in-package :nwe-asd)

(pushnew :nwe-use-inquisitor *features*)

(defsystem nwe
  :version "0.1"
  :depends-on (;; asfd/uiop the Utilities for Implementation- and OS- Portability.
               ;; https://github.com/fare/asdf/tree/master/uiop
               :uiop
               ;; loop macro like iterator.
               ;; https://common-lisp.net/project/iterate/
               :iterate
               ;; libcurses interface.
               ;; https://github.com/HiTECNOLOGYs/cl-charms
               :cl-charms
               ;; portable path name library.
               ;; http://weitz.de/cl-fad/
               :cl-fad
               ;; use to sbcl library.
               #+sbcl :sb-posix
               #+sbcl :sb-introspect
               ;; swank is rpc server
               ;; https://github.com/slime/slime/tree/master/swank
               :swank
               ;; portabillity library for CL gray streams.
               ;; https://github.com/trivial-gray-streams/trivial-gray-streams
               :trival-gray-streams
               ;; portable perl conpatible regular expressions for common lisp.
               ;; http://weitz.de/cl-ppcre/
               :cl-ppcre
               #+nwe-use-inquisitor :inquisitor
               ;; babel is a charset encoding/decoding library.
               ;; https://common-lisp.net/project/babel/
               :babel)
  :serial t
  :components ((:module "src"
                :serial t)))

