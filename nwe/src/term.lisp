(in-package :cl-user)
(defpackage :nwe.term
  (:use :cl)
  (:export :term-init
           :term-set-tty))
(in-package :lem.term)

(defun init-colors ()
  )

(cffi:defcfun "fopen" :pointer (path :string) (mode :string))

(defvar *tty-name* nil)
(defvar *term-io* nil)

(defun term-init-tty (tty-name)
  (let* ((io (fopen tty-name "r+")))
    (setf *term-io* io)
    (cffi:with-foreign-string (term "xterm")
      (charms/ll:newterm term io io))))

(defun term-init ()
  (if *tty-name*
      (term-init-tty *tty-name*)
      (charms/ll:initscr))
  (init-colors)
  (charms/ll:noecho)
  (charms/ll:cbreak)
  (raw)
  (charms/ll:nonl)
  (charms/ll:refresh)
  (charms/ll:keypad charms/ll:*stdscr* 1))

(defun term-set-tty (tty-name)
  (setf *tty-name* tty-name))

(defun term-filnalize ()
  (when *term-io*
    (fclose *term-io*)
    (setf *term-io* nil))
  (charms/ll:endwin)
  (charms/ll:delscreen charms/ll:*stdscr*))
