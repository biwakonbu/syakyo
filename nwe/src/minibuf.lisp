(in-package :nwe)

(defparameter *minibuffer-window-height* 1)

(defvar *minibuf-window*)
(defvar *minibuffer-calls-window*)
(defvar *minibuffer-start-point*)
(defvar minibuffer-prompt-attribute* (make-attribute "blue" nil :bold-p t))

(defun minibuffer-window-height () *minibuffer-window-height*)

(defun minibuf-init ()
  (let* ((buffer (make-buffer " *minibuffer*"))
         (window (make-window buffer
                              0
                              (- (display-height)
                                 (minibuffer-window-height))
                              (display-width)
                              (minibuffer-window-height)
                              nil)))
    (setq *minibuf-window* window)))
