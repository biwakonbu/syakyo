(in-package :nwe)

(defparameter *minibuffer-window-height* 1)

(defvar *minibuf-window*)

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
