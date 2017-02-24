(in-package :nwe)

(export '(minibuffer-window-p
          minibuffer-window-height
          message
          minibuf-y-or-n-p
          minibuf-read-char
          *minibuf-read-keymap*
          active-minibuffer-window
          check-switch-minibuffer-window
          minibuf-read-line-confirm
          minibuf-read-line-completion
          minibuf-read-line-prev-history
          minibuf-read-line-next-history
          minibuf-read-line
          minibuf-read-string
          minibuf-read-number
          minibuf-read-buffer
          minibuf-read-file))

(defparameter *minibuffer-window-height* 1)

(defvar *minibuf-window*)
(defvar *minibuffer-calls-window*)
(defvar *minibuffer-start-point*)
(defvar minibuffer-prompt-attribute* (make-attribute "blue" nil :bold-p t))

(defun minibuffer-window () *minibuf-window*)
(defun minibuffer-window-p (window) (eq window (minibuffer-window)))
(defun minibuffer-window-active-p () (eq (current-window) (minibuffer-window)))
(defun minibuffer-window-height () *minibuffer-window-height*)
(defun minibuffer () (window-buffer (minibuffer-window)))
(defun minibuffer-calls-window () *minibuffer-calls-window*)

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
