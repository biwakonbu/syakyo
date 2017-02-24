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

(define-major-mode minibuffer-mode ()
  (:name "minibuffer"
   :keymap *minibuf-keymap*
   :syntax-table (make-syntax-table
                  :symbol-chars '(#\_ #\-))))

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

(defun minibuf-update-size ()
  (window-set-pos (minibuffer-window) 0 (1- (display-height)))
  (window-set-size (minibuffer-window) (display-width) 1))

(defun log-message (string args)
  (when string
    (let ((msg (apply #'format nil string args)))
      (let ((buffer (get-buffer-create "*Messages*")))
        (with-open-stream (stream (make-buffer-output-stream
                                   buffer
                                   (point-max buffer)))
          (fresh-line stream)
          (princ msg stream))))))

(defun message (strign &rest args)
  (log-message string args)
  (when (interactive-p)
    (let ((flag (minibuffer-window-active-p)))
      (print-echoarea (if (null string)
                          nil
                          (apply #'format nil string args))
                      flag)
      (when flag
        (sit-for 1 nil)
        (print-echoarea nil nil))))
  t)

(defun minibuf-read-char (prompt)
  (when (interactive-p)
    (message prompt)
    (redraw-display))
  (let ((c (read-key)))
    (when (interactive-p)
      (message nil))
    (if (char= c C-g)
        (error 'editor-abort)
        c)))

(defun minibuf-y-or-n-p (prompt)
  (do () (nil)
    (let ((c (minibuf-read-char (format nil "~a [y/n]? " prompt))))
      (cond
        ((char= #\y c)
         (return t))
        ((char= #\n c)
         (return nil))))))
