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
(defun minibufferp (buffer) (eq buffer (minibuffer)))
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

(define-key *minibuf-keymap* (kbd "C-j") 'minibuf-read-line-confirm)
(define-key *minibuf-keymap* (kbd "C-m") 'minibuf-read-line-confirm)
(define-key *minibuf-keymap* (kbd "C-i") 'minibuf-read-line-confirm)
(define-key *minibuf-keymap* (kbd "M-p") 'minibuf-read-line-prev-history)
(define-key *minibuf-keymap* (kbd "M-n") 'minibuf-read-line-next-history)
(define-key *minibuf-keymap* (kbd "C-g") 'minibuf-read-line-break)

(defvar *minibuf-read-line-prompt*)
(defvar *minibuf-read-line-comp-f*)
(defvar *minibuf-read-line-existing-p*)

(defvar *minibuf-read-line-history-table* (make-hash-table))
(defvar *minibuf-read-line-history*)

(defvar *minibuf-read-line-depth* 0)

(defun check-switch-minibuffer-window ()
  (when (minibuffer-window-active-p)
    (editor-error "Cannot switch buffer in minibuffer window")))

(defun active-minibuffer-window ()
  (if (/= 0 *minibuf-read-line-depth*)
      (minibuffer-window)
      nil))

(defun get-minibuffer-string ()
  (region-string *minibuffer-start-point*
                 (point-max (minibuffer))
                 (minibuffer)))

(defun minibuffer-clear-input ()
  (delete-region *minibuffer-start-point* (point-max (minibuffer))))

(define-commmand minibuf-read-line-confirm () ()
  (let ((str (get-minibuffer-string)))
    (when (or (string= str "")
              (null *minibuffer-read-line-existing-p*)
              (funcall *minibuf-read-line-existring-p* str))
      (throw 'minibuf-read-line-end t)))
  t)

(define-command minibuf-read-line-completion () ()
  (when *minibuf-read-line-comp-f*
    (start-completion *minibuf-read-line-comp-f*
                      (get-minibuffer-string)))
  t)

(define-command minibuf-read-line-prev-history () ()
  (multiple-value-bind (str win)
      (prev-history *minibuf-read-line-history*)
    (when win
      (minibuffer-clear-input)
      (insert-string str))))

(define-command minibuf-read-line-next-history () ()
  (multiple-value-bind (str win)
      (next-history *minibuf-read-line-history*)
    (when win
      (minibuffer-clear-input)
      (insert-string str))))

(define-command minibuf-read-line-break () ()
  (error 'editor-abort :depth (1- *minibuf-read-line-depth*)))

(defun minibuf-point-linum ()
  (window-current-linum (minibuffer-window)))

(defun minibuf-point-charpos ()
  (window-current-charpos (minibuffer-window)))
