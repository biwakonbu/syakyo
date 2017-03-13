(in-package :nwe)

(export '(*undo-limit*
          current-buffer
          buffer
          buffer-p
          buffer-name
          buffer-filename
          buffer-modified-p
          buffer-read-only-p
          buffer-enable-undo-p
          buffer-major-omde
          buffer-minor-modes
          buffer-mark-p
          buffer-nlines
          buffer-overlays
          buffer-truncate-lines
          buffer-enable-undo
          buffer-disable-undo
          buffer-put-property
          buffer-get-char
          buffer-line-string-with-attributes
          buffer-line-string
          map-buffer-lines
          buffer-take-lines
          buffer-erase
          buffer-rename
          buffer-directory
          buffer-undo-boundary
          get-bvar
          clear-buffer-variables
          buffer-add-delete-hook))

(defstruct (line (:constructor %make-line))
  prev
  next
  str
  plist
  %symbol-lifetimes
  %region)

(defun make-buffer (name &key filename read-only-p (enable-undo-p t))
  (let ((buffer (make-instance 'buffer
                   :name name
                   :%filename filename
                   :%directory (when filename (directory-namestring filename))
                   :read-only-p read-only-p
                   :%enable-undo-p enable-undo-p
                   :major-mode 'fundamental-mode)))
    (buffer-reset buffer)
    (setf (buffer-%modified-p buffer) 0)
    (setf (buffer-undo-size buffer) 0)
    (setf (buffer-undo-stack buffer) nil)
    (setf (buffer-redo-stack buffer) nil)
    (setf (buffer-markers buffer) nil)
    (setf (buffer-truncate-lines buffer) t)
    (setf (buffer-variables buffer) (make-hash-table :test 'equal))
    (setf (buffer-point-marker buffer)
          (make-marker buffer (make-min-point)
                       :name "buffer-point"))
    (add-buffer buffer)
    buffer))
