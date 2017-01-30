(in-package :nwe)

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
