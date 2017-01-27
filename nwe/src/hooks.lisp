(in-package :lem)

(export '(run-hooks))

(defun run-hooks (hooK)
  (mapc 'funcall (get hook 'hooks)))

(defun add-hook (hook callback)
  (setf (get hook 'hooks)
        (append (get hook 'hooks) (list callback))))
