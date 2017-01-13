(in-package :nwe)

(export '(nwe))

(let ((passed nil))
  (defun call-with-editor (function)
    (unwind-protect
         (let ((*running-p* t))
           (unless passed
             (setq passed t)
             (display-init)
             (window-init)
             (minibuf-init)
             (run-hooks 'after-init-hook))
           (funcall function))
      (display-finalize))))

(defmacro with-editor (() &body body)
  `(call-with-editor (lambda () ,@body)))

(defun check-init ()
  (when *running-p*
    (error "~A is already running" *program-name*)))

(defun nwe (&rest args)
  (check-init)
  (let ((report (with-editor ()
                    (mapc 'find-file args)
                  (nwe-mainloop))))
    (when report
      (format t "~&~a~%" report))))
