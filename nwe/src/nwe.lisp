(in-package :nwe)

(export '(nwe))

(defmacro with-editor (() &body body)
  `(call-with-editor (lambda () ,@body)))

(defun nwe (&rest args)
  (check-init)
  (let ((report (with-editor ()
                    (mapc 'find-file args)
                  (nwe-mainloop))))
    (when report
      (format t "~&~a~%" report))))
