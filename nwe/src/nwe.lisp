(in-package :nwe)

(export '(nwe))

(defun nwe (&rest args)
  (check-init)
  (let ((report (with-editor ()
                    (mapc 'find-file args)
                  (nwe-mainloop))))
    (when report
      (format t "~&~a~%" report))))
