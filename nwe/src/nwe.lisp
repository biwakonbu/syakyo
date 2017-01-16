(in-package :nwe)

(export '(nwe))

(defvar *running-p* nil)

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

;; main loop
(defun nwe-mainlopp ()
  (macrolet ((form (&body body)
               `(cond (*debug-p*
                       (handler-bind ((error #'bailout)
                                      #+sbcl (sb-sys:interactive-interrupt #'bailout))
                         ,@body))
                      (t
                       ,@body))))
    (do-commandloop (:toplevel t)
      (with-error-handler ()
        (form
         (syntax-scan-lines (current-buffer)
                            (current-linum)
                            (1+ (current-linum)))
         (redraw-display)
         (let ()
           (start-idle-timers)
           (let ((cmd (read-key-command)))
             (stop-idle-timers)
             (if (changed-disk-p (current-buffer))
                 (ask-revert-buffer)
                 (progn
                   (message nil)
                   (handler-case
                       (handler-case
                           (handler-bind ((editor-condition
                                           (lambda (c)
                                             (declare (ignore c))
                                             (stop-record-key))))
                             (cmd-call cmd nil))
                         (editor-abort ()
                           (buffer-mark-cancel (current-buffer))
                           (message "Quit"))
                         (read-only-error ()
                           (message "Read Only"))
                         (editor-error (c)
                           (message (editor-error-message))))))))))))))

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
