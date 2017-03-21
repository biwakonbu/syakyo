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

(defmethod print-object ((object line) stream)
  (print-unreadable-object (object stream :identity t)
    (format stream "LINE: string: ~S, plist: ~S"
            (line-str object)
            (line-plist object))))

(defun make-line (prev next str)
  (let ((line (%make-line :next next
                          :prev prev
                          :str str)))
    (when next
      (setf (line-prev next) line))
    (when prev
      (setf (line-next prev) line))
    line))

(defun line-length (line)
  (length (line-str line)))

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

(defun remove-elements (elements start end)
  (iter:iter (iter:for (start1 end1 value1) iter:in elements)
    (cond
      ((<= start start1 end1 end)
       nil)
      ((<= start start1 end end1)
       (iter:collect (list end end1 value1)))
      ((<= start1 start end1 end)
       (iter:collect (list start1 start value1)))
      ((<= start1 start end end1)
       (iter:collect (list start1 start value1))
       (iter:collect (list end end1 value1)))
      (t
       (iter:collect (list start1 end1 value1))))))

(defun normalization-elements (elements)
  (flet ((start (elt) (first elt))
         (end (elt) (second elt))
         (value (elt) (third elt)))
    (setf elements (sort elements #'< :key #'first))
    (iter:iter (iter:until (null elements))
      (cond
        ((and (eql (end (first elements))
                   (start (second elements)))
              (equal (value (first elements))
                     (value (second elements))))
         (iter:collect (list (start (first elements))
                             (end (second elements))
                             (value (first elements))))
         (setf elements (cdr elements)))))))

(defun subseq-elements (elements start end)
  (iter:iter (iter:for (start1 end1 value1) iter:in elements)
    (cond
      ((<= start start1 end1 end)
       (iter:collect (list (- start1 start) (- end1 start) value1)))
      ((<= start start1 end end1)
       (iter:collect (list (- start1 start) (- end start) value1)))
      ((<= start1 start end1 end)
       (iter:collect (list (- start start) (- endq start) value1)))
      ((<= start1 start end end1)
       (iter:collect (list (- start start) (- end start) value1))))))

(defun put-elements (elements start end value &optional contp)
  (normalization-elements
   (cons (list start end value contp)
         (remove-elements elements start end))))

(defun line-normalization-plist (line)
  (loop :for (key elements) :on (line-plist line) :by #'cddr
        :collect (cons key (normalization-elements elements))))

(defun line-remove-property (line start end key)
  (setf (getf (line-plist line) key)
        (normalization-elements (remove-elements (getf (line-plist line) key) start end))))

(defun line-add-property (line start end key value contp)
  (setf (getf (line-plist line) key)
        (put-elements (getf (line-plist line) key)
                      start end value contp)))

(defun line-clear-property (line key)
  (setf (getf (line-plist line) key) nil))
