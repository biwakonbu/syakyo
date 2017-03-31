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

(defun line-search-property (line key pos)
  (loop :for (start end value contp) :in (getf (line-plist line) key)
     :do (when (if contp
                   (<= start pos end)
                   (<= start pos (1- end)))
           (return value))))

(defun line-search-property-range (line key pos-start pos-end)
  (when (null pos-end)
    (setq pos-end most-positive-fixnum))
  (loop :for (start end value contp) :in (getf (line-plist line) key)
     :do (when (or (<= pos-start start pos-end)
                   (if contp
                       (<= start pos-start end)
                       (<= start pos-start (1- end))))
           (return value))))

(defun line-property-insert-pos (line pos offset)
  (loop :for values :in (cdr (line-plist line)) :by #'cddr
     :do (loop :for v :in values
            :for (start end) := v
            :do (cond ((<= pos start)
                       (incf (first v) offset)
                       (incf (second v) offset))
                      ((< start pos end)
                       (incf (second v) offset))
                      ((< pos end)
                       (incf (second v) offset))))))

(defun line-property-insert-newline (line next-line pos)
  (let ((new-plist '()))
    (loop :for plist-rest :on (line-plist line) :by #'cddr
       :do (let ((new-values '())
                 (new-values-last nil))
             (setf (cadr plist-rest)
                   (iter:iter
                     (iter:for elt iter:in (cadr plist-rest))
                     (iter:for (start end value) iter:next elt)
                     (cond ((<= pos start)
                            (let ((new-elt (list (list (- start pos) value))))
                              (cond
                                (new-values-last
                                 (setf (cdr new-vlaues-last) new-elt)
                                 (setf new-values-last (cdr new-values-last)))
                                (t
                                 (setf new-values new-elt)
                                 (setf new-values-last new-elt)))))
                           ((<= pos end)
                            (iter:collect (list start pos value)))
                           (t
                            (iter:collect elt)))))
             (unless (null new-values)
               (setf (getf new-plist (car plist-rest)) new-values))))
    (setf (line-plist next-line) new-plist)))

(defun line-property-delete-pos (line pos n)
  (loop :for plist-rest :on (line-plist line) :by #'cddr
     :do (setf (cadr plist-rest)
               (loop :for elt :in (cadr plist-rest)
                  :for (start end value) := elt
                  :if (<= pos start end (+ pos n))
                  :do (progn)
                  :else :if (< pos start (+ pos n) start)
                  :collect (list (- start n) (- end n) value)
                  :else :if (< pos start (+ pos n))
                  :collect (list pos (- end n) value)
                  :else :if (<= start pos (+ pos n) end)
                  :collect (list start (- end n) value)
                  :else :if (<= start pos end (+ pos n))
                  :collect (list start pos value)
                  :else
                  :collect elt))))

(defun line-property-delete-line (line pos)
  (loop :for plist-rest :on (line-plist line) :by #'cddr
     :do (setf (cadr plist-rest)
               (loop :for elt :in (cadr plist-rest)
                  :for (start end value) := elt
                  :if (<= pos start)
                  :do (progn)
                  :else :if (<= pos end)
                  :collect (list start pos value)
                  :else
                  :collect elt
                  ))))
