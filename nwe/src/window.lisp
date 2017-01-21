(in-package :nwe)

(defun window-max-width () (display-width))
(defun window-max-height () (- (display-height) (minibuffer-window-height)))


(defvar *current-windo
(defvar *modified-window-tree-p* nil)
(defvar *window-tree*)

(defun make-window (buffer x y width height use-modeline-p)
  (setf *modified-window-tree-p* t)
  (make-instance 'window
                 :x x
                 :y y
                 :width width
                 :height height
                 :%buffer buffer
                 :screen (make-screen x y width height use-modeline-p)
                 :view-marker (make-marker buffer (make-min-point) :name "view")
                 :use-modeline-p use-modeline-p
                 :point-marker (make-marker buffer (make-min-point) :name "point")))

(defun current-window ()
  *current-window*)

(defun window-tree ()
  *window-tree*)

(defun window-init ()
  (setf (current-window)
        (make-window (get-buffer-create "*tmp*")
                     0
                     0
                     (window-max-width)
                     (window-max-height)
                     t))
  (setf (window-tree) (current-window)))
