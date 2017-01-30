(in-package :nwe-interface)

(defvar *old-display-width*)
(defvar *old-display-height*)

(defun display-init ()
  (term-init)
  (setq *old-display-width* charms/ll:*cols*)
  (setq *old-display-height* charms/ll:*lines*)
  (setq *echo-area-scrwin*
        (charms/ll:newwin (minibuffer-window-height)
                          (display-width)
                          (- (display-height) (minibuffer-window-height))
                          0)))

(defun display-width () charms/ll:*cols*)
(defun display-height () charms/ll:*lines*)

(defstruct (screen (:constructor %make-screen))
  %scrwin
  %modeline-scrwin
  x
  y
  lines
  old-lines
  wrap-lines
  width
  modifies-p)

(defun make-screen (x y width height use-modeline-p)
  (when use-modeline-p
    (decf height))
  (%make-screen :%scrwin (charms/ll:newwin height width y x)
                :%modeline-scrwin (when use-modeline-p
                                    (charms/ll:newwin 1 width (+ x y height) x))
                :x x
                :y y
                :width width
                :lines (make-array (max 0 height) :initial-element nil)
                :old-lines (make-array (max 0 height) :initial-element nil)))

(defun redraw-display-window (window doupdata-p)
  (window-see window)
  (nwe:window-prompt-display window)
  (screen-display-lines (window-screen window)
                        (screen-modified-p (window-screen window))
                        (window-buffer window)
                        (nwe::window-view-charpos window)
                        (nwe::window-view-linum window)
                        (window-current-charpos window)
                        (window-current-linum window))
  (when (window-use-modeline-p window)
    (screen-redraw-separator window)
    (screen-redraw-modeline window))
  (charms/ll:wnoutrefresh (screen-%scrwin (window-screen window)))
  (setf (screen-modified-p (window-screen window)) nil)
  (when doupdate-p
    (charms/ll:doupdate)))

(defun redraw-display ()
  (dolist (window (window-list))
    (unless (eq window (current-window))
      (redraw-display-window window nil)))
  (redraw-display-window (current-window) nil)
  (charms/ll:doupdate))
