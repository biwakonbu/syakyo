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
