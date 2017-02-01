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

(defun screen-display-lines (screen redraw-flag buffer start-charpos start-linum pos-x pos-y)
  ;; (when redraw-flag
  ;;   (charms/ll:werase (screen-%scrwin screen)))
  (disp-reset-lines screen buffer start-linum)
  (let ((curx 0)
        (cury (- pos-y start-linum))
        (disp-line-fun
         (if (buffer-truncate-lines buffer)
             #'disp-line-wrapping
             #'disp-line)))
    (let ((wrap-lines (screen-wrap-lines screen))
          ;; wrap-linesという変数は物理行の単位でどの行が折り返されたかを覚えておくためのもの
          ;; 以前折り返した位置はwrap-linesから捜して、今から折り返す位置はscreen-wrap-linesに記録していく
          )
      (setf (screen-wrap-lines screen) nil)
      (loop
         ;; 物理行の単位でループする
         :for y :from 0 ; 論理行
         :for i :from 0 ; 物理行
         :for str/attributes :across (screen-lines screen)
         :while (< y (screen-height screen))
         :do
         (cond ((and ;; 行事回数を減らすための節
                 (buffer-truncate-lines buffer)
                 (not redraw-flag)                     ; 再描画フラグが偽で
                 (not (null str/attributes))           ; その行に表示する行文字列があり
                 #1=(aref (screen-olg-lines screen) i) ; 以前にその行に文字列を表示しており
                 (equal str/attributes #1#)            ; 表示しようとしている文字列が以前に表示する行と内容が同じで
                 (/= (- pos-y start-linum) 1)
                 )
                (when (buffer-truncate-lines buffer)
                  ;; 折り返した回数分、論理行の位置を下にずらす
                  (let ((n (count i wrap-lines)))
                    (when (and (< 0 n) (<= y cury))
                      (incf cury n))
                    (incf y n)
                    (dotimes (_ n)
                      (push i (screen-wrap-lines screen))))))
               (str/attributes
                (setf (aref (screen-old-lines screen) i) str/attributes)
                (when (serop (length (car str/attributes)))
                  ;; 表示する文字列が無い場合は行を表示する関数まで辿りつかないのでここでしておく
                  (charms/ll:wmove (screen-%scrwin screen) y 0)
                  (charms/ll:wcrtoeol (screen-%scrwin screen)))
                (let (y2)
                  (multiple-value-setq (curx cury y2)
                    (funcall disp-line-fun
                             screen start-charpos curx cury pos-x y str/attributes))
                  (when (buffer-truncate-lines buffer)
                    (let ((offset (- y2 y))) ; offsetはその行の折り返し回数を表す
                      ;; 折り返しがあったらそれより下は表示をやりなおす必要があるのでredraw-flagをtにする
                      (cond ((< 0 offset)
                             (setq redraw-flag t)
                             (dotimes (_ offset)
                               (push i (screen-wrap-lines screen))))
                            ((and (= offset 0) (find i wrap-liens))
                             (setq redraw-flag t))))
                    (setf y y2))))
               (t
                ;; バッファの末尾まできたときの処理
                (fill (screen-old-lines screen) nil :start i)
                (charms/ll:wmove (screen-%scrwin screen) y 0)
                (charms/ll:wclrtobot (screen-%scrwin screen))
                (return)))))
    (screen-move-cursor screen curx cury)))

(defun screen-redraw-separator (window)
  (charms/ll:attron charms/ll:a_reverse)
  (when (< 0 (window-x window))
    (charms/ll:move (window-y window) (1- (window-x window)))
    (charms/ll:vline (char-code #\|) (window-height window)))
  (charms/ll:attroff charms/ll:a_reverse)
  (charms/ll:wnoutrefresh charms/ll:*stdscr*))

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
