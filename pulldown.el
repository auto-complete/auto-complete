;;; pulldown.el --- Visual pulldown menu interface

;; Copyright (C) 2009  Tomohiro Matsuyama

;; Author: Tomohiro Matsuyama <m2ym.pub@gmail.com>
;; Keywords: lisp
;; Version: 0.2a

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(eval-when-compile
  (require 'cl))

(defgroup pulldown nil
  "Visual pulldown menu interface"
  :group 'lisp
  :prefix "pulldown-")

(defface pulldown-default-face
  '((t (:background "lightgray" :foreground "black" :underline "darkgray")))
  "Face for pulldown menu."
  :group 'pulldown)

(defface pulldown-default-selection-face
  '((t (:background "steelblue" :foreground "white")))
  "Face for selection."
  :group 'pulldown)

(defface pulldown-scroll-bar-foreground-face
  '((t (:background "black")))
  "Foreground face for scroll-bar."
  :group 'pulldown)

(defface pulldown-scroll-bar-background-face
  '((t (:background "gray")))
  "Background face for scroll-bar."
  :group 'pulldown)

(defvar pulldown-scroll-bar-foreground-char
  (propertize " " 'face 'pulldown-scroll-bar-foreground-face)
  "Foreground character for scroll-bar.")

(defvar pulldown-scroll-bar-background-char
  (propertize " " 'face 'pulldown-scroll-bar-background-face)
  "Background character for scroll-bar.")

(defstruct pulldown
  point row column width height direction overlays
  parent depth
  face selection-face
  margin-left margin-right margin-left-cancel scroll-bar icon-width
  cursor offset scroll-top list)

(defun pulldown-x-to-string (x)
  "Convert any object to string effeciently.
This is faster than prin1-to-string in many cases."
  (typecase x
    (string x)
    (symbol (symbol-name x))
    (integer (number-to-string x))
    (float (number-to-string x))
    (t (format "%s" x))))

(defun pulldown-current-physical-column ()
  (car (posn-col-row (posn-at-point))))

(defun pulldown-last-line-of-buffer-p ()
  (save-excursion (end-of-line) (/= (forward-line) 0)))

(defun pulldown-item-propertize (item &rest properties)
  (apply 'propertize
         (if (stringp item)
             item
           (pulldown-x-to-string item))
         properties))

(defun pulldown-item-property (item property)
  (if (stringp item)
      (get-text-property 0 property item)))

(defsubst pulldown-item-value (item)
  (pulldown-item-property item 'value))

(defsubst pulldown-item-value-or-self (item)
  (or (pulldown-item-value item)
      item))

(defsubst pulldown-item-icon (item)
  (pulldown-item-property item 'icon))

(defsubst pulldown-item-sublist (item)
  (pulldown-item-property item 'sublist))

(defun* pulldown-make-item (name
                            &key
                            value
                            sublist
                            icon)
  "Utility function to make pulldown item.
See also `pulldown-item-propertize'."
  (pulldown-item-propertize name
                            'value value
                            'sublist sublist
                            'icon icon))

(defun pulldown-selected-item (menu)
  (nth (pulldown-cursor menu) (pulldown-list menu)))
  
(defun pulldown-set-list (menu list)
  (setf (pulldown-list menu) list
        (pulldown-offset menu) (if (> (pulldown-direction menu) 0)
                                   0
                                 (max (- (pulldown-height menu) (length list)) 0))))

(defun pulldown-line-overlay (menu line)
  (aref (pulldown-overlays menu) line))

(defun pulldown-selected-line-overlay (menu)
  (aref (pulldown-overlays menu) (- (pulldown-cursor menu) (pulldown-scroll-top menu))))

(defun pulldown-hide-line (menu line)
  (let ((overlay (pulldown-line-overlay menu line)))
    (overlay-put overlay 'display nil)
    (overlay-put overlay 'after-string nil)))

(defun pulldown-set-line-item (menu line item face margin-left margin-right scroll-bar-char icon)
  (let* ((overlay (pulldown-line-overlay menu line))
         (content (propertize (concat margin-left
                                      (pulldown-create-line-string menu item)
                                      icon
                                      margin-right)
                              'face face)))
    (overlay-put overlay 'real-item item)
    (unless (overlay-get overlay 'dangle)
      (overlay-put overlay 'display (substring content 0 1))
      (setq content (concat (substring content 1))))
    (overlay-put overlay
                 'after-string
                 (concat (overlay-get overlay 'prefix)
                         (propertize content 'face face)
                         scroll-bar-char
                         (overlay-get overlay 'postfix)))))

(defun pulldown-create-line-string (menu item)
  (loop with string = (pulldown-x-to-string item)
        with width = 0
        with menu-width = (pulldown-width menu)
        for length from 0
        for c in (append string nil)
        while (<= (incf width (char-width c)) menu-width)
        finally return
        (let ((string-width (string-width (if (< length (length string))
                                              (substring string 0 length)
                                            string))))
          (if (< string-width menu-width)
              ;; Padding
              (concat string (make-string (- menu-width string-width) ? ))
            string))))

(defsubst pulldown-live-p (menu)
  (and menu (pulldown-overlays menu) t))

(defun pulldown-hide (menu)
  (dotimes (i (pulldown-height menu))
    (pulldown-hide-line menu i)))

(defun pulldown-draw (menu)
  (loop with height = (pulldown-height menu)
        with list = (pulldown-list menu)
        with length = (length list)
        with thum-size = (max (/ (* height height) length) 1)
        with page-size = (/ (+ 0.0 length) height)
        with margin-left = (make-string (if (pulldown-margin-left-cancel menu) 0 (pulldown-margin-left menu)) ? )
        with margin-right = (make-string (pulldown-margin-right menu) ? )
        with icon-width = (pulldown-icon-width menu)
        with cursor = (pulldown-cursor menu)
        with scroll-top = (pulldown-scroll-top menu)
        with offset = (pulldown-offset menu)
        for o from offset
        for i from scroll-top
        while (< o height)
        for item in (nthcdr scroll-top list)
        for page-index = (* thum-size (/ o thum-size))
        for face = (if (= i cursor)
                       (or (pulldown-item-property item 'selection-face) (pulldown-selection-face menu))
                     (or (pulldown-item-property item 'menu-face) (pulldown-face menu)))
        for empty-char = (propertize " " 'face face)
        for scroll-bar-char = (if (pulldown-scroll-bar menu)
                                  (cond
                                   ((<= page-size 1)
                                    empty-char)
                                   ((and (> page-size 1)
                                         (>= cursor (* page-index page-size))
                                         (< cursor (* (+ page-index thum-size) page-size)))
                                    pulldown-scroll-bar-foreground-char)
                                   (t
                                    pulldown-scroll-bar-background-char))
                                "")
        for icon = (let* ((icon (pulldown-item-icon item)) ; TODO double width characters
                          (w (length icon)))
                     (cond
                      ((null icon)
                       (make-string icon-width ? ))
                      ((>= w icon-width)
                       (substring icon 0 icon-width)) 
                      (t
                       (concat icon (make-string (- icon-width w) ? )))))
        
        do
        ;; Show line and set item to the line
        (pulldown-set-line-item menu o item face margin-left margin-right scroll-bar-char icon)
        
        finally
        ;; Hide remaining lines
        (if (> (pulldown-direction menu) 0)
            (while (< o height)
              (pulldown-hide-line menu o)
              (incf o))
          (dotimes (o offset)
            (pulldown-hide-line menu o)))))

(defun pulldown-next (menu)
  (let ((height (pulldown-height menu))
        (cursor (1+ (pulldown-cursor menu)))
        (scroll-top (pulldown-scroll-top menu))
        (length (length (pulldown-list menu))))
    (cond
     ((>= cursor length)
      ;; Back to first page
      (setq cursor 0
            scroll-top 0))
     ((= cursor (+ scroll-top height))
      ;; Go to next page
      (setq scroll-top (min (1+ scroll-top) (max (- length height) 0)))))
    (setf (pulldown-cursor menu) cursor
          (pulldown-scroll-top menu) scroll-top)
    (pulldown-draw menu)))

(defun pulldown-previous (menu)
  (let ((height (pulldown-height menu))
        (cursor (1- (pulldown-cursor menu)))
        (scroll-top (pulldown-scroll-top menu))
        (length (length (pulldown-list menu))))
    (cond
     ((< cursor 0)
      ;; Go to last page
      (setq cursor (1- length)
            scroll-top (max (- length height) 0)))
     ((= cursor (1- scroll-top))
      ;; Go to previous page
      (decf scroll-top)))
    (setf (pulldown-cursor menu) cursor
          (pulldown-scroll-top menu) scroll-top)
    (pulldown-draw menu)))

(defun* pulldown-create (point
                         width
                         height
                         &key
                         (first-motion t)
                         (face 'pulldown-default-face)
                         (selection-face 'pulldown-default-selection-face)
                         scroll-bar
                         margin-left
                         margin-right
                         icon-width
                         parent)
  (unless point
    (if parent
        (setq point (overlay-end (pulldown-selected-line-overlay parent))
              first-motion nil)
      (setq point (point))))
  (unless margin-left
    (setq margin-left 0))
  (unless margin-right
    (setq margin-right 0))
  (unless icon-width
    (setq icon-width 0))
  (save-excursion
    (goto-char point)
    (let* ((row (line-number-at-pos))
           (column (pulldown-current-physical-column))
           (overlays (make-vector height nil))
           (menu-width (+ width
                          (if scroll-bar 1 0)
                          margin-left
                          margin-right
                          icon-width))
           margin-left-cancel
           (window (selected-window))
           (window-start (window-start))
           (window-hscroll (window-hscroll))
           (window-width (window-width))
           (right (+ column menu-width))
           (direction (or
                       ;; Currently the direction of cascade menu won't be changed
                       (and parent (pulldown-direction parent))

                       ;; Calculate direction
                       (if (and (> row height)
                                (> height (- (max 1 (- (window-height)
                                                       (if mode-line-format 1 0)
                                                       (if header-line-format 1 0)))
                                             (count-lines window-start (point)))))
                           -1
                         1)))
           (depth (if parent (1+ (pulldown-depth parent)) 0))
           current-column)
      (save-excursion
        (goto-char (point-max))
        (let ((buffer-undo-list t)) (insert (make-string height ?\n))))

      (when (null parent)               ;TODO
        (if (and (> right window-width)
                 (>= right menu-width)
                 (>= column menu-width))
            (decf column (- menu-width margin-left margin-right))
          (decf column margin-left)
          (when (< column 0)
            ;; Cancel margin left
            (setq column 0)
            (decf menu-width margin-left)
            (setq margin-left-cancel t))))
      
      (dotimes (i height)
        (let (overlay begin w (dangle t) (prefix "") (postfix ""))
          (when first-motion
            (if (>= emacs-major-version 23)
                (vertical-motion (cons column direction))
              (vertical-motion direction)
              (move-to-column (+ (current-column) column))))
	  (setq first-motion t
                current-column (pulldown-current-physical-column))

          (when (> current-column column)
            (backward-char)
            (setq current-column (pulldown-current-physical-column)))
          (when (< current-column column)
            ;; Extend short buffer lines by menu prefix (line of spaces)
            (setq prefix (make-string (+ (if (= current-column 0)
                                             (- window-hscroll (current-column))
                                           0)
                                         (- column current-column))
                                      ? )))
          
          (setq begin (point))
          (setq w (+ menu-width (length prefix)))
          (while (and (not (eolp)) (> w 0))
            (setq dangle nil)
            (decf w (char-width (char-after)))
            (forward-char))
          (if (< w 0)
              (setq postfix (make-string (- w) ? )))

          (setq overlay (make-overlay begin (point)))
          (overlay-put overlay 'window window)
          (overlay-put overlay 'dangle dangle)
          (overlay-put overlay 'prefix prefix)
          (overlay-put overlay 'postfix postfix)
          (overlay-put overlay 'width width)
          (aset overlays
		(if (> direction 0) i (- height i 1))
		overlay)))
      (loop for p from (- 10000 (* depth 1000))
            for overlay in (nreverse (append overlays nil))
            do (overlay-put overlay 'priority p))
      (make-pulldown :point point
                     :row row
                     :column column
                     :width width
                     :height height
                     :direction direction
                     :parent parent
                     :depth depth
                     :face face
                     :selection-face selection-face
                     :margin-left margin-left
                     :margin-right margin-right
                     :margin-left-cancel margin-left-cancel
                     :scroll-bar scroll-bar
                     :icon-width icon-width
                     :cursor 0
                     :scroll-top 0
                     :list '()
                     :overlays overlays))))

(defun pulldown-delete (menu)
  (when (pulldown-live-p menu)
    (pulldown-hide menu)
    (mapc 'delete-overlay (pulldown-overlays menu))
    (setf (pulldown-overlays menu) nil)
    (save-excursion
      (goto-char (point-max))
      (let ((buffer-undo-list t))
        (delete-char (- (pulldown-height menu)))))))

(defun pulldown-preferred-width (list)
  "Return preferred width of pulldown menu to show `LIST' beautifully."
  (loop for item in list
        maximize (string-width (pulldown-x-to-string item)) into width
        finally return (* (ceiling (/ (or width 0) 10.0)) 10)))

(defun pulldown-lookup-key-by-event (function event)
  (or (funcall function (vector event))
      (if (symbolp event)
          (let ((mask (get event 'event-symbol-element-mask)))
            (if mask
                (funcall function (vector (logior (or (get (car mask) 'ascii-character) 0)
                                                  (cadr mask)))))))))

(defun* pulldown-event-loop (menu keymap fallback &optional prompt &aux event binding)
  (unwind-protect
      (block nil
        (while (and (pulldown-live-p menu)
                    (setq event (progn (clear-this-command-keys) (read-event prompt))))
          (setq binding (pulldown-lookup-key-by-event (lambda (key) (lookup-key keymap key)) event))
          (cond
           ((eq binding 'pulldown-close)
            (if (pulldown-parent menu) (return nil)))
           ((memq binding '(pulldown-select pulldown-open))
            (let* ((item (pulldown-selected-item menu))
                   (sublist (pulldown-item-sublist item)))
              (if sublist
                  (let ((value (pulldown-cascade-menu sublist
                                                      :parent menu
                                                      :margin-left (pulldown-margin-left menu)
                                                      :margin-right (pulldown-margin-right menu)
                                                      :scroll-bar (pulldown-scroll-bar menu))))
                    (if value (return value)))
                (if (eq binding 'pulldown-select)
                    (return (pulldown-item-value-or-self item))))))
           ((eq binding 'pulldown-next)
            (pulldown-next menu))
           ((eq binding 'pulldown-previous)
            (pulldown-previous menu))
           (binding
            (call-interactively binding))
           (t
            (funcall fallback event (pulldown-lookup-key-by-event (lambda (key) (key-binding key)) event))))))
    (pulldown-delete menu)))

(defun pulldown-default-fallback (event default))
  
(defun* pulldown-menu (list
                       &key
                       point
                       (first-motion t)
                       (width (pulldown-preferred-width list))
                       (height 10)
                       margin
                       margin-left
                       margin-right
                       scroll-bar
                       icon
                       icon-width
                       parent
                       (keymap pulldown-keymap)
                       (fallback 'pulldown-default-fallback)
                       prompt
                       &aux menu event)
  (if (eq margin t) (setq margin 1))
  (unless margin-left (setq margin-left margin))
  (unless margin-right (setq margin-right margin))
  (if (and scroll-bar
           (integerp margin-right)
           (> margin-right 0))
      ;; Make scroll-bar space as margin-right
      (decf margin-right))
  (if (and (eq icon t)
           (null icon-width))
      (setq icon-width 1))

  (setq menu (pulldown-create point width height
                              :margin-left margin-left
                              :margin-right margin-right
                              :scroll-bar scroll-bar
                              :icon-width icon-width
                              :parent parent
                              :first-motion first-motion))
  (pulldown-set-list menu list)
  (pulldown-draw menu)
  (pulldown-event-loop menu keymap fallback prompt))

(defun pulldown-cascade-menu (list &rest args)
  "Same to `pulldown-menu', but an element of `LIST' can be
list of submenu." 
  (apply 'pulldown-menu
         (mapcar (lambda (item)
                   (if (consp item)
                       (pulldown-make-item (car item)
                                           :sublist (cdr item)
                                           :icon ">")
                     item))
                 list)
         :icon t
         args))

(defvar pulldown-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "\r" 'pulldown-select)
    (define-key map "\C-n" 'pulldown-next)
    (define-key map "\C-p" 'pulldown-previous)
    (define-key map [down] 'pulldown-next)
    (define-key map [up] 'pulldown-previous)
    (define-key map [right] 'pulldown-open)
    (define-key map [left] 'pulldown-close)
    map))

(provide 'pulldown)
;;; pulldown.el ends here
