;;; pulldown.el --- Visual pulldown menu interface

;; Copyright (C) 2009  Tomohiro Matsuyama

;; Author: Tomohiro Matsuyama <m2ym.pub@gmail.com>
;; Keywords: lisp
;; Version: 0.1

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

(defstruct pulldown
  point row column width height direction overlays
  newline-added
  face selection-face
  cursor offset scroll-top list)

(defun pulldown-x-to-string (x)
  "Convert anything to string effeciently."
  (typecase x
    (string x)
    (symbol (symbol-name x))
    (integer (number-to-string x))
    (float (number-to-string x))
    (t (format "%s" x))))

(defun pulldown-goto-line (line)
  "Goto `LINE' regarding of narrowing."
  (goto-char (point-min))
  (forward-line (1- line)))

(defun pulldown-current-physical-column ()
  "Current physical column. (not logical column)"
  (car (posn-col-row (posn-at-point))))

(defun pulldown-last-line-of-buffer ()
  "Return non-nil if at last line of buffer."
  (save-excursion (/= (forward-line) 0)))

(defun pulldown-item-propertize (item &rest properties)
  (if (stringp item)
      (apply 'propertize item properties)
    item))

(defun pulldown-item-property (item property)
  (if (stringp item)
      (get-text-property 0 property item)))

(defun pulldown-set-list (menu list)
  "Set menu list."
  (setf (pulldown-list menu) list
        (pulldown-offset menu) (if (> (pulldown-direction menu) 0)
                                   0
                                 (max (- (pulldown-height menu) (length list)) 0))))

(defun pulldown-line-overlay (menu line)
  "Return a overlay of `MENU' at `LINE'."
  (aref (pulldown-overlays menu) line))

(defun pulldown-hide-line (menu line)
  "Hide `LINE' in `MENU'."
  (let ((overlay (pulldown-line-overlay menu line)))
    (overlay-put overlay 'invisible nil)
    (overlay-put overlay 'after-string nil)))

(defun pulldown-show-line (menu line)
  "Show `LINE' in `MENU'."
  (overlay-put (pulldown-line-overlay menu line) 'invisible t))

(defun pulldown-set-line-item (menu line item &optional face)
  "Set list of `LINE' in `MENU'."
  (let ((overlay (pulldown-line-overlay menu line)))
    (overlay-put overlay 'real-item item)
    (overlay-put overlay
                 'after-string
                 (concat (overlay-get overlay 'prefix)
                         (propertize (pulldown-create-line-string menu item) 'face face)
                         (overlay-get overlay 'postfix)))))

(defun pulldown-create-line-string (menu item)
  "Create string for showing `ITEM' in `MENU'."
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

(defun pulldown-hide (menu)
  "Hide `MENU'."
  (dotimes (i (pulldown-height menu))
    (pulldown-hide-line menu i)))

(defun pulldown-draw (menu)
  "Draw `MENU'."
  (loop with height = (pulldown-height menu)
        with list = (pulldown-list menu)
        with cursor = (pulldown-cursor menu)
        with scroll-top = (pulldown-scroll-top menu)
        with offset = (pulldown-offset menu)
        for o from offset
        for i from scroll-top
        for item in (nthcdr scroll-top list)
        while (< o height)

        do
        ;; Show line and set item to the line
        (pulldown-show-line menu o)
        (pulldown-set-line-item
         menu o item
         (if (= i cursor)
             (or (pulldown-item-property item 'selection-face) (pulldown-selection-face menu))
           (or (pulldown-item-property item 'menu-face) (pulldown-face menu))))
        
        finally
        ;; Hide remaining lines
        (if (> (pulldown-direction menu) 0)
            (while (< o height)
              (pulldown-hide-line menu o)
              (incf o))
          (dotimes (o offset)
            (pulldown-hide-line menu o)))))

(defun pulldown-next (menu)
  "Select next item."
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
  "Select previous item."
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

(defun* pulldown-create (point width height
                               &key
                               (face 'pulldown-default-face)
                               (selection-face 'pulldown-default-selection-face))
  "Create pulldown menu."
  (save-excursion
    (goto-char point)
    (let* ((row (line-number-at-pos))
           (column (pulldown-current-physical-column))
           (overlays (make-vector height nil))
           (window (selected-window))
           (window-start (window-start))
           (window-hscroll (window-hscroll))
           (window-width (window-width))
           (right (+ column width))
           (direction (if (and (> row height)
                               (> height (- (max 1 (- (window-height)
                                                      (if mode-line-format 1 0)
                                                      (if header-line-format 1 0)))
                                            (count-lines window-start (point)))))
                          -1
                        1))
           (newline-added (save-excursion
                            (goto-char (point-max))
                            (unless (bolp)
                              (newline)
                              t)))
           current-column)
      (if (and (> right window-width)
               (>= right width)
               (>= column width))
          (decf column width))
      (dotimes (i height)
        (let (overlay begin w (prefix "") (postfix ""))
          (if (>= emacs-major-version 23)
              (vertical-motion (cons column direction))
            (vertical-motion direction)
            (move-to-column (+ (current-column) column)))
	  (setq current-column (pulldown-current-physical-column))

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
          (setq w (+ width (length prefix)))
          (while (and (not (eolp)) (> w 0))
            (decf w (char-width (char-after)))
            (forward-char))
          (if (< w 0)
              (setq postfix (make-string (- w) ? )))
          (if (pulldown-last-line-of-buffer)
              (setq postfix (concat postfix "\n")))

          (setq overlay (make-overlay begin (point)))
          (overlay-put overlay 'window window)
          (overlay-put overlay 'prefix prefix)
          (overlay-put overlay 'postfix postfix)
          (overlay-put overlay 'width width)
          (aset overlays
		(if (> direction 0) i (- height i 1))
		overlay)))
      (loop for p from 100
            for overlay in (nreverse (append overlays nil))
            do (overlay-put overlay 'priority p))
      (make-pulldown :point point
                     :row row
                     :column column
                     :width width
                     :height height
                     :direction direction
                     :newline-added newline-added
                     :face face
                     :selection-face selection-face
                     :cursor 0
                     :scroll-top 0
                     :list '()
                     :overlays overlays))))

(defun pulldown-delete (menu)
  (mapcar 'delete-overlay (pulldown-overlays menu))
  (setf (pulldown-overlays menu) nil)
  (if (pulldown-newline-added menu)
      (save-excursion
        (goto-char (point-max))
        (if (and (bolp) (eolp))
            (delete-char -1)))))

(defun pulldown-live-p (menu)
  (and menu (pulldown-overlays menu) t))

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

(defun* pulldown-event-loop (menu keymap fallback &optional message &aux event binding)
  (unwind-protect
      (block nil
        (while (and (pulldown-live-p menu)
                    (setq event (progn (clear-this-command-keys) (read-event message))))
          (if (eq event 'Quit)
              (return nil))
          (setq binding (pulldown-lookup-key-by-event (lambda (key) (lookup-key keymap key)) event))
          (cond
           ((eq binding 'pulldown-select)
            (return (nth (pulldown-cursor menu) (pulldown-list menu))))
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
                       (width (pulldown-preferred-width list))
                       (height 10)
                       (keymap pulldown-keymap)
                       (fallback 'pulldown-default-fallback)
                       message
                       &aux menu event)
  (setq menu (pulldown-create (point) width height))
  (pulldown-set-list menu list)
  (pulldown-draw menu)
  (pulldown-event-loop menu keymap fallback message))

(defvar pulldown-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "\r" 'pulldown-select)

    (define-key map "\C-n" 'pulldown-next)
    (define-key map "\C-p" 'pulldown-previous)
    (define-key map [down] 'pulldown-next)
    (define-key map [up] 'pulldown-previous)
    map))

(provide 'pulldown)
;;; pulldown.el ends here
