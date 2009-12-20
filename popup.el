;;; popup.el --- Visual popup interface

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



;; Utilities

;; Borrowed from anything.el
(defmacro popup-aif (test-form then-form &rest else-forms)
  "Anaphoric if. Temporary variable `it' is the result of test-form."
  (declare (indent 2))
  `(let ((it ,test-form))
     (if it ,then-form ,@else-forms)))

(defun popup-x-to-string (x)
  "Convert any object to string effeciently.
This is faster than prin1-to-string in many cases."
  (typecase x
    (string x)
    (symbol (symbol-name x))
    (integer (number-to-string x))
    (float (number-to-string x))
    (t (format "%s" x))))

(defun popup-substring-by-width (string width)
  "Return cons of substring and remaining string by `WIDTH'."
  (loop with w = 0
        for l from 0
        for c in (append string nil)
        while (<= (incf w (char-width c)) width)
        finally return
        (if (< l (length string))
            (cons (substring string 0 l) (substring string l))
          (list string))))

(defun popup-fill-string (string &optional width max-width)
  (let (lines)
    (setq lines (split-string string "\n"))
    (unless width
      (setq width (loop for line in lines maximize (string-width line)))
      (if (and max-width
               (> width max-width))
          (setq width max-width)))
    (setq lines
          (apply 'append
                 (mapcar (lambda (line)
                           (loop while (stringp line)
                                 for result = (popup-substring-by-width line width)
                                 do (setq line (cdr result))
                                 collect (car result)))
                         lines)))
    (cons width lines)))

(defmacro popup-save-buffer-state (&rest body)
  (declare (indent 0))
  `(save-excursion
     (let ((buffer-undo-list t)
           (modified (buffer-modified-p)))
       (unwind-protect
           (progn ,@body)
         (set-buffer-modified-p modified)))))
  
(defun popup-preferred-width (list)
  "Return preferred width of popup to show `LIST' beautifully."
  (loop for item in list
        maximize (string-width (popup-x-to-string item)) into width
        finally return (* (ceiling (/ (or width 0) 10.0)) 10)))

(defun popup-current-physical-column ()
  (car (posn-col-row (posn-at-point))))

(defun popup-last-line-of-buffer-p ()
  (save-excursion (end-of-line) (/= (forward-line) 0)))

(defun popup-lookup-key-by-event (function event)
  (or (funcall function (vector event))
      (if (symbolp event)
          (popup-aif (get event 'event-symbol-element-mask)
              (funcall function (vector (logior (or (get (car it) 'ascii-character) 0)
                                                (cadr it))))))))



;; Popup common

(defgroup popup nil
  "Visual popup interface"
  :group 'lisp
  :prefix "popup-")

(defface popup-face
  '((t (:background "lightgray" :foreground "black")))
  "Face for popup."
  :group 'popup)

(defface popup-scroll-bar-foreground-face
  '((t (:background "black")))
  "Foreground face for scroll-bar."
  :group 'popup)

(defface popup-scroll-bar-background-face
  '((t (:background "gray")))
  "Background face for scroll-bar."
  :group 'popup)

(defvar popup-instances nil
  "Popup instances.")

(defvar popup-scroll-bar-foreground-char
  (propertize " " 'face 'popup-scroll-bar-foreground-face)
  "Foreground character for scroll-bar.")

(defvar popup-scroll-bar-background-char
  (propertize " " 'face 'popup-scroll-bar-background-face)
  "Background character for scroll-bar.")

(defstruct popup
  point row column width height min-height direction overlays
  parent depth
  face selection-face
  margin-left margin-right margin-left-cancel scroll-bar symbol
  cursor offset scroll-top list)

(defun popup-item-propertize (item &rest properties)
  (apply 'propertize
         (if (stringp item)
             item
           (popup-x-to-string item))
         properties))

(defun popup-item-property (item property)
  (if (stringp item)
      (get-text-property 0 property item)))

(defun* popup-make-item (name
                         &key
                         value
                         sublist
                         document
                         symbol)
  "Utility function to make popup item.
See also `popup-item-propertize'."
  (popup-item-propertize name
                         'value value
                         'document document
                         'symbol symbol
                         'sublist sublist))

(defsubst popup-item-value (item)               (popup-item-property item 'value))
(defsubst popup-item-value-or-self (item)       (or (popup-item-value item) item))
(defsubst popup-item-document (item)            (popup-item-property item 'document))
(defsubst popup-item-symbol (item)              (popup-item-property item 'symbol))
(defsubst popup-item-sublist (item)             (popup-item-property item 'sublist))

(defun popup-set-list (popup list)
  (setf (popup-list popup) list
        (popup-offset popup) (if (> (popup-direction popup) 0)
                                 0
                               (max (- (popup-height popup) (length list)) 0))))

(defun popup-selected-item (popup)
  (nth (popup-cursor popup) (popup-list popup)))

(defun popup-selected-line (popup)
  (- (popup-cursor popup) (popup-scroll-top popup)))

(defun popup-line-overlay (popup line)
  (aref (popup-overlays popup) line))

(defun popup-selected-line-overlay (popup)
  (popup-line-overlay popup (popup-selected-line popup)))

(defun popup-hide-line (popup line)
  (let ((overlay (popup-line-overlay popup line)))
    (overlay-put overlay 'display nil)
    (overlay-put overlay 'after-string nil)))

(defun popup-set-line-item (popup line item face margin-left margin-right scroll-bar-char symbol)
  (let* ((overlay (popup-line-overlay popup line))
         (content (propertize (concat margin-left
                                      (popup-create-line-string popup item)
                                      symbol
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

(defun popup-create-line-string (popup item)
  (let* ((string (car (popup-substring-by-width (popup-x-to-string item)
                                                (popup-width popup))))
         (string-width (string-width string))
         (popup-width (popup-width popup)))
    (if (< string-width popup-width)
        ;; Padding
        (concat string (make-string (- popup-width string-width) ? ))
      string)))

(defun popup-live-p (popup)
  (and popup (popup-overlays popup) t))

(defun* popup-create (point
                      width
                      height
                      &key
                      min-height
                      around
                      (face 'popup-face)
                      (selection-face face)
                      scroll-bar
                      margin-left
                      margin-right
                      symbol
                      parent
                      parent-offset)
  (or margin-left (setq margin-left 0))
  (or margin-right (setq margin-right 0))
  (unless point
    (setq point
          (if parent
              (overlay-end (popup-line-overlay parent
                                               (or parent-offset
                                                   (popup-selected-line parent))))
            (point))))

  (save-excursion
    (goto-char point)
    (let* ((row (line-number-at-pos))
           (column (popup-current-physical-column))
           (overlays (make-vector height nil))
           (popup-width (+ width
                           (if scroll-bar 1 0)
                           margin-left
                           margin-right
                           (if symbol 1 0)))
           margin-left-cancel
           (window (selected-window))
           (window-start (window-start))
           (window-hscroll (window-hscroll))
           (window-width (window-width))
           (right (+ column popup-width))
           (overflow (and (> right window-width)
                          (>= right popup-width)))
           (foldable (and (null parent)
                          (>= column popup-width)))
           (direction (or
                       ;; Currently the direction of cascade popup won't be changed
                       (and parent (popup-direction parent))

                       ;; Calculate direction
                       (if (and (> row height)
                                (> height (- (max 1 (- (window-height)
                                                       (if mode-line-format 1 0)
                                                       (if header-line-format 1 0)))
                                             (count-lines window-start (point)))))
                           -1
                         1)))
           (depth (if parent (1+ (popup-depth parent)) 0))
           current-column)
      ;; TODO no need to insert newlines
      (popup-save-buffer-state
        (goto-char (point-max))
        (insert (make-string height ?\n)))
      
      (if overflow
          (if foldable
              (progn
                (decf column (- popup-width margin-left margin-right))
                (unless around (move-to-column column)))
            (when (not truncate-lines)
              ;; Cut out overflow
              (let ((d (1+ (- popup-width (- window-width column)))))
                (decf popup-width d)
                (decf width d))))
        (when (and (null parent)
                   (< (decf column margin-left) 0))
          ;; Cancel margin left
          (setq column 0)
          (decf popup-width margin-left)
          (setq margin-left-cancel t)))
      
      (dotimes (i height)
        (let (overlay begin w (dangle t) (prefix "") (postfix ""))
          (when around
            (if (>= emacs-major-version 23)
                (vertical-motion (cons column direction))
              (vertical-motion direction)
              (move-to-column (+ (current-column) column))))
	  (setq around t
                current-column (popup-current-physical-column))

          (when (> current-column column)
            (backward-char)
            (setq current-column (popup-current-physical-column)))
          (when (< current-column column)
            ;; Extend short buffer lines by popup prefix (line of spaces)
            (setq prefix (make-string (+ (if (= current-column 0)
                                             (- window-hscroll (current-column))
                                           0)
                                         (- column current-column))
                                      ? )))
          
          (setq begin (point))
          (setq w (+ popup-width (length prefix)))
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
      (let ((it (make-popup :point point
                            :row row
                            :column column
                            :width width
                            :height height
                            :min-height min-height
                            :direction direction
                            :parent parent
                            :depth depth
                            :face face
                            :selection-face selection-face
                            :margin-left margin-left
                            :margin-right margin-right
                            :margin-left-cancel margin-left-cancel
                            :scroll-bar scroll-bar
                            :symbol symbol
                            :cursor 0
                            :scroll-top 0
                            :list nil
                            :overlays overlays)))
        (push it popup-instances)
        it))))

(defun popup-delete (popup)
  (when (popup-live-p popup)
    (popup-hide popup)
    (mapc 'delete-overlay (popup-overlays popup))
    (setf (popup-overlays popup) nil)
    (setq popup-instances (delq popup popup-instances))
    (popup-save-buffer-state
      (goto-char (point-max))
      (dotimes (i (popup-height popup))
        (if (= (char-before) ?\n)
            (delete-char -1)))))
  nil)

(defun popup-draw (popup)
  (loop with height = (popup-height popup)
        with min-height = (popup-min-height popup)
        with popup-face = (popup-face popup)
        with selection-face = (popup-selection-face popup)
        with list = (popup-list popup)
        with length = (length list)
        with thum-size = (max (/ (* height height) length) 1)
        with page-size = (/ (+ 0.0 length) height)
        with scroll-bar = (popup-scroll-bar popup)
        with margin-left = (make-string (if (popup-margin-left-cancel popup) 0 (popup-margin-left popup)) ? )
        with margin-right = (make-string (popup-margin-right popup) ? )
        with symbol = (popup-symbol popup)
        with cursor = (popup-cursor popup)
        with scroll-top = (popup-scroll-top popup)
        with offset = (popup-offset popup)
        for o from offset
        for i from scroll-top
        while (< o height)
        for item in (nthcdr scroll-top list)
        for page-index = (* thum-size (/ o thum-size))
        for face = (if (= i cursor)
                       (or (popup-item-property item 'selection-face) selection-face)
                     (or (popup-item-property item 'popup-face) popup-face))
        for empty-char = (propertize " " 'face face)
        for scroll-bar-char = (if scroll-bar
                                  (cond
                                   ((<= page-size 1)
                                    empty-char)
                                   ((and (> page-size 1)
                                         (>= cursor (* page-index page-size))
                                         (< cursor (* (+ page-index thum-size) page-size)))
                                    popup-scroll-bar-foreground-char)
                                   (t
                                    popup-scroll-bar-background-char))
                                "")
        for sym = (if symbol
                      (or (popup-item-symbol item) " ")
                    "")
        
        do
        ;; Show line and set item to the line
        (popup-set-line-item popup o item face margin-left margin-right scroll-bar-char sym)
        
        finally
        ;; Hide remaining lines
        (let ((scroll-bar-char (if scroll-bar (propertize " " 'face popup-face) ""))
              (symbol (if symbol " " "")))
          (if (> (popup-direction popup) 0)
              (progn
                (when min-height
                  (while (< o min-height)
                    (popup-set-line-item popup o "" popup-face margin-left margin-right scroll-bar-char symbol)
                    (incf o)))
                (while (< o height)
                  (popup-hide-line popup o)
                  (incf o)))
            (loop with h = (if min-height (- height min-height) offset)
                  for o from 0 below offset
                  if (< o h)
                  do (popup-hide-line popup o)
                  if (>= o h)
                  do (popup-set-line-item popup o "" popup-face margin-left margin-right scroll-bar-char symbol))))))

(defun popup-hide (popup)
  (dotimes (i (popup-height popup))
    (popup-hide-line popup i)))

(defun popup-next (popup)
  (let ((height (popup-height popup))
        (cursor (1+ (popup-cursor popup)))
        (scroll-top (popup-scroll-top popup))
        (length (length (popup-list popup))))
    (cond
     ((>= cursor length)
      ;; Back to first page
      (setq cursor 0
            scroll-top 0))
     ((= cursor (+ scroll-top height))
      ;; Go to next page
      (setq scroll-top (min (1+ scroll-top) (max (- length height) 0)))))
    (setf (popup-cursor popup) cursor
          (popup-scroll-top popup) scroll-top)
    (popup-draw popup)))

(defun popup-previous (popup)
  (let ((height (popup-height popup))
        (cursor (1- (popup-cursor popup)))
        (scroll-top (popup-scroll-top popup))
        (length (length (popup-list popup))))
    (cond
     ((< cursor 0)
      ;; Go to last page
      (setq cursor (1- length)
            scroll-top (max (- length height) 0)))
     ((= cursor (1- scroll-top))
      ;; Go to previous page
      (decf scroll-top)))
    (setf (popup-cursor popup) cursor
          (popup-scroll-top popup) scroll-top)
    (popup-draw popup)))

(defun popup-scroll-down (popup &optional n)
  (let ((scroll-top (min (+ (popup-scroll-top popup) (or n 1))
                         (- (length (popup-list popup)) (popup-height popup)))))
    (setf (popup-cursor popup) scroll-top
          (popup-scroll-top popup) scroll-top)
    (popup-draw popup)))

(defun popup-scroll-up (popup &optional n)
  (let ((scroll-top (max (- (popup-scroll-top popup) (or n 1))
                         0)))
    (setf (popup-cursor popup) scroll-top
          (popup-scroll-top popup) scroll-top)
    (popup-draw popup)))



;; Popup tip

(defface popup-tip-face
  '((t (:background "khaki1" :foreground "black")))
  "Face for popup tip."
  :group 'popup)

(defvar popup-tip-max-width 80)

(defun* popup-tip (string
                   &key
                   point
                   (around t)
                   width
                   (height 15)
                   min-height
                   truncate
                   margin
                   margin-left
                   margin-right
                   scroll-bar
                   parent
                   parent-offset
                   nowait
                   prompt
                   &aux tip lines)
  (if (bufferp string)
      (setq string (with-current-buffer string (buffer-string))))
  
  (and (eq margin t) (setq margin 1))
  (or margin-left (setq margin-left margin))
  (or margin-right (setq margin-right margin))
  
  (let ((it (popup-fill-string string width popup-tip-max-width)))
    (setq width (car it)
          lines (cdr it)))
  
  (setq tip (popup-create point width height
                          :min-height min-height
                          :around around
                          :margin-left margin-left
                          :margin-right margin-right
                          :scroll-bar scroll-bar
                          :face 'popup-tip-face
                          :parent parent
                          :parent-offset parent-offset))
  

  (when (and (not (eq width (popup-width tip))) ; truncated
             (not truncate))
    ;; Refill once again to lines be fitted to popup width
    (setq width (popup-width tip))
    (setq lines (cdr (popup-fill-string string width width))))

  (unwind-protect
      (progn
        (popup-set-list tip lines)
        (popup-draw tip)
        (if nowait
            tip
          (push (read-event prompt) unread-command-events)
          t))
    (unless nowait
      (popup-delete tip))))



;; Popup menu

(defface popup-menu-face
  '((t (:background "lightgray" :foreground "black")))
  "Face for popup menu."
  :group 'popup)

(defface popup-menu-selection-face
  '((t (:background "steelblue" :foreground "white")))
  "Face for popup menu selection."
  :group 'popup)

(defun popup-menu-show-help (menu &optional item &rest args)
  (or item (setq item (popup-selected-item menu)))
  (let ((height (or (plist-get args :height) (popup-height menu)))
        (doc (popup-item-document item)))
    (if (functionp doc)
        (setq doc (funcall doc item)))
    (when (stringp doc)
      (apply 'popup-tip
             doc
             :height height
             :min-height (min height (length (popup-list menu)))
             :around nil
             :parent menu
             :parent-offset 0
             args))))

(defun popup-menu-fallback (event default))

(defun* popup-menu-event-loop (menu keymap fallback &optional prompt help-delay &aux event binding)
  (block nil
    (while (popup-live-p menu)
      (setq event (progn (clear-this-command-keys) (read-event prompt nil help-delay)))
      (if (null event)
          (popup-menu-show-help menu)
        (setq binding (popup-lookup-key-by-event (lambda (key) (lookup-key keymap key)) event))
        (cond
         ((eq binding 'popup-close)
          (if (popup-parent menu)
              (return nil)))
         ((memq binding '(popup-select popup-open))
          (let* ((item (popup-selected-item menu))
                 (sublist (popup-item-sublist item)))
            (if sublist
                (popup-aif (popup-cascade-menu sublist
                                               :around nil
                                               :parent menu
                                               :margin-left (popup-margin-left menu)
                                               :margin-right (popup-margin-right menu)
                                               :scroll-bar (popup-scroll-bar menu))
                    (and it (return it)))
              (if (eq binding 'popup-select)
                  (return (popup-item-value-or-self item))))))
         ((eq binding 'popup-next)
          (popup-next menu))
         ((eq binding 'popup-previous)
          (popup-previous menu))
         (binding
          (call-interactively binding))
         (t
          (funcall fallback event (popup-lookup-key-by-event (lambda (key) (key-binding key)) event))))))))

(defun* popup-menu (list
                    &key
                    point
                    (around t)
                    (width (popup-preferred-width list))
                    (height 15)
                    margin
                    margin-left
                    margin-right
                    scroll-bar
                    symbol
                    parent
                    parent-offset
                    (keymap popup-menu-keymap)
                    (fallback 'popup-menu-fallback)
                    help-delay
                    prompt
                    &aux menu event)
  (and (eq margin t) (setq margin 1))
  (or margin-left (setq margin-left margin))
  (or margin-right (setq margin-right margin))
  (if (and scroll-bar
           (integerp margin-right)
           (> margin-right 0))
      ;; Make scroll-bar space as margin-right
      (decf margin-right))
  (setq menu (popup-create point width height
                           :around around
                           :face 'popup-menu-face
                           :selection-face 'popup-menu-selection-face
                           :margin-left margin-left
                           :margin-right margin-right
                           :scroll-bar scroll-bar
                           :symbol symbol
                           :parent parent))
  (unwind-protect
      (progn
        (popup-set-list menu list)
        (popup-draw menu)
        (popup-menu-event-loop menu keymap fallback prompt help-delay))
    (popup-delete menu)))

(defun popup-cascade-menu (list &rest args)
  "Same to `popup-menu', but an element of `LIST' can be
list of submenu."
  (apply 'popup-menu
         (mapcar (lambda (item)
                   (if (consp item)
                       (popup-make-item (car item)
                                        :sublist (cdr item)
                                        :symbol ">")
                     item))
                 list)
         :symbol t
         args))

(defvar popup-menu-keymap
  (let ((map (make-sparse-keymap)))
    ;; Dummy bind
    (define-key map "\r"        'popup-select)
    (define-key map "\C-f"      'popup-open)
    (define-key map [right]     'popup-open)
    (define-key map "\C-b"      'popup-close)
    (define-key map [left]      'popup-close)

    (define-key map "\C-n"      'popup-next)
    (define-key map [down]      'popup-next)
    (define-key map "\C-p"      'popup-previous)
    (define-key map [up]        'popup-previous)
    map))

(provide 'popup)
;;; popup.el ends here
