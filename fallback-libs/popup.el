;;; popup.el --- Visual popup interface

;; Copyright (C) 2009, 2010  Tomohiro Matsuyama

;; Author: Tomohiro Matsuyama <tomo@cx4a.org>
;; Keywords: lisp
;; Version: 0.4

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

(defvar popup-use-optimized-column-computation t
  "Use optimized column computation routine.
If there is a problem, please set it to nil.")

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
  ;; Expand tabs with 4 spaces
  (setq string (replace-regexp-in-string "\t" "    " string))
  (loop with len = (length string)
        with w = 0
        for l from 0
        for c in (append string nil)
        while (<= (incf w (char-width c)) width)
        finally return
        (if (< l len)
            (cons (substring string 0 l) (substring string l))
          (list string))))

(defun popup-fill-string (string &optional width max-width justify squeeze)
  "Split STRING into fixed width strings and return a cons cell like
\(WIDTH . ROWS). Here, the car WIDTH indicates the actual maxim width of ROWS.

The argument WIDTH specifies the width of filling each paragraph. WIDTH nil
means don't perform any justification and word wrap. Note that this function
doesn't add any padding characters at the end of each row.

MAX-WIDTH, if WIDTH is nil, specifies the maximum number of columns.

The optional fourth argument JUSTIFY specifies which kind of justification
to do: `full', `left', `right', `center', or `none' (equivalent to nil).
A value of t means handle each paragraph as specified by its text properties.

SQUEEZE nil means leave whitespaces other than line breaks untouched."
  (if (eq width 0)
      (error "Can't fill string with 0 width"))
  (if width
      (setq max-width width))
  (with-temp-buffer
    (let ((tab-width 4)
          (fill-column width)
          (left-margin 0)
          (kinsoku-limit 1)
          indent-tabs-mode
          row rows)
      (insert string)
      (untabify (point-min) (point-max))
      (if width
          (fill-region (point-min) (point-max) justify (not squeeze)))
      (goto-char (point-min))
      (setq width 0)
      (while (prog2
                 (let ((line (buffer-substring
                              (point) (progn (end-of-line) (point)))))
                   (if max-width
                       (while (progn
                                (setq row (truncate-string-to-width line max-width)
                                      width (max width (string-width row)))
                                (push row rows)
                                (if (not (= (length row) (length line)))
                                    (setq line (substring line (length row))))))
                     (setq width (max width (string-width line)))
                     (push line rows)))
                 (< (point) (point-max))
               (beginning-of-line 2)))
      (cons width (nreverse rows)))))

(defmacro popup-save-buffer-state (&rest body)
  (declare (indent 0))
  `(save-excursion
     (let ((buffer-undo-list t)
           (buffer-read-only nil)
           (modified (buffer-modified-p)))
       (unwind-protect
           (progn ,@body)
         (set-buffer-modified-p modified)))))
  
(defun popup-preferred-width (list)
  "Return preferred width of popup to show `LIST' beautifully."
  (loop with tab-width = 4
        for item in list
        for summary = (popup-item-summary item)
        maximize (string-width (popup-x-to-string item)) into width
        if (stringp summary)
        maximize (+ (string-width summary) 2) into summary-width
        finally return (* (ceiling (/ (+ (or width 0) (or summary-width 0)) 10.0)) 10)))

;; window-full-width-p is not defined in Emacs 22.1
(defun popup-window-full-width-p (&optional window)
  (if (fboundp 'window-full-width-p)
      (window-full-width-p window)
    (= (window-width window) (frame-width (window-frame (or window (selected-window)))))))

;; truncated-partial-width-window-p is not defined in Emacs 22
(defun popup-truncated-partial-width-window-p (&optional window)
  (unless window
    (setq window (selected-window)))
  (unless (popup-window-full-width-p window)
    (let ((t-p-w-w (buffer-local-value 'truncate-partial-width-windows
				       (window-buffer window))))
      (if (integerp t-p-w-w)
	  (< (window-width window) t-p-w-w)
	t-p-w-w))))

(defun popup-current-physical-column ()
  (or (when (and popup-use-optimized-column-computation
                 (eq (window-hscroll) 0))
        (let ((current-column (current-column)))
          (if (or (popup-truncated-partial-width-window-p)
                  truncate-lines
                  (< current-column (window-width)))
              current-column)))
      (car (posn-col-row (posn-at-point)))))

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
  cursor offset scroll-top current-height list newlines
  pattern original-list)

(defun popup-item-propertize (item &rest properties)
  "Same to `propertize` but this avoids overriding existed value with `nil` property."
  (let (props)
    (while properties
      (when (cadr properties)
        (push (car properties) props)
        (push (cadr properties) props))
      (setq properties (cddr properties)))
    (apply 'propertize
           (popup-x-to-string item)
           (nreverse props))))

(defun popup-item-property (item property)
  (if (stringp item)
      (get-text-property 0 property item)))

(defun* popup-make-item (name
                         &key
                         value
                         popup-face
                         selection-face
                         sublist
                         document
                         symbol
                         summary)
  "Utility function to make popup item.
See also `popup-item-propertize'."
  (popup-item-propertize name
                         'value value
                         'popup-face popup-face
                         'selection-face selection-face
                         'document document
                         'symbol symbol
                         'summary summary
                         'sublist sublist))

(defsubst popup-item-value (item)               (popup-item-property item 'value))
(defsubst popup-item-value-or-self (item)       (or (popup-item-value item) item))
(defsubst popup-item-popup-face (item)          (popup-item-property item 'popup-face))
(defsubst popup-item-selection-face (item)      (popup-item-property item 'selection-face))
(defsubst popup-item-document (item)            (popup-item-property item 'document))
(defsubst popup-item-summary (item)             (popup-item-property item 'summary))
(defsubst popup-item-symbol (item)              (popup-item-property item 'symbol))
(defsubst popup-item-sublist (item)             (popup-item-property item 'sublist))

(defun popup-item-documentation (item)
  (let ((doc (popup-item-document item)))
    (if (functionp doc)
        (setq doc (funcall doc (popup-item-value-or-self item))))
    doc))

(defun popup-item-show-help-1 (item)
  (let ((doc (popup-item-documentation item)))
    (when doc
      (with-current-buffer (get-buffer-create " *Popup Help*")
        (erase-buffer)
        (insert doc)
        (goto-char (point-min))
        (display-buffer (current-buffer)))
      t)))

(defun popup-item-show-help (item &optional persist)
  (when item
    (if (not persist)
        (save-window-excursion
          (when (popup-item-show-help-1 item)
            (block nil
              (while t
                (clear-this-command-keys)
                (let ((key (read-key-sequence-vector nil)))
                  (case (key-binding key)
                    ('scroll-other-window
                     (scroll-other-window))
                    ('scroll-other-window-down
                     (scroll-other-window-down nil))
                    (t
                     (setq unread-command-events (append key unread-command-events))
                     (return))))))))
      (popup-item-show-help-1 item))))

(defun popup-set-list (popup list)
  (popup-set-filtered-list popup list)
  (setf (popup-pattern popup) nil)
  (setf (popup-original-list popup) list))
  
(defun popup-set-filtered-list (popup list)
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

(defun popup-line-hidden-p (popup line)
  (let ((overlay (popup-line-overlay popup line)))
    (and (eq (overlay-get overlay 'display) nil)
         (eq (overlay-get overlay 'after-string) nil))))

(defun popup-set-line-item (popup line item face margin-left margin-right scroll-bar-char symbol summary)
  (let* ((overlay (popup-line-overlay popup line))
         (content (popup-create-line-string popup (popup-x-to-string item) margin-left margin-right symbol summary))
         (start 0)
         (prefix (overlay-get overlay 'prefix))
         (postfix (overlay-get overlay 'postfix))
         end)
    ;; Overlap face properties
    (if (get-text-property start 'face content)
        (setq start (next-single-property-change start 'face content)))
    (while (and start (setq end (next-single-property-change start 'face content)))
      (put-text-property start end 'face face content)
      (setq start (next-single-property-change end 'face content)))
    (if start
        (put-text-property start (length content) 'face face content))
    (unless (overlay-get overlay 'dangle)
      (overlay-put overlay 'display (concat prefix (substring content 0 1)))
      (setq prefix nil
            content (concat (substring content 1))))
    (overlay-put overlay
                 'after-string
                 (concat prefix
                         content
                         scroll-bar-char
                         postfix))))

(defun popup-create-line-string (popup string margin-left margin-right symbol summary)
  (let* ((popup-width (popup-width popup))
         (summary-width (string-width summary))
         (string (car (popup-substring-by-width string
                                                (- popup-width
                                                   (if (> summary-width 0)
                                                       (+ summary-width 2)
                                                     0)))))
         (string-width (string-width string)))
    (concat margin-left
            string
            (make-string (max (- popup-width string-width summary-width) 0) ? )
            summary
            symbol
            margin-right)))

(defun popup-live-p (popup)
  (and popup (popup-overlays popup) t))

(defun popup-child-point (popup &optional offset)
  (overlay-end (popup-line-overlay popup
                                   (or offset
                                       (popup-selected-line popup)))))

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
          (if parent (popup-child-point parent parent-offset) (point))))

  (save-excursion
    (goto-char point)
    (let* ((row (line-number-at-pos))
           (column (popup-current-physical-column))
           (overlays (make-vector height nil))
           (popup-width (+ width
                           (if scroll-bar 1 0)
                           margin-left
                           margin-right
                           (if symbol 2 0)))
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
           (newlines (max 0 (+ (- height (count-lines point (point-max))) (if around 1 0))))
           current-column)
      (when (> newlines 0)
        (popup-save-buffer-state
          (goto-char (point-max))
          (insert (make-string newlines ?\n))))
      
      (if overflow
          (if foldable
              (progn
                (decf column (- popup-width margin-left margin-right))
                (unless around (move-to-column column)))
            (when (not truncate-lines)
              ;; Cut out overflow
              (let ((d (1+ (- popup-width (- window-width column)))))
                (decf popup-width d)
                (decf width d)))
            (decf column margin-left))
        (decf column margin-left))
      (when (and (null parent)
                 (< column 0))
          ;; Cancel margin left
        (setq column 0)
        (decf popup-width margin-left)
        (setq margin-left-cancel t))
      
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
                            :current-height 0
                            :list nil
                            :newlines newlines
                            :overlays overlays)))
        (push it popup-instances)
        it))))

(defun popup-delete (popup)
  (when (popup-live-p popup)
    (popup-hide popup)
    (mapc 'delete-overlay (popup-overlays popup))
    (setf (popup-overlays popup) nil)
    (setq popup-instances (delq popup popup-instances))
    (let ((newlines (popup-newlines popup)))
      (when (> newlines 0)
        (popup-save-buffer-state
          (goto-char (point-max))
          (dotimes (i newlines)
            (if (= (char-before) ?\n)
                (delete-char -1)))))))
  nil)

(defun popup-draw (popup)
  (loop with height = (popup-height popup)
        with min-height = (popup-min-height popup)
        with popup-face = (popup-face popup)
        with selection-face = (popup-selection-face popup)
        with list = (popup-list popup)
        with length = (length list)
        with thum-size = (max (/ (* height height) (max length 1)) 1)
        with page-size = (/ (+ 0.0 (max length 1)) height)
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
                       (or (popup-item-selection-face item) selection-face)
                     (or (popup-item-popup-face item) popup-face))
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
                      (concat " " (or (popup-item-symbol item) " "))
                    "")
        for summary = (or (popup-item-summary item) "")
        
        do
        ;; Show line and set item to the line
        (popup-set-line-item popup o item face margin-left margin-right scroll-bar-char sym summary)
        
        finally
        ;; Remember current height
        (setf (popup-current-height popup) (- o offset))

        ;; Hide remaining lines
        (let ((scroll-bar-char (if scroll-bar (propertize " " 'face popup-face) ""))
              (symbol (if symbol " " "")))
          (if (> (popup-direction popup) 0)
              (progn
                (when min-height
                  (while (< o min-height)
                    (popup-set-line-item popup o "" popup-face margin-left margin-right scroll-bar-char symbol "")
                    (incf o)))
                (while (< o height)
                  (popup-hide-line popup o)
                  (incf o)))
            (loop with h = (if min-height (- height min-height) offset)
                  for o from 0 below offset
                  if (< o h)
                  do (popup-hide-line popup o)
                  if (>= o h)
                  do (popup-set-line-item popup o "" popup-face margin-left margin-right scroll-bar-char symbol ""))))))

(defun popup-hide (popup)
  (dotimes (i (popup-height popup))
    (popup-hide-line popup i)))

(defun popup-hidden-p (popup)
  (let ((hidden t))
    (when (popup-live-p popup)
      (dotimes (i (popup-height popup))
        (unless (popup-line-hidden-p popup i)
          (setq hidden nil))))
    hidden))

(defun popup-select (popup i)
  (setq i (+ i (popup-offset popup)))
  (when (and (<= 0 i) (< i (popup-height popup)))
    (setf (popup-cursor popup) i)
    (popup-draw popup)
    t))

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



;; Popup isearch

(defface popup-isearch-match
  '((t (:background "sky blue")))
  "Popup isearch match face."
  :group 'popup)

(defvar popup-isearch-cursor-color "blue")

(defvar popup-isearch-keymap
  (let ((map (make-sparse-keymap)))
    ;(define-key map "\r"        'popup-isearch-done)
    (define-key map "\C-g"      'popup-isearch-cancel)
    (define-key map "\C-h"      'popup-isearch-delete)
    (define-key map (kbd "DEL") 'popup-isearch-delete)
    map))

(defsubst popup-isearch-char-p (char)
  (and (integerp char)
       (<= 32 char)
       (<= char 126)))

(defun popup-isearch-filter-list (pattern list)
  (loop with regexp = (regexp-quote pattern)
        for item in list
        do
        (unless (stringp item)
          (setq item (popup-item-propertize (popup-x-to-string item)
                                            'value item)))
        if (string-match regexp item)
        collect (let ((beg (match-beginning 0))
                      (end (match-end 0)))
                  (alter-text-property 0 (length item) 'face
                                       (lambda (prop)
                                         (unless (eq prop 'popup-isearch-match)
                                           prop))
                                       item)
                  (put-text-property beg end
                                     'face 'popup-isearch-match
                                     item)
                  item)))

(defun popup-isearch-prompt (popup pattern)
  (format "Pattern: %s" (if (= (length (popup-list popup)) 0)
                            (propertize pattern 'face 'isearch-fail)
                          pattern)))

(defun popup-isearch-update (popup pattern &optional callback)
  (setf (popup-cursor popup) 0
        (popup-scroll-top popup) 0
        (popup-pattern popup) pattern)
  (let ((list (popup-isearch-filter-list pattern (popup-original-list popup))))
    (popup-set-filtered-list popup list)
    (if callback
        (funcall callback list)))
  (popup-draw popup))

(defun* popup-isearch (popup
                       &key
                       (cursor-color popup-isearch-cursor-color)
                       (keymap popup-isearch-keymap)
                       callback
                       help-delay)
  (let ((list (popup-original-list popup))
        (pattern (or (popup-pattern popup) ""))
        (old-cursor-color (frame-parameter (selected-frame) 'cursor-color))
        prompt key binding done)
    (unwind-protect
        (unless (block nil
                  (if cursor-color
                      (set-cursor-color cursor-color))
                  (while t
                    (setq prompt (popup-isearch-prompt popup pattern))
                    (setq key (popup-menu-read-key-sequence keymap prompt help-delay))
                    (if (null key)
                        (unless (funcall popup-menu-show-quick-help-function popup nil :prompt prompt)
                          (clear-this-command-keys)
                          (push (read-event prompt) unread-command-events))
                      (setq binding (lookup-key keymap key))
                      (cond
                       ((and (stringp key)
                             (popup-isearch-char-p (aref key 0)))
                        (setq pattern (concat pattern key)))
                       ((eq binding 'popup-isearch-done)
                        (return t))
                       ((eq binding 'popup-isearch-cancel)
                        (return nil))
                       ((eq binding 'popup-isearch-delete)
                        (if (> (length pattern) 0)
                            (setq pattern (substring pattern 0 (1- (length pattern))))))
                       (t
                        (setq unread-command-events
                              (append (listify-key-sequence key) unread-command-events))
                        (return t)))
                      (popup-isearch-update popup pattern callback))))
          (popup-isearch-update popup "" callback)
          t) ; Return non-nil if isearch is cancelled
      (if old-cursor-color
          (set-cursor-color old-cursor-color)))))



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
  ;; TODO strip text (mainly face) properties
  (setq string (substring-no-properties string))

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

  (unwind-protect
      (when (> (popup-width tip) 0)                   ; not to be corrupted
        (when (and (not (eq width (popup-width tip))) ; truncated
                   (not truncate))
          ;; Refill once again to lines be fitted to popup width
          (setq width (popup-width tip))
          (setq lines (cdr (popup-fill-string string width width))))

        (popup-set-list tip lines)
        (popup-draw tip)
        (if nowait
            tip
          (clear-this-command-keys)
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

(defvar popup-menu-show-tip-function 'popup-tip
  "Function used for showing tooltip by `popup-menu-show-quick-help'.")

(defvar popup-menu-show-quick-help-function 'popup-menu-show-quick-help
  "Function used for showing quick help by `popup-menu*'.")

(defun popup-menu-show-help (menu &optional persist item)
  (popup-item-show-help (or item (popup-selected-item menu)) persist))

(defun popup-menu-documentation (menu &optional item)
  (popup-item-documentation (or item (popup-selected-item menu))))

(defun popup-menu-show-quick-help (menu &optional item &rest args)
  (let* ((point (plist-get args :point))
         (height (or (plist-get args :height) (popup-height menu)))
         (min-height (min height (popup-current-height menu)))
         (around nil)
         (parent-offset (popup-offset menu))
         (doc (popup-menu-documentation menu item)))
    (when (stringp doc)
      (if (popup-hidden-p menu)
          (setq around t
                menu nil
                parent-offset nil)
        (setq point nil))
      (let ((popup-use-optimized-column-computation nil)) ; To avoid wrong positioning
        (apply popup-menu-show-tip-function
               doc
               :point point
               :height height
               :min-height min-height
               :around around
               :parent menu
               :parent-offset parent-offset
               args)))))

(defun popup-menu-read-key-sequence (keymap &optional prompt timeout)
  (catch 'timeout
    (let ((timer (and timeout
                      (run-with-timer timeout nil
                                      (lambda ()
                                        (if (zerop (length (this-command-keys)))
                                            (throw 'timeout nil))))))
          (old-global-map (current-global-map))
          (temp-global-map (make-sparse-keymap))
          (overriding-terminal-local-map (make-sparse-keymap)))
      (substitute-key-definition 'keyboard-quit 'keyboard-quit
                                 temp-global-map old-global-map)
      (define-key temp-global-map [menu-bar] (lookup-key old-global-map [menu-bar]))
      (define-key temp-global-map [tool-bar] (lookup-key old-global-map [tool-bar]))
      (set-keymap-parent overriding-terminal-local-map keymap)
      (if (current-local-map)
          (define-key overriding-terminal-local-map [menu-bar]
            (lookup-key (current-local-map) [menu-bar])))
      (unwind-protect
          (progn
            (use-global-map temp-global-map)
            (clear-this-command-keys)
            (with-temp-message prompt
              (read-key-sequence nil)))
        (use-global-map old-global-map)
        (if timer (cancel-timer timer))))))

(defun popup-menu-fallback (event default))

(defun* popup-menu-event-loop (menu keymap fallback &optional prompt help-delay isearch isearch-cursor-color isearch-keymap isearch-callback &aux key binding)
  (block nil
    (while (popup-live-p menu)
      (and isearch
           (popup-isearch menu
                          :cursor-color isearch-cursor-color
                          :keymap isearch-keymap
                          :callback isearch-callback
                          :help-delay help-delay)
           (keyboard-quit))
      (setq key (popup-menu-read-key-sequence keymap prompt help-delay))
      (if (null key)
          (unless (funcall popup-menu-show-quick-help-function menu nil :prompt prompt)
            (clear-this-command-keys)
            (push (read-event prompt) unread-command-events))
        (if (eq (lookup-key (current-global-map) key) 'keyboard-quit)
            (keyboard-quit))
        (setq binding (lookup-key keymap key))
        (cond
         ((eq binding 'popup-close)
          (if (popup-parent menu)
              (return)))
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
         ((eq binding 'popup-help)
          (popup-menu-show-help menu))
         ((eq binding 'popup-isearch)
          (popup-isearch menu
                         :cursor-color isearch-cursor-color
                         :keymap isearch-keymap
                         :callback isearch-callback
                         :help-delay help-delay))
         ((commandp binding)
          (call-interactively binding))
         (t
          (funcall fallback key (key-binding key))))))))

;; popup-menu is used by mouse.el unfairly...
(defun* popup-menu* (list
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
                     isearch
                     (isearch-cursor-color popup-isearch-cursor-color)
                     (isearch-keymap popup-isearch-keymap)
                     isearch-callback
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
        (popup-menu-event-loop menu keymap fallback prompt help-delay isearch
                               isearch-cursor-color isearch-keymap isearch-callback))
    (popup-delete menu)))

(defun popup-cascade-menu (list &rest args)
  "Same to `popup-menu', but an element of `LIST' can be
list of submenu."
  (apply 'popup-menu*
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
    (define-key map "\r"        'popup-select)
    (define-key map "\C-f"      'popup-open)
    (define-key map [right]     'popup-open)
    (define-key map "\C-b"      'popup-close)
    (define-key map [left]      'popup-close)

    (define-key map "\C-n"      'popup-next)
    (define-key map [down]      'popup-next)
    (define-key map "\C-p"      'popup-previous)
    (define-key map [up]        'popup-previous)

    (define-key map [f1]        'popup-help)
    (define-key map (kbd "\C-?") 'popup-help)

    (define-key map "\C-s"      'popup-isearch)
    map))

(provide 'popup)
;;; popup.el ends here
