;;; auto-complete.el --- Auto completion

;; Copyright (C) 2008, 2009  Tomohiro Matsuyama

;; Author: Tomohiro Matsuyama <m2ym.pub@gmail.com>
;; URL: http://github.com/m2ym/auto-complete
;; Keywords: convenience
;; Version: 1.1a

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
;; This extension provides a way to complete with popup menu like:
;;
;;     def-!-
;;     +-----------------+
;;     |defun::::::::::::|
;;     |defvar           |
;;     |defmacro         |
;;     |       ...       |
;;     +-----------------+
;;
;; You can complete by typing and selecting menu.
;; Enjoy!

;;; Qualification:
;;
;; This extension can work properly on GNU Emacs 22 or higher.

;;; Installation:
;;
;; To use this extension, compile necessary elisp files and locate them to your load-path directory.
;;
;;     $ emacs -L . -batch -f batch-byte-compile *.el
;;     $ cp *.el *.elc ~/.emacs.d/
;;
;; And write following code into your .emacs.
;;
;;     (require 'auto-complete)
;;     (require 'auto-complete-config)
;;     (global-auto-complete-mode t)

;;; Sample configuration:
;;
;; Here is my configuration. It is useful for many people.
;;
;;     (setq-default ac-sources '(ac-source-words-in-same-mode-buffers))
;;     (add-hook 'emacs-lisp-mode-hook (lambda () (add-to-list 'ac-sources 'ac-source-symbols)))
;;     (add-hook 'auto-complete-mode-hook (lambda () (add-to-list 'ac-sources 'ac-source-filename)))
;;     (global-auto-complete-mode t)
;;     (set-face-background 'ac-candidate-face "lightgray")
;;     (set-face-underline 'ac-candidate-face "darkgray")
;;     (set-face-background 'ac-selection-face "steelblue")
;;     (define-key ac-completing-map "\M-n" 'ac-next)
;;     (define-key ac-completing-map "\M-p" 'ac-previous)
;;     (setq ac-auto-start 2)
;;     (setq ac-dwim t)
;;     (define-key ac-mode-map (kbd "M-TAB") 'auto-complete)

;;; Tips:
;;
;; Use C-n/C-p to select candidates
;; --------------------------------
;;
;; Add following code to your .emacs.
;; 
;;     (define-key ac-completing-map "\C-n" 'ac-next)
;;     (define-key ac-completing-map "\C-p" 'ac-previous)
;;
;;
;; Don't start completion automatically
;; ------------------------------------
;;
;; Add following code to your .emacs.
;;
;;     (setq ac-auto-start nil)
;;     (global-set-key "\M-/" 'auto-complete)
;;
;; or
;;
;;     ;; start completion when entered 3 characters
;;     (setq ac-auto-start 3)
;;
;;
;; Use trigger key
;; ---------------
;;
;; You can use common key as auto-complete trigger.
;; Add following code to your .emacs.
;;
;;     (ac-set-trigger-key "TAB")
;;
;; Now you can use TAB as auto-complete trigger.
;; It is enabled only when
;; a. After insertion/deletion command
;; b. With prefix (C-u TAB)
;;
;;
;; Use M-TAB for completion
;; ------------------------
;;
;; Add following code to your .emacs.
;;
;;     (define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
;;
;;
;; Stop completion
;; ---------------
;;
;; Add following code to your .emacs.
;;
;;     (define-key ac-completing-map "\M-/" 'ac-stop)
;;
;; Now you can stop completion by pressing M-/.
;;
;;
;; Completion by TAB
;; -----------------
;;
;; Add following code to your .emacs.
;;
;;     (define-key ac-completing-map "\t" 'ac-complete)
;;     (define-key ac-completing-map "\r" nil)
;;
;;
;; Do What I Mean mode
;; -------------------
;;
;; If DWIM (Do What I Mean) mode is enabled,
;; the following features is available:
;;
;; a. TAB (ac-expand) behave as completion (ac-complete)
;;    when only one candidate is left
;; b. TAB (ac-expand) behave as completion (ac-complete)
;;    after you select candidate
;; c. Disapear automatically when you
;;    complete a candidate.
;;
;; DWIM mode is enabled by default.
;; You can enable this feature by
;; setting `ac-dwim' to t.
;;
;;     (setq ac-dwim t)
;;
;;
;; Change default sources
;; ----------------------
;;
;;     (setq-default ac-sources '(ac-source-abbrev ac-source-words-in-buffer))
;;
;;
;; Change sources for particular mode
;; ----------------------------------
;;
;;     (add-hook 'emacs-lisp-mode-hook
;;                 (lambda ()
;;                   (setq ac-sources '(ac-source-words-in-buffer ac-source-symbols))))

;;; Code:



(eval-when-compile
  (require 'cl))

(require 'popup)

(defgroup auto-complete nil
  "Auto completion."
  :group 'convenience
  :prefix "ac-")

(defcustom ac-delay 0.1
  "Delay to show menu."
  :type 'float
  :group 'auto-complete)

(defcustom ac-quick-help-delay 1.0
  "Delay to show quick help."
  :type 'float
  :group 'auto-complete)

(defcustom ac-menu-height 10
  "Max height of candidate menu."
  :type 'integer
  :group 'auto-complete)
(defvaralias 'ac-candidate-menu-height 'ac-menu-height)

(defcustom ac-quick-help-height 20
  "Max height of quick help"
  :type 'integer
  :group 'auto-complete)

(defcustom ac-candidate-limit 10
  "Limit number of candidates."
  :type 'integer
  :group 'auto-complete)
(defvaralias 'ac-candidate-max 'ac-candidate-limit)

(defcustom ac-modes
  '(emacs-lisp-mode
    lisp-interaction-mode
    c-mode cc-mode c++-mode java-mode
    perl-mode cperl-mode python-mode ruby-mode
    ecmascript-mode javascript-mode js2-mode php-mode css-mode
    makefile-mode sh-mode fortran-mode f90-mode ada-mode
    xml-mode sgml-mode)
  "Major modes `auto-complete-mode' can run on."
  :type '(repeat symbol)
  :group 'auto-complete)

(defcustom ac-compatible-packages-regexp
  "^ac-"
  "Regexp to indicate what packages can work with auto-complete."
  :type 'string
  :group 'auto-complete)

(defcustom ac-trigger-commands
  '(self-insert-command)
  "Trigger commands that specify whether `auto-complete' should start or not."
  :type '(repeat symbol)
  :group 'auto-complete)

(defcustom ac-trigger-commands-on-completing
  '(delete-backward-char
    backward-delete-char
    backward-delete-char-untabify)
  "Trigger commands that specify whether `auto-complete' should continue or not."
  :type '(repeat symbol)
  :group 'auto-complete)

(defcustom ac-trigger-key nil
  "Non-nil means `auto-complete' will start by typing this key.
If you specify this TAB, for example, `auto-complete' will start by typing TAB,
and if there is no completions, an original command will be fallbacked."
  :type 'string
  :group 'auto-complete
  :set (lambda (symbol value)
         (set-default symbol value)
         (when (and value
                    (fboundp 'ac-set-trigger-key))
           (ac-set-trigger-key value))))

(defcustom ac-auto-start t
  "Non-nil means completion will be started automatically.
Positive integer means if a length of a word you entered is larger than the value,
completion will be started automatically.
If you specify `nil', never be started automatically."
  :type '(choice (const :tag "Yes" t)
                 (const :tag "Never" nil)
                 (integer :tag "Require"))
  :group 'auto-complete)

(defcustom ac-ignore-case nil
  "Non-nil means auto-complete ignores case.
If this value is `smart', auto-complete ignores case only when
a prefix doen't contain any upper case letters."
  :type '(choice (const :tag "Yes" t)
                 (const :tag "Smart" smart)
                 (const :tag "No" nil))
  :group 'auto-complete)

(defcustom ac-common-part-threshold 1
  "Threshold whether the larger length of string will be common-part."
  :type 'integer
  :group 'auto-complete)

(defcustom ac-dwim t
  "Non-nil means `auto-complete' works based on Do What I Mean."
  :type 'boolean
  :group 'auto-complete)

(defface ac-completion-face
  '((t (:background "darkblue" :foreground "white")))
  "Face for inline completion"
  :group 'auto-complete)

(defface ac-candidate-face
  '((t (:background "lightgray" :foreground "black")))
  "Face for candidate."
  :group 'auto-complete)

(defface ac-selection-face
  '((t (:background "blue" :foreground "white")))
  "Face for selected candidate."
  :group 'auto-complete)

(defvar auto-complete-mode-hook nil
  "Hook for `auto-complete-mode'.")



;; Internal variables

(defvar auto-complete-mode nil
  "Dummy variable to suppress compiler warnings.")

(defvar ac-inline nil
  "Inline completion instance.")

(defvar ac-menu nil
  "Menu instance.")

(defvar ac-quick-help nil
  "Quick help instance")

(defvar ac-completing nil
  "Non-nil means `auto-complete-mode' is now working on completion.")

(defvar ac-buffer nil
  "Buffer where auto-complete is started.")

(defvar ac-point nil
  "Start point of prefix.")

(defvar ac-prefix nil
  "Prefix string.")
(defvaralias 'ac-target 'ac-prefix)

(defvar ac-common-part nil
  "Common part string of candidates.
If there is no common part, this will be nil.")

(defvar ac-prefix-overlay nil
  "Overlay for prefix string.")

(defvar ac-timer nil
  "Menu idle timer.")

(defvar ac-quick-help-timer nil
  "Quick help idle timer.")

(defvar ac-triggered nil
  "Flag to update.")

(defvar ac-limit 0
  "Limit number of candidates for each sources.")

(defvar ac-candidates nil
  "Current candidates.")

(defvar ac-candidates-cache nil
  "Candidates cache for individual sources.")

(defvar ac-dwim-enable nil
  "Non-nil means DWIM completion will be allowed.")

(defvar ac-mode-map (make-sparse-keymap)
  "Auto-complete mode map. It is also used for trigger key command. See also `ac-trigger-key'.")

(defvar ac-completing-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\t" 'ac-expand)
    (define-key map "\r" 'ac-complete)
    
    (define-key map [down] 'ac-next)
    (define-key map [up] 'ac-previous)

    (define-key map [C-down] 'ac-quick-help-scroll-down)
    (define-key map [C-up] 'ac-quick-help-scroll-up)

    map)
  "Keymap for completion")
(defvaralias 'ac-complete-mode-map 'ac-completing-map)

(defvar ac-prefix-definitions
  '((symbol . ac-prefix-symbol)
    (file . ac-prefix-file)
    (valid-file . ac-prefix-valid-file)
    (c-dot . ac-prefix-c-dot))
  "Prefix definitions for common use.")

(defvar ac-sources '(ac-source-words-in-same-mode-buffers)
  "Sources for completion.

Source takes a form of just function which returns candidates or alist:

init INIT-FUNC
  INIT-FUNC will be called before creating candidate every time.

candidates CANDIDATE-FUNC
  CANDIDATE-FUNC will return a list of string as candidates.
CANDIDATE-FUNC should care about `ac-limit' that is specified at limit for performance.

action ACTION-FUNC
  ACTION-FUNC will be called when `ac-complete' is called.

limit LIMIT-NUM
  A limit of candidates.

requires REQUIRES-NUM
  This source will be included when `ac-prefix' length is larger than REQUIRES-NUM.")
(make-variable-buffer-local 'ac-sources)

(defvar ac-compiled-sources nil
  "Compiled source of `ac-sources'.")

(defvar ac-current-sources nil
  "Current working sources. This is sublist of `ac-compiled-sources'.")

(defvar ac-omni-completion-sources nil
  "Do not use this anymore.")



;; Auto completion internals

(defun ac-error (&optional var)
  "Report an error and disable `auto-complete-mode'."
  (ignore-errors
    (message "auto-complete error: %s" var)
    (auto-complete-mode -1)
    var))

(defun ac-menu-at-wrapper-line-p ()
  "Return non-nil if current line is long and wrapped to next visual line."
  (and (not truncate-lines)
       (eq (line-beginning-position)
           (save-excursion
             (vertical-motion 1)
             (line-beginning-position)))))

(defun ac-prefix-symbol ()
  "Default prefix definition function."
  (require 'thingatpt)
  (car-safe (bounds-of-thing-at-point 'symbol)))
(defalias 'ac-prefix-default 'ac-prefix-symbol)

(defun ac-prefix-file ()
  "File prefix."
  (let ((point (re-search-backward "[\"<>' \t\r\n]" nil t)))
    (if point (1+ point))))

(defun ac-prefix-valid-file ()
  "Existed (or to be existed) file prefix."
  (let* ((line-beg (line-beginning-position))
         (end (point))
         (start (or (let ((point (re-search-backward "[\"<>' \t\r\n]" line-beg t)))
                      (if point (1+ point)))
                    line-beg))
         (file (buffer-substring start end)))
    (if (and file (or (string-match "^/" file)
                      (and (setq file (and (string-match "^[^/]*/" file)
                                           (match-string 0 file)))
                           (file-directory-p file))))
        start)))

(defun ac-prefix-c-dot ()
  "C-like languages dot(.) prefix."
  (let ((point (re-search-backward "\\.\\([a-zA-Z0-9][_a-zA-Z0-9]*\\)?\\=" nil t)))
    (if point (1+ point))))

(defun ac-define-prefix (name prefix)
  "Define new prefix definition.
You can not use it in source definition like (prefix . `NAME')."
  (push (cons name prefix) ac-prefix-definitions))

(defun ac-match-substring (prefix candidates)
  (loop with regexp = (regexp-quote prefix)
        for candidate in candidates
        if (string-match regexp candidate)
        collect candidate))

(defun ac-compile-sources (sources)
  "Compiled `SOURCES' into expanded sources style."
  (loop for source in sources
        if (symbolp source) do (setq source (symbol-value source))
        do
        (flet ((add-attribute (name value &optional append) (add-to-list 'source (cons name value) append)))
          ;; prefix
          (let* ((prefix (assoc 'prefix source))
                 (real (assoc-default (cdr prefix) ac-prefix-definitions)))
            (cond
             (real
              (add-attribute 'prefix real))
             ((null prefix)
              (add-attribute 'prefix 'ac-prefix-default)
              (add-attribute 'requires 1 t))))
          ;; match
          (let ((match (assq 'match source)))
            (cond
             ((eq (cdr match) 'substring)
              (setcdr match 'ac-match-substring))
             ((null match)
              (add-attribute 'match 'all-completions)))))
        collect source))

(defun ac-compiled-sources ()
  (or ac-compiled-sources
      (setq ac-compiled-sources
            (ac-compile-sources ac-sources))))

(defsubst ac-menu-live-p ()
  (popup-live-p ac-menu))

(defun ac-menu-create (point width height)
  (setq ac-menu
        (popup-create point width height
                      :around t
                      :face 'ac-candidate-face
                      :selection-face 'ac-selection-face
                      :scroll-bar t
                      :margin-left 1)))

(defun ac-menu-delete ()
  (when ac-menu
    (popup-delete ac-menu)
    (setq ac-menu)))

(defsubst ac-inline-marker ()
  (nth 0 ac-inline))

(defsubst ac-inline-overlay ()
  (nth 1 ac-inline))

(defsubst ac-inline-live-p ()
  (and ac-inline (ac-inline-overlay) t))

(defun ac-inline-show (point string)
  (unless ac-inline
    (setq ac-inline (list (make-marker) nil)))
  (save-excursion
    (let ((overlay (ac-inline-overlay))
          (width 0)
          (string-width (string-width string))
          (original-string string))
      ;; Calculate string space to show completion
      (goto-char point)
      (while (and (not (eolp))
                  (< width string-width))
        (incf width (char-width (char-after)))
        (forward-char))

      ;; Show completion
      (goto-char point)
      (cond
       ((= width 0)
        (set-marker (ac-inline-marker) point)
        (let ((buffer-undo-list t))
          (insert " "))
        (setq width 1))
       ((<= width string-width)
        ;; No space to show
        ;; Do nothing
        )
       ((> width string-width)
        ;; Need to fill space
        (setq string (concat string (make-string (- width string-width) ? )))))
      (setq string (propertize string 'face 'ac-completion-face))
      (if overlay
          (progn
            (move-overlay overlay point (+ point width))
            (overlay-put overlay 'invisible nil))
        (setq overlay (make-overlay point (+ point width)))
        (setf (nth 1 ac-inline)  overlay)
        (overlay-put overlay 'priority 9999))
      (overlay-put overlay 'display (substring string 0 1))
      ;; TODO no width but char
      (overlay-put overlay 'after-string (substring string 1))
      (overlay-put overlay 'string original-string))))

(defun ac-inline-delete ()
  (when (ac-inline-live-p)
    (ac-inline-hide)
    (delete-overlay (ac-inline-overlay))
    (setq ac-inline nil)))

(defun ac-inline-hide ()
  (when (ac-inline-live-p)
    (let ((overlay (ac-inline-overlay))
          (marker (ac-inline-marker))
          (buffer-undo-list t))
      (when overlay
        (when (marker-position marker)
          (save-excursion
            (goto-char marker)
            (delete-char 1)
            (set-marker marker nil)))
        (move-overlay overlay (point-min) (point-min))
        (overlay-put overlay 'invisible t)
        (overlay-put overlay 'display nil)
        (overlay-put overlay 'after-string nil)))))

(defun ac-inline-update ()
  (when (and ac-completing ac-prefix
             (stringp (setq ac-common-part (try-completion ac-prefix ac-candidates))))
    (let ((common-part-length (length ac-common-part))
          (prefix-length (length ac-prefix)))
      (if (and (> common-part-length prefix-length)
               (> (- common-part-length prefix-length) ac-common-part-threshold))
          (progn
            (ac-inline-hide)
            (ac-inline-show (point) (substring ac-common-part prefix-length)))
        (ac-inline-delete)))))

(defun ac-put-prefix-overlay ()
  (unless ac-prefix-overlay
    (setq ac-prefix-overlay (make-overlay ac-point (1+ (point)) nil t t))
    (overlay-put ac-prefix-overlay 'priority 9999)
    (overlay-put ac-prefix-overlay 'keymap (make-sparse-keymap))))

(defun ac-remove-prefix-overlay ()
  (when ac-prefix-overlay
    (delete-overlay ac-prefix-overlay)))

(defun ac-activate-completing-map ()
  (when ac-prefix-overlay
    (set-keymap-parent (overlay-get ac-prefix-overlay 'keymap) ac-completing-map)))

(defun ac-deactivate-completing-map ()
  (when ac-prefix-overlay
    (set-keymap-parent (overlay-get ac-prefix-overlay 'keymap) nil)))

(defsubst ac-selected-candidate ()
  (popup-selected-item ac-menu))

(defun ac-prefix ()
  "Return a pair of POINT of prefix and SOURCES to be applied."
  (loop with point
        with determined-prefix
        with sources
        for source in (ac-compiled-sources)
        for prefix = (assoc-default 'prefix source)

        if (null determined-prefix) do
        (save-excursion
          (setq point (cond
                       ((symbolp prefix)
                        (funcall prefix))
                       ((stringp prefix)
                        (when (re-search-backward (concat prefix "\\=") nil t)
                          (or (match-beginning 1) (match-beginning 0))))
                       (t
                        (eval prefix))))
          (if point
              (setq determined-prefix prefix)))

        if (equal prefix determined-prefix) do (push source sources)

        finally return (and point (list determined-prefix point (nreverse sources)))))

(defun ac-init ()
  "Initialize current sources to start completion."
  (setq ac-candidates-cache nil)
  (loop for source in ac-current-sources
        for function = (assoc-default 'init source)
        if function do
        (save-excursion
          (cond
           ((functionp function)
            (funcall function))
           (t
            (eval function))))))

(defun ac-candidates-1 (source)
  (let* ((do-cache (assq 'cache source))
         (function (assoc-default 'candidates source))
         (action (assoc-default 'action source))
         (document (assoc-default 'document source))
         (ac-limit (or (assoc-default 'limit source) ac-limit))
         (face (or (assoc-default 'face source) (assoc-default 'candidate-face source)))
         (selection-face (assoc-default 'selection-face source))
         (cache (and do-cache (assq source ac-candidates-cache)))
         (candidates (cdr cache)))
    (unless cache
      (setq candidates (save-excursion
                         (cond
                          ((functionp function)
                           (funcall function))
                          (t
                           (eval function)))))
      (when do-cache
        (push (cons source candidates) ac-candidates-cache)))
    (setq candidates (funcall (assoc-default 'match source) ac-prefix candidates))
    ;; Remove extra items regarding to ac-limit
    (if (and (> ac-limit 1) (> (length candidates) ac-limit))
        (setcdr (nthcdr (1- ac-limit) candidates) nil))
    ;; Put candidate properties
    (setq candidates (mapcar (lambda (candidate)
                               (popup-item-propertize candidate
                                                      'action action
                                                      'document document
                                                      'popup-face face
                                                      'selection-face selection-face))
                             candidates))
    candidates))

(defun ac-candidates ()
  "Produce candidates for current sources."
  (loop with completion-ignore-case = (or (eq ac-ignore-case t)
                                          (and (eq ac-ignore-case 'smart)
                                               (let ((case-fold-search nil)) (not (string-match "[[:upper:]]" ac-prefix)))))
        with prefix-len = (length ac-prefix)
        for source in ac-current-sources
        for function = (assoc-default 'candidates source)
        for requires = (or (assoc-default 'requires source) 0)

        if (and function (>= prefix-len requires))
        append (ac-candidates-1 source) into candidates
        finally return (delete-dups candidates)))

(defun ac-update-candidates (cursor scroll-top)
  "Update candidates of menu to `ac-candidates' and redraw it."
  (setf (popup-cursor ac-menu) cursor
        (popup-scroll-top ac-menu) scroll-top)
  (setq ac-dwim-enable (= (length ac-candidates) 1))
  (if ac-candidates
      (progn
        (setq ac-completing t)
        (ac-activate-completing-map))
    (setq ac-completing nil)
    (ac-deactivate-completing-map))
  (ac-inline-update)
  (when (and ac-common-part
             (member ac-common-part ac-candidates))
    ;; TODO general implementation
    ;; Move common-part candidate to the first.
    (setq ac-candidates (cons ac-common-part
                              (delete ac-common-part ac-candidates))))
  (popup-set-list ac-menu ac-candidates)
  (popup-draw ac-menu))

(defun ac-reposition ()
  "Force to redraw candidate menu with current `ac-candidates'."
  (let ((cursor (popup-cursor ac-menu))
        (scroll-top (popup-scroll-top ac-menu)))
    (popup-delete ac-menu)
    (ac-menu-create ac-point (popup-preferred-width ac-candidates) (popup-height ac-menu))
    (ac-update-candidates cursor scroll-top)))

(defun ac-cleanup ()
  "Cleanup auto completion."
  (ac-remove-quick-help)
  (ac-remove-prefix-overlay)
  (ac-inline-delete)
  (ac-menu-delete)
  (ac-cancel-timer)
  (ac-cancel-quick-help-timer)
  (setq ac-inline nil
        ac-menu nil
        ac-completing nil
        ac-point nil
        ac-prefix nil
        ac-prefix-overlay nil
        ac-candidates nil
        ac-candidates-cache nil
        ac-compiled-sources nil
        ac-current-sources nil))

(defsubst ac-abort ()
  "Abort completion."
  (ac-cleanup))

(defun ac-expand-string (string &optional remove-undo-boundary)
  "Expand `STRING' into the buffer and update `ac-prefix' to `STRING'.
This function records deletion and insertion sequences by `undo-boundary'.
If `remove-undo-boundary' is non-nil, this function also removes `undo-boundary'
that have been made before in this function."
  (undo-boundary)
  ;; We can't use primitive-undo since it undoes by
  ;; groups, divided by boundaries.
  ;; We don't want boundary between deletion and insertion.
  ;; So do it manually.
  ;; Delete region silently for undo:
  (if remove-undo-boundary
      (progn
        (let (buffer-undo-list)
          (save-excursion
            (delete-region ac-point (point))))
        (setq buffer-undo-list
              (nthcdr 2 buffer-undo-list)))
    (delete-region ac-point (point)))
  (insert string)
  ;; Sometimes, possible when omni-completion used, (insert) added
  ;; to buffer-undo-list strange record about position changes.
  ;; Delete it here:
  (when (and remove-undo-boundary
             (integerp (cadr buffer-undo-list)))
    (setcdr buffer-undo-list (nthcdr 2 buffer-undo-list)))
  (undo-boundary)
  (setq ac-prefix string))

(defun ac-set-trigger-key (key)
  "Set `ac-trigger-key' to `KEY'. It is recommemded to use this function instead of calling `setq'."
  ;; Remove old mapping
  (when ac-trigger-key
    (define-key ac-mode-map (read-kbd-macro ac-trigger-key) nil))

  ;; Make new mapping
  (setq ac-trigger-key key)
  (when key
    (define-key ac-mode-map (read-kbd-macro key) 'ac-trigger-key-command)))

(defun ac-set-timer ()
  (unless ac-timer
    (setq ac-timer (run-with-idle-timer ac-delay ac-delay 'ac-update))))

(defun ac-cancel-timer ()
  (when (timerp  ac-timer)
    (cancel-timer ac-timer)
    (setq ac-timer nil)))

(defun ac-update (&optional force)
  (when (and auto-complete-mode
             (or ac-triggered
                 force)
             (not isearch-mode))
    (progn
      (setq ac-candidates (ac-candidates))
      (let ((preferred-width (popup-preferred-width ac-candidates)))
        ;; Reposition if needed
        (when (or (null ac-menu)
                  (>= (popup-width ac-menu) preferred-width)
                  (<= (popup-width ac-menu) (- preferred-width 10))
                  (and (> (popup-direction ac-menu) 0)
                       (ac-menu-at-wrapper-line-p)))
          (ac-menu-delete)
          (ac-menu-create ac-point preferred-width ac-menu-height)))
      (ac-update-candidates 0 0))))

(defun ac-set-quick-help-timer ()
  (unless ac-quick-help-timer
    (setq ac-quick-help-timer (run-with-idle-timer ac-quick-help-delay ac-quick-help-delay 'ac-quick-help))))

(defun ac-cancel-quick-help-timer ()
  (when (timerp ac-quick-help-timer)
    (cancel-timer ac-quick-help-timer)
    (setq ac-quick-help-timer nil)))

(defun ac-quick-help ()
  (when (and (ac-menu-live-p)
             (null ac-quick-help))
    (setq ac-quick-help
          (popup-menu-show-help ac-menu nil :height ac-quick-help-height :scroll-bar t :nowait t))))

(defun ac-remove-quick-help ()
  (when ac-quick-help
    (popup-delete ac-quick-help)
    (setq ac-quick-help nil)))

(defmacro ac-define-quick-help-command (name arglist &rest body)
  (declare (indent 2))
  `(progn
     (defun ,name ,arglist ,@body)
     (put ',name 'ac-quick-help-command t)))

(defun ac-make-quick-help-command (command)
  (put command 'ac-quick-help-command t))



;; Auto completion commands

(defun auto-complete ()
  "Start auto-completion at current point."
  (interactive)
  (ac-abort)
  (ac-start)
  (ac-update t)
  ;; TODO Not to cause inline completion to be disrupted.
  (if (ac-inline-live-p)
      (ac-inline-hide))
  (ac-expand-common)
  t)

(defun ac-next ()
  "Select next candidate."
  (interactive)
  (when (ac-menu-live-p)
    (popup-next ac-menu)
    (if (eq this-command 'ac-next)
        (setq ac-dwim-enable t))))

(defun ac-previous ()
  "Select previous candidate."
  (interactive)
  (when (ac-menu-live-p)
    (popup-previous ac-menu)
    (if (eq this-command 'ac-previous)
        (setq ac-dwim-enable t))))

(defun ac-expand ()
  "Try expand, and if expanded twice, select next candidate."
  (interactive)
  (unless (ac-expand-common)
    (let ((string (ac-selected-candidate)))
      (when (equal ac-prefix string)
        (ac-next)
        (setq string (ac-selected-candidate)))
      (ac-expand-string string (eq last-command this-command))
      ;; Do reposition if menu at long line
      (if (and (> (popup-direction ac-menu) 0)
               (ac-menu-at-wrapper-line-p))
          (ac-reposition))
      string)))

(defun ac-expand-common ()
  "Try expand common part."
  (interactive)
  (if (and ac-dwim ac-dwim-enable)
      (ac-complete)
    (when (and (ac-inline-live-p)
               ac-common-part)
      (ac-inline-hide) 
      (ac-expand-string ac-common-part (eq last-command this-command))
      (setq ac-common-part nil)
      t)))

(defun ac-complete ()
  "Try complete."
  (interactive)
  (let* ((candidate (ac-selected-candidate))
         (action (popup-item-property candidate 'action)))
    (ac-expand-string candidate)
    (ac-abort)
    (if action
        (funcall action))
    candidate))

(defun ac-start (&optional nomessage)
  "Start completion."
  (interactive)
  (if (not auto-complete-mode)
      (message "auto-complete-mode is not enabled")
    (let* ((info (ac-prefix))
           (prefix (nth 0 info))
           (point (nth 1 info))
           (sources (nth 2 info))
           (init (not (eq ac-point point))))
      (if (or (null point)
              (and (eq prefix 'ac-prefix-default) ; if not omni-completion
                   (integerp ac-auto-start)
                   (< (- (point) point)
                      ac-auto-start)))
          (prog1 nil
            (ac-abort)
            (unless nomessage (message "Nothing to complete")))
        (setq ac-current-sources sources
              ac-buffer (current-buffer)
              ac-point point
              ac-prefix (buffer-substring-no-properties point (point))
              ac-limit ac-candidate-limit
              ac-triggered t)
        (when (or init (null ac-prefix-overlay))
          (ac-init))
        (ac-set-timer)
        (ac-set-quick-help-timer)
        (ac-put-prefix-overlay)))))

(defun ac-stop ()
  "Stop completiong."
  (interactive)
  (ac-abort))

(ac-define-quick-help-command ac-quick-help-scroll-down ()
  (interactive)
  (when ac-quick-help
    (popup-scroll-down ac-quick-help)))

(ac-define-quick-help-command ac-quick-help-scroll-up ()
  (interactive)
  (when ac-quick-help
    (popup-scroll-up ac-quick-help)))

(defun ac-trigger-key-command (&optional force)
  (interactive "P")
  (or (and (or force
               (ac-trigger-command-p last-command))
           (auto-complete))
      ;; borrowed from yasnippet.el
      (let* ((auto-complete-mode nil)
             (keys-1 (this-command-keys-vector))
             (keys-2 (read-kbd-macro ac-trigger-key))
             (command-1 (if keys-1 (key-binding keys-1)))
             (command-2 (if keys-2 (key-binding keys-2)))
             (command (or (if (not (eq command-1 'ac-trigger-key-command))
                              command-1)
                          command-2)))
        (when (and (commandp command)
                   (not (eq command 'ac-trigger-key-command)))
          (setq this-command command)
          (call-interactively command)))))



;; Auto complete mode

(defun ac-trigger-command-p (command)
  "Return non-nil if `COMMAND' is a trigger command."
  (and (symbolp command)
       (or (memq command ac-trigger-commands)
           (string-match "self-insert-command" (symbol-name command))
           (string-match "electric" (symbol-name command)))))

(defun ac-compatible-package-command-p (command)
  "Return non-nil if `COMMAND' is compatible with auto-complete."
  (and (symbolp command)
       (string-match ac-compatible-packages-regexp (symbol-name command))))

(defun ac-handle-pre-command ()
  (condition-case var
      (if (or (setq ac-triggered (or (ac-trigger-command-p this-command)
                                     (and ac-completing
                                          (memq this-command ac-trigger-commands-on-completing))))
              (ac-compatible-package-command-p this-command))
          (progn
            (if (or (not (symbolp this-command))
                    (not (get this-command 'ac-quick-help-command)))
                (ac-remove-quick-help))
            ;; Not to cause inline completion to be disrupted.
            (if (ac-inline-live-p)
                (ac-inline-hide)))
        (ac-abort))
    (error (ac-error var))))

(defun ac-handle-post-command ()
  (condition-case var
      (when (and ac-triggered
                 (or ac-auto-start
                     ac-completing)
                 (not isearch-mode))
        (ac-start t)
        (ac-inline-update))
    (error (ac-error var))))

(defun ac-setup ()
  (if ac-trigger-key
      (ac-set-trigger-key ac-trigger-key)))

(define-minor-mode auto-complete-mode
  "AutoComplete mode"
  :lighter " AC"
  :keymap ac-mode-map
  :group 'auto-complete
  (if auto-complete-mode
      (progn
        (ac-setup)
        (add-hook 'pre-command-hook 'ac-handle-pre-command nil t)
        (add-hook 'post-command-hook 'ac-handle-post-command nil t)
        (run-hooks 'auto-complete-mode-hook))
    (remove-hook 'pre-command-hook 'ac-handle-pre-command t)
    (remove-hook 'post-command-hook 'ac-handle-post-command t)
    (ac-abort)
    (assq-delete-all 'auto-complete-mode minor-mode-map-alist)))

(defun auto-complete-mode-maybe ()
  "What buffer `auto-complete-mode' prefers."
  (if (and (not (minibufferp (current-buffer)))
           (memq major-mode ac-modes))
      (auto-complete-mode 1)))

(define-global-minor-mode global-auto-complete-mode
  auto-complete-mode auto-complete-mode-maybe
  :group 'auto-complete)



;;;; Basic cache facility

(defvar ac-clear-variables-after-save nil)

(defun ac-clear-variable-after-save (variable)
  (push variable ac-clear-variables-after-save))

(defun ac-clear-variables-after-save ()
  (dolist (variable ac-clear-variables-after-save)
    (set variable nil)))



;;;; Standard sources

(defun ac-candidate-words-in-buffer (&optional limit)
  (or limit (setq limit ac-limit))
  (let ((i 0)
        candidate
        candidates
        (regexp (concat "\\_<" (regexp-quote ac-prefix) "\\(\\sw\\|\\s_\\)+\\_>")))
    (save-excursion
      ;; Search backward
      (goto-char ac-point)
      (while (and (or (eq limit t)
                      (< i limit))
                  (re-search-backward regexp nil t))
        (setq candidate (match-string-no-properties 0))
        (unless (member candidate candidates)
          (push candidate candidates)
          (incf i)))
      ;; Search backward
      (goto-char (+ ac-point (length ac-prefix)))
      (while (and (or (eq limit t)
                      (< i limit))
                  (re-search-forward regexp nil t))
        (setq candidate (match-string-no-properties 0))
        (unless (member candidate candidates)
          (push candidate candidates)
          (incf i)))
      (nreverse candidates))))

(defvar ac-source-words-in-buffer
  '((candidates . ac-candidate-words-in-buffer))
  "Source for completing words in current buffer.")

(defvar ac-word-index nil
  "Word index for individual buffer.")

(ac-clear-variable-after-save 'ac-word-index)

(defun ac-build-word-index ()
  (dolist (buffer (buffer-list))
    (unless (eq buffer ac-buffer)
      (with-current-buffer buffer
        (unless (local-variable-p 'ac-word-index)
          (make-local-variable 'ac-word-index))
        (when (and (null ac-word-index)
                   (< (buffer-size) 102400))
          (let ((ac-prefix "")
                (ac-point (point-min)))
            (setq ac-word-index (ac-candidate-words-in-buffer t))))))))

(defun ac-word-candidates (&optional buffer-pred)
  (loop initially (setq candidates (ac-candidate-words-in-buffer t))
        for buffer in (buffer-list)
        while (< (length candidates) ac-limit)
        if (and (not (eq buffer ac-buffer))
                (if buffer-pred (funcall buffer-pred buffer) t))
        append (all-completions ac-prefix (buffer-local-value 'ac-word-index buffer)) into candidates
        finally return (delete-dups candidates)))

(defvar ac-source-words-in-all-buffer
  '((init . ac-build-word-index)
    (candidates . ac-word-candidates))
  "Source for completing words in all buffer.")

(defvar ac-source-words-in-same-mode-buffers
  '((init . ac-build-word-index)
    (candidates . (ac-word-candidates
                   (lambda (buffer)
                     (derived-mode-p (buffer-local-value 'major-mode buffer))))))
  "Source for completing words in all of same mode buffers.")

(defvar ac-symbols-cache nil)

(defun ac-symbol-documentation (symbol)
  (if (stringp symbol)
      (setq symbol (intern-soft symbol)))
  (or (ignore-errors (documentation symbol t))
      (ignore-errors (documentation-property symbol 'variable-documentation t))))

(defvar ac-source-symbols
  '((init . (unless ac-symbols-cache
              (setq ac-symbols-cache
                    (loop for x being the symbols collect (symbol-name x)))))
    (candidates . ac-symbols-cache)
    ;(document . ac-symbol-documentation)
    (cache))
  "Source for Emacs lisp symbols.")

(defvar ac-source-abbrev
  '((candidates . (mapcar 'popup-x-to-string (append (vconcat local-abbrev-table global-abbrev-table) nil)))
    (action . expand-abbrev)
    (cache))
  "Source for abbrev.")

(defvar ac-source-files-in-current-dir
  '((candidates . (directory-files default-directory))
    (cache))
  "Source for listing files in current directory.")

(defvar ac-filename-cache nil)

(defun ac-filename-candidate ()
  (unless (file-regular-p ac-prefix)
    (ignore-errors
      (loop with dir = (file-name-directory ac-prefix)
            with files = (or (assoc-default dir ac-filename-cache)
                             (let ((files (directory-files dir nil "^[^.]")))
                               (push (cons dir files) ac-filename-cache)
                               files))
            for file in files
            for path = (concat dir file)
            collect (if (file-directory-p path)
                        (concat path "/")
                      path)))))

(defvar ac-source-filename
  '((init . (setq ac-filename-cache))
    (candidates . ac-filename-candidate)
    (prefix . valid-file)
    (action . ac-start)
    (limit . 0))
  "Source for completing file name.")

(defvar ac-imenu-index nil
  "Imenu index.")

(defun ac-imenu-candidate ()
  (require 'imenu)
  (let ((i 0)
        (stack ac-imenu-index)
        candidates
        node)
    (while (and stack
                (< i ac-limit))
      (setq node (pop stack))
      (when (consp node)
        (let ((car (car node))
              (cdr (cdr node)))
          (if (consp cdr)
              (mapc (lambda (child)
                      (push child stack))
                    cdr)
            (when (and (stringp car)
                       (string-match (concat "^" (regexp-quote ac-prefix)) car))
              (push car candidates)
              (setq i (1+ i)))))))
    (nreverse candidates)))

(defvar ac-source-imenu
  '((init
     . (lambda ()
         (require 'imenu)
         (setq ac-imenu-index
               (ignore-errors (imenu--make-index-alist)))))
    (candidates . ac-imenu-candidate))
  "Source for imenu.")

(defmacro ac-define-dictionary-source (name list)
  "Define dictionary source named `NAME'.
`LIST' is a list of string.
This is useful if you just want to define a dictionary/keywords source."
  `(defvar ,name
     '((candidates . (list ,@list))
       (cache))))

(provide 'auto-complete)
;;; auto-complete.el ends here
