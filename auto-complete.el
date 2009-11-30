;;; auto-complete.el --- Auto completion

;; Copyright (C) 2008, 2009  Tomohiro Matsuyama

;; Author: Tomohiro Matsuyama <m2ym.pub@gmail.com>
;; Repository http://github.com/m2ym/auto-complete
;; Keywords: convenience
;; Version: 1.0

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

(require 'pulldown)
(require 'expander)

(defgroup auto-complete nil
  "Auto completion."
  :group 'convenience
  :prefix "ac-")

(defcustom ac-menu-height 10
  "Max height of candidate menu."
  :type 'integer
  :group 'auto-complete)
(defvaralias 'ac-candidate-menu-height 'ac-menu-height)

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

(defcustom ac-trigger-commands
  '(self-insert-command)
  "Trigger commands that specify whether `auto-complete' should start or not."
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

(defcustom ac-dwim nil
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

(defvar ac-expander nil
  "Expander instance.")

(defvar ac-menu nil
  "Menu instance.")

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

    map)
  "Keymap for completion")
(defvaralias 'ac-complete-mode-map 'ac-completing-map)

(defvar ac-prefix-definitions
  '((symbol . ac-prefix-symbol)
    (file . ac-prefix-file)
    (valid-file . ac-prefix-valid-file)
    (c-dot . ac-prefix-c-dot))
  "Prefix definitions for common use.")

(defvar ac-sources '(ac-source-words-in-buffer)
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

(defun ac-menu-live-p ()
  (pulldown-live-p ac-menu))

(defun ac-menu-delete ()
  (when ac-menu
    (pulldown-delete ac-menu)
    (setq ac-menu)))

(defun ac-expander-live-p ()
  (expander-live-p ac-expander))

(defun ac-expander-show (point string)
  (unless ac-expander
    (setq ac-expander (expander-create 'ac-completion-face)))
  (expander-show ac-expander point string))

(defun ac-expander-delete ()
  (when ac-expander
    (expander-delete ac-expander)
    (setq ac-expander nil)))

(defun ac-expander-hide ()
  (if ac-expander
      (expander-hide ac-expander)))

(defun ac-expander-update ()
  (setq ac-common-part (try-completion ac-prefix ac-candidates))
  (if (and (stringp ac-common-part)
           (> (length ac-common-part) (length ac-prefix)))
      (ac-expander-show (point) (substring ac-common-part (length ac-prefix)))
    (ac-expander-delete)))

(defun ac-activate-mode-map ()
  "Activate `ac-completing-map'. This cause `ac-completing' to be used temporaly."
  (assq-delete-all 'ac-completing minor-mode-map-alist)
  (push (cons 'ac-completing ac-completing-map) minor-mode-map-alist))

(defun ac-deactivate-mode-map ()
  "Deactivate `ac-completing-map'."
  (assq-delete-all 'ac-completing minor-mode-map-alist))

(defun ac-get-selected-candidate ()
  (nth (pulldown-cursor ac-menu) ac-candidates))

(defun ac-get-candidate-action (candidate)
  (ac-get-candidate-property 'action candidate))

(defun ac-propertize-candidate (candidate &rest properties)
  (apply 'propertize candidate properties))

(defun ac-get-candidate-property (prop candidate)
  (get-text-property 0 prop candidate))

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
    (setq candidates (mapcar (lambda (candidate)
                               (pulldown-item-propertize (pulldown-x-to-string candidate)
                                                         'action action
                                                         'menu-face face
                                                         'selection-face selection-face))
                             (funcall (assoc-default 'match source) ac-prefix candidates)))
    ;; Remove extra items regarding to ac-limit
    (if (and (> ac-limit 1) (> (length candidates) ac-limit))
        (setcdr (nthcdr (1- ac-limit) candidates) nil))
    candidates))

(defun ac-candidates ()
  "Produce candidates for current sources."
  (loop with prefix-len = (length ac-prefix)
        for source in ac-current-sources
        for function = (assoc-default 'candidates source)
        for requires = (or (assoc-default 'requires source) 0)

        if (and function (>= prefix-len requires))
        append (ac-candidates-1 source) into candidates
        finally return (delete-dups candidates)))

(defun ac-update-candidates (cursor scroll-top)
  "Update candidates of menu to `ac-candidates' and redraw it."
  (setf (pulldown-cursor ac-menu) cursor
        (pulldown-scroll-top ac-menu) scroll-top)
  (setq ac-dwim-enable (= (length ac-candidates) 1))
  (if ac-candidates
      (progn
        (setq ac-completing t)
        (ac-activate-mode-map))
    (setq ac-completing nil)
    (ac-deactivate-mode-map))
  (ac-expander-update)
  (when (and ac-common-part
             (member ac-common-part ac-candidates))
    ;; TODO general implementation
    ;; Move common-part candidate to the first.
    (setq ac-candidates (cons ac-common-part
                              (delete ac-common-part ac-candidates))))
  (pulldown-set-list ac-menu ac-candidates)
  (pulldown-draw ac-menu))

(defun ac-reposition ()
  "Force to redraw candidate menu with current `ac-candidates'."
  (let ((cursor (pulldown-cursor ac-menu))
        (scroll-top (pulldown-scroll-top ac-menu)))
    (pulldown-delete ac-menu)
    (setq ac-menu (pulldown-create ac-point (pulldown-preferred-width ac-candidates) (pulldown-height ac-menu)))
    (ac-update-candidates cursor scroll-top)))

(defun ac-cleanup ()
  "Cleanup auto completion."
  (ac-deactivate-mode-map)
  (ac-expander-delete)
  (ac-menu-delete)
  (setq ac-expander nil
        ac-menu nil
        ac-completing nil
        ac-point nil
        ac-prefix nil
        ac-candidates nil
        ac-candidates-cache nil
        ac-compiled-sources nil
        ac-current-sources nil))

(defun ac-abort ()
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



;; Auto completion commands

(defun auto-complete ()
  "Start auto-completion at current point."
  (interactive)
  (ac-abort)
  (ac-start)
  ;; TODO Not to cause inline completion to be disrupted.
  (if (ac-expander-live-p)
      (expander-hide ac-expander))
  (ac-expand-common)
  t)

(defun ac-next ()
  "Select next candidate."
  (interactive)
  (when (ac-menu-live-p)
    (pulldown-next ac-menu)
    (if (eq this-command 'ac-next)
        (setq ac-dwim-enable t))))

(defun ac-previous ()
  "Select previous candidate."
  (interactive)
  (when (ac-menu-live-p)
    (pulldown-previous ac-menu)
    (if (eq this-command 'ac-previous)
        (setq ac-dwim-enable t))))

(defun ac-expand ()
  "Try expand, and if expanded twice, select next candidate."
  (interactive)
  (unless (ac-expand-common)
    (let ((string (ac-get-selected-candidate)))
      (when (equal ac-prefix string)
        (ac-next)
        (setq string (ac-get-selected-candidate)))
      (ac-expand-string string (eq last-command this-command))
      ;; Do reposition if menu at long line
      (if (and (> (pulldown-direction ac-menu) 0)
               (ac-menu-at-wrapper-line-p))
          (ac-reposition))
      string)))

(defun ac-expand-common ()
  "Try expand common part."
  (interactive)
  (if (and ac-dwim ac-dwim-enable)
      (ac-complete)
    (when (and (ac-expander-live-p)
               ac-common-part)
      (ac-expand-string ac-common-part (eq last-command this-command))
      (setq ac-common-part nil)
      t)))

(defun ac-complete ()
  "Try complete."
  (interactive)
  (let* ((candidate (ac-get-selected-candidate))
         (action (ac-get-candidate-action candidate)))
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
              ac-completing t)
        (when (or init (null ac-menu))
          (ac-init))
        (setq ac-candidates (ac-candidates))
        (unless nomessage (message "Completion started"))
        (let ((preferred-width (pulldown-preferred-width ac-candidates)))
          ;; Reposition if needed
          (when (or (null ac-menu)
                    (>= (pulldown-width ac-menu) preferred-width)
                    (<= (pulldown-width ac-menu) (- preferred-width 10))
                    (and (> (pulldown-direction ac-menu) 0)
                         (ac-menu-at-wrapper-line-p)))
            (ac-menu-delete)
            (setq ac-menu (pulldown-create ac-point preferred-width ac-menu-height))))
        (ac-update-candidates 0 0)
        (not (null ac-candidates))))))

(defun ac-stop ()
  "Stop completiong."
  (interactive)
  (ac-abort))



;; Auto complete mode

(defun ac-trigger-command-p (command)
  "Return non-nil if `COMMAND' is a trigger command."
  (and (symbolp command)
       (or (memq command ac-trigger-commands)
           (string-match "self-insert-command" (symbol-name command))
           (string-match "electric" (symbol-name command))
           (and                         ;ac-completing
            (memq command
                  '(delete-backward-char
                    backward-delete-char
                    backward-delete-char-untabify))))))

(defun ac-handle-pre-command ()
  (condition-case var
      (if (or (ac-trigger-command-p this-command)
              (and (symbolp this-command)
                   (string-match "^ac-" (symbol-name this-command))))
          ;; Not to cause inline completion to be disrupted.
          (if (ac-expander-live-p)
              (expander-hide ac-expander))
        (ac-abort))
    (error (ac-error var))))

(defun ac-handle-post-command ()
  (condition-case var
      (if (and (or ac-auto-start
                   ac-completing)
               (not isearch-mode)
               (ac-trigger-command-p this-command))
          (ac-start t))
    (error (ac-error var))))

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

(defun ac-set-trigger-key (key)
  "Set `ac-trigger-key' to `KEY'. It is recommemded to use this function instead of calling `setq'."
  ;; Remove old mapping
  (when ac-trigger-key
    (define-key ac-mode-map (read-kbd-macro ac-trigger-key) nil))

  ;; Make new mapping
  (setq ac-trigger-key key)
  (when key
    (define-key ac-mode-map (read-kbd-macro key) 'ac-trigger-key-command)))

(defun auto-complete-mode-maybe ()
  "What buffer `auto-complete-mode' prefers."
  (if (and (not (minibufferp (current-buffer)))
           (memq major-mode ac-modes))
      (auto-complete-mode 1)))

(defun ac-setup ()
  (make-local-variable 'ac-clear-variables-after-save)
  (add-hook 'after-save-hook 'ac-clear-variables-after-save nil t)
  (if ac-trigger-key
      (ac-set-trigger-key ac-trigger-key))
  (assq-delete-all 'auto-complete-mode minor-mode-map-alist)
  (push (cons 'auto-complete-mode ac-mode-map) minor-mode-map-alist))

(define-minor-mode auto-complete-mode
  "AutoComplete mode"
  :lighter " AC"
  :group 'auto-complete
  (if auto-complete-mode
      (progn
        (ac-setup)
        (add-hook 'post-command-hook 'ac-handle-post-command nil t)
        (add-hook 'pre-command-hook 'ac-handle-pre-command nil t)
        (run-hooks 'auto-complete-mode-hook))
    (remove-hook 'post-command-hook 'ac-handle-post-command t)
    (remove-hook 'pre-command-hook 'ac-handle-pre-command t)
    (ac-abort)
    (assq-delete-all 'auto-complete-mode minor-mode-map-alist)))

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

(defvar ac-source-symbols
  '((init . (unless ac-symbols-cache
              (setq ac-symbols-cache (loop for x being the symbols collect (symbol-name x)))))
    (candidates . ac-symbols-cache)
    (cache))
  "Source for Emacs lisp symbols.")

(defvar ac-source-abbrev
  '((candidates . (mapcar 'prin1-to-string (append (vconcat local-abbrev-table global-abbrev-table) nil)))
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
