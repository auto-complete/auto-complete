;;; auto-complete-config.el --- auto-complete additional configuations

;; Copyright (C) 2009, 2010  Tomohiro Matsuyama

;; Author: Tomohiro Matsuyama <m2ym.pub@gmail.com>
;; Keywords: convenience

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

(require 'auto-complete)



;;;; Additional sources

;; imenu

(defvar ac-imenu-index nil)

(ac-clear-variable-every-10-minutes 'ac-imenu-index)

(defun ac-imenu-candidates ()
  (loop with i = 0
        with stack = (progn
                       (unless (local-variable-p 'ac-imenu-index)
                         (make-local-variable 'ac-imenu-index))
                       (or ac-imenu-index
                           (setq ac-imenu-index (ignore-errors (imenu--make-index-alist)))))
        with result
        while (and stack (or (not (integerp ac-limit))
                             (< i ac-limit)))
        for node = (pop stack)
        if (consp node)
        do
        (let ((car (car node))
              (cdr (cdr node)))
          (if (consp cdr)
              (mapc (lambda (child)
                      (push child stack))
                    cdr)
            (when (and (stringp car)
                       (string-match (concat "^" (regexp-quote ac-prefix)) car))
              ;; Remove extra characters
              (if (string-match "^.*\\(()\\|=\\|<>\\)$" car)
                  (setq car (substring car 0 (match-beginning 1))))
              (push car result)
              (incf i))))
        finally return (nreverse result)))

(ac-define-source imenu
  '((depends imenu)
    (candidates . ac-imenu-candidates)
    (symbol . "s")))

;; gtags

(defface ac-gtags-candidate-face
  '((t (:background "lightgray" :foreground "navy")))
  "Face for gtags candidate"
  :group 'auto-complete)

(defface ac-gtags-selection-face
  '((t (:background "navy" :foreground "white")))
  "Face for the gtags selected candidate."
  :group 'auto-complete)

(defun ac-gtags-candidate ()
  (ignore-errors
    (split-string (shell-command-to-string (format "global -ci %s" ac-prefix)) "\n")))

(ac-define-source gtags
  '((candidates . ac-gtags-candidate)
    (candidate-face . ac-gtags-candidate-face)
    (selection-face . ac-gtags-selection-face)
    (requires . 3)
    (symbol . "s")))

;; yasnippet

(defface ac-yasnippet-candidate-face
  '((t (:background "sandybrown" :foreground "black")))
  "Face for yasnippet candidate."
  :group 'auto-complete)

(defface ac-yasnippet-selection-face
  '((t (:background "coral3" :foreground "white")))
  "Face for the yasnippet selected candidate."
  :group 'auto-complete)

(defun ac-yasnippet-candidate-1 (table)
  (let ((hashtab (yas/snippet-table-hash table))
        (parent (if (fboundp 'yas/snippet-table-parent)
                    (yas/snippet-table-parent table)))
        candidates)
    (maphash (lambda (key value)
               (push key candidates))
             hashtab)
    (setq candidates (all-completions ac-prefix (nreverse candidates)))
    (if parent
        (setq candidates
              (append candidates (ac-yasnippet-candidate-1 parent))))
    candidates))

(defun ac-yasnippet-candidate ()
  (if (fboundp 'yas/get-snippet-tables)
      ;; >0.6.0
      (apply 'append (mapcar 'ac-yasnippet-candidate-1 (yas/get-snippet-tables major-mode)))
    (let ((table
           (if (fboundp 'yas/snippet-table)
               ;; <0.6.0
               (yas/snippet-table major-mode)
             ;; 0.6.0
             (yas/current-snippet-table))))
      (if table
          (ac-yasnippet-candidate-1 table)))))

(ac-define-source yasnippet
  '((depends yasnippet)
    (candidates . ac-yasnippet-candidate)
    (action . yas/expand)
    (candidate-face . ac-yasnippet-candidate-face)
    (selection-face . ac-yasnippet-selection-face)
    (symbol . "a")))

;; semantic

(defun ac-semantic-candidates (prefix)
  (mapcar 'semantic-tag-name
          (ignore-errors
            (or (semantic-analyze-possible-completions
                 (semantic-analyze-current-context))
                (senator-find-tag-for-completion prefix)))))

(ac-define-source semantic
  '((depends semantic-ia)
    (candidates . (ac-semantic-candidates ac-prefix))
    (prefix . c-dot)
    (requires . 0)
    (symbol . "f")))

;; eclim

(defun ac-eclim-candidates ()
  (loop for c in (eclim/java-complete)
        collect (nth 1 c)))

(ac-define-source eclim
  '((candidates . ac-eclim-candidates)
    (prefix . c-dot)
    (requires . 0)
    (symbol . "f")))



;;;; Not maintained sources

;; ropemacs

(defvar ac-ropemacs-loaded nil)
(defun ac-ropemacs-require ()
  (unless ac-ropemacs-loaded
    (pymacs-load "ropemacs" "rope-")
    (if (boundp 'ropemacs-enable-autoimport)
        (setq ropemacs-enable-autoimport t))
    (setq ac-ropemacs-loaded t)))

(defun ac-ropemacs-setup ()
  (ac-ropemacs-require)
  ;(setq ac-sources (append (list 'ac-source-ropemacs) ac-sources))
  (setq ac-omni-completion-sources '(("\\." ac-source-ropemacs))))

(defun ac-ropemacs-initialize ()
  (autoload 'pymacs-apply "pymacs")
  (autoload 'pymacs-call "pymacs")
  (autoload 'pymacs-eval "pymacs" nil t)
  (autoload 'pymacs-exec "pymacs" nil t)
  (autoload 'pymacs-load "pymacs" nil t)
  (add-hook 'python-mode-hook 'ac-ropemacs-setup)
  t)

(defvar ac-ropemacs-completions-cache nil)
(defvar ac-source-ropemacs
  '((init
     . (lambda ()
         (setq ac-ropemacs-completions-cache
               (mapcar
                (lambda (completion)
                  (concat ac-prefix completion))
                (ignore-errors
                  (rope-completions))))))
    (candidates . ac-ropemacs-completions-cache)))

;; rcodetools

(defvar ac-source-rcodetools
  '((init . (lambda ()
              (require 'rcodetools)
              (condition-case x
                  (save-excursion
                    (rct-exec-and-eval rct-complete-command-name "--completion-emacs-icicles"))
                (error) (setq rct-method-completion-table nil))))
    (candidates . (lambda ()
                    (all-completions
                     ac-prefix
                     (mapcar
                      (lambda (completion)
                        (replace-regexp-in-string "\t.*$" "" (car completion)))
                      rct-method-completion-table))))))



;;;; Default settings

(defun ac-common-setup ()
  (add-to-list 'ac-sources 'ac-source-filename))

(defun ac-emacs-lisp-mode-setup ()
  (setq ac-sources (append '(ac-source-features ac-source-functions ac-source-yasnippet ac-source-variables ac-source-symbols) ac-sources)))

(defun ac-cc-mode-setup ()
  (setq ac-sources (append '(ac-source-yasnippet ac-source-gtags) ac-sources)))

(defun ac-ruby-mode-setup ()
  (make-local-variable 'ac-ignores)
  (add-to-list 'ac-ignores "end"))

(defun ac-config-default ()
  (setq-default ac-sources '(ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))
  (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
  (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
  (add-hook 'ruby-mode-hook 'ac-ruby-mode-setup)
  (add-hook 'auto-complete-mode-hook 'ac-common-setup)
  (global-auto-complete-mode t))

(provide 'auto-complete-config)
;;; auto-complete-config.el ends here
