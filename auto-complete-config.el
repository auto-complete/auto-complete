;;; auto-complete-config.el --- auto-complete additional configuations

;; Copyright (C) 2009-2010  Tomohiro Matsuyama

;; Author: Tomohiro Matsuyama <m2ym.pub@gmail.com>
;; Keywords: convenience
;; Version: 1.5.0

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

(require 'cl-lib)
(require 'auto-complete)

(declare-function semantic-analyze-current-context "semantic/analyze")
(declare-function semantic-tag-class "semantic/tag")
(declare-function semantic-tag-function-arguments "semantic/tag")
(declare-function semantic-format-tag-type "semantic/format")
(declare-function semantic-format-tag-name "semantic/format")
(declare-function yas-expand-snippet "yasnippet")
(declare-function oref "eieio" (obj slot))



;;;; Additional sources

;; imenu

(defvar ac-imenu-index nil)

(ac-clear-variable-every-10-minutes 'ac-imenu-index)

(defun ac-imenu-candidates ()
  "No documentation."
  (cl-loop with i = 0
           with stack = (progn
                          (unless (local-variable-p 'ac-imenu-index)
                            (make-local-variable 'ac-imenu-index))
                          (or ac-imenu-index
                              (setq ac-imenu-index
                                    (ignore-errors
                                      (with-no-warnings
                                        (imenu--make-index-alist))))))
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
                 (cl-incf i))))
           finally return (nreverse result)))

(ac-define-source imenu
  '((depends imenu)
    (candidates . ac-imenu-candidates)
    (symbol . "s")))

;; gtags

(defface ac-gtags-candidate-face
  '((t (:inherit ac-candidate-face :foreground "navy")))
  "Face for gtags candidate"
  :group 'auto-complete)

(defface ac-gtags-selection-face
  '((t (:inherit ac-selection-face :background "navy")))
  "Face for the gtags selected candidate."
  :group 'auto-complete)

(defun ac-gtags-candidate ()
  "No documentation."
  (ignore-errors
    (split-string (shell-command-to-string (format "global -ciq %s" ac-prefix)) "\n")))

(ac-define-source gtags
  '((candidates . ac-gtags-candidate)
    (candidate-face . ac-gtags-candidate-face)
    (selection-face . ac-gtags-selection-face)
    (requires . 3)
    (symbol . "s")))

;; yasnippet

(defface ac-yasnippet-candidate-face
  '((t (:inherit ac-candidate-face
                 :background "sandybrown" :foreground "black")))
  "Face for yasnippet candidate."
  :group 'auto-complete)

(defface ac-yasnippet-selection-face
  '((t (:inherit ac-selection-face :background "coral3")))
  "Face for the yasnippet selected candidate."
  :group 'auto-complete)

(defun ac-yasnippet-table-hash (table)
  "No documentation, TABLE."
  (cond
   ((fboundp 'yas/snippet-table-hash)
    (yas/snippet-table-hash table))
   ((fboundp 'yas/table-hash)
    (yas/table-hash table))))

(defun ac-yasnippet-table-parent (table)
  "No documentation, TABLE."
  (cond
   ((fboundp 'yas/snippet-table-parent)
    (yas/snippet-table-parent table))
   ((fboundp 'yas/table-parent)
    (yas/table-parent table))))

(defun ac-yasnippet-candidate-1 (table)
  "No documentation, TABLE."
  (with-no-warnings
    (let ((hashtab (ac-yasnippet-table-hash table))
          (parent (ac-yasnippet-table-parent table))
          candidates)
      (maphash (lambda (key value)
                 (push key candidates))
               hashtab)
      (setq candidates (all-completions ac-prefix (nreverse candidates)))
      (when parent
        (setq candidates
              (append candidates (ac-yasnippet-candidate-1 parent))))
      candidates)))

(defun ac-yasnippet-candidates ()
  "No documentation."
  (with-no-warnings
    (cond (;; 0.8 onwards
           (fboundp 'yas-active-keys)
           (all-completions ac-prefix (yas-active-keys)))
          (;; >0.6.0
           (fboundp 'yas/get-snippet-tables)
           (apply 'append (mapcar 'ac-yasnippet-candidate-1
                                  (condition-case nil
                                      (yas/get-snippet-tables major-mode)
                                    (wrong-number-of-arguments
                                     (yas/get-snippet-tables)))))
           )
          (t
           (let ((table
                  (if (fboundp 'yas/snippet-table)
                      ;; <0.6.0
                      (yas/snippet-table major-mode)
                    ;; 0.6.0
                    (yas/current-snippet-table))))
             (when table
               (ac-yasnippet-candidate-1 table)))))))

(ac-define-source yasnippet
  '((depends yasnippet)
    (candidates . ac-yasnippet-candidates)
    (action . yas/expand)
    (candidate-face . ac-yasnippet-candidate-face)
    (selection-face . ac-yasnippet-selection-face)
    (symbol . "a")))

;; semantic

(defun ac-semantic-candidates (prefix)
  "No documentation, PREFIX."
  (with-no-warnings
    (delete ""            ; semantic sometimes returns an empty string
            (mapcar (lambda (elem)
                      (cons (semantic-tag-name elem)
                            (semantic-tag-clone elem)))
                    (ignore-errors
                      (or (semantic-analyze-possible-completions
                           (semantic-analyze-current-context))
                          (senator-find-tag-for-completion prefix)))))))

(defun ac-semantic-doc (symbol)
  "No documentation, SYMBOL."
  (with-no-warnings
    (let* ((proto (semantic-format-tag-summarize-with-file symbol nil t))
           (doc (semantic-documentation-for-tag symbol))
           (res proto))
      (when doc
        (setq res (concat res "\n\n" doc)))
      res)))

(defun ac-semantic-action ()
  "No documentation."
  (when (and (boundp 'yas-minor-mode) yas-minor-mode)
    (let* ((tag (car (last (oref (semantic-analyze-current-context) prefix))))
           (class (semantic-tag-class tag))
           (args))
      (when (eq class 'function)
        (setq args (semantic-tag-function-arguments tag))
        (yas-expand-snippet
         (concat "("
                 (mapconcat
                  (lambda (arg)
                    (let ((arg-type (semantic-format-tag-type arg nil))
                          (arg-name (semantic-format-tag-name arg nil)))
                      (concat "${"
                              (if (string= arg-name "")
                                  arg-type
                                (concat arg-type " " arg-name))
                              "}")))
                  args
                  ", ")
                 ")$0"))))))

(ac-define-source semantic
  '((available . (or (require 'semantic-ia nil t)
                     (require 'semantic/ia nil t)))
    (candidates . (ac-semantic-candidates ac-prefix))
    (document . ac-semantic-doc)
    (action . ac-semantic-action)
    (prefix . cc-member)
    (requires . 0)
    (symbol . "m")))

(ac-define-source semantic-raw
  '((available . (or (require 'semantic-ia nil t)
                     (require 'semantic/ia nil t)))
    (candidates . (ac-semantic-candidates ac-prefix))
    (document . ac-semantic-doc)
    (action . ac-semantic-action)
    (symbol . "s")))


;; eclim
(defun ac-eclim-candidates ()
  "No documentation."
  (with-no-warnings
    (cl-loop for c in (eclim/java-complete)
             collect (nth 1 c))))

(ac-define-source eclim
  '((candidates . ac-eclim-candidates)
    (prefix . c-dot)
    (requires . 0)
    (symbol . "f")))


;; slime
(ac-define-source slime
  '((depends slime)
    (candidates . (car (slime-simple-completions ac-prefix)))
    (symbol . "s")
    (cache)))


;; ghc-mod
(ac-define-source ghc-mod
  '((depends ghc)
    (candidates . (ghc-select-completion-symbol))
    (symbol . "s")
    (cache)))



;;;; Not maintained sources

;; ropemacs

(defvar ac-ropemacs-loaded nil)

(defun ac-ropemacs-require ()
  "No documentation."
  (with-no-warnings
    (unless ac-ropemacs-loaded
      (pymacs-load "ropemacs" "rope-")
      (when (boundp 'ropemacs-enable-autoimport)
        (setq ropemacs-enable-autoimport t))
      (setq ac-ropemacs-loaded t))))

(defun ac-ropemacs-setup ()
  "No documentation."
  (ac-ropemacs-require)
  ;;(setq ac-sources (append (list 'ac-source-ropemacs) ac-sources))
  (setq ac-omni-completion-sources '(("\\." ac-source-ropemacs))))

(defun ac-ropemacs-initialize ()
  "No documentation."
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
  "No documentation."
  ;; TODO: Don't know this is commented; leave an empty function.
  ;;(add-to-list 'ac-sources 'ac-source-filename)
  )

(defun ac-emacs-lisp-mode-setup ()
  "No documentation."
  (setq ac-sources (cl-union '(ac-source-features
                               ac-source-functions
                               ac-source-yasnippet
                               ac-source-variables
                               ac-source-symbols)
                             ac-sources)))

(defun ac-cc-mode-setup ()
  "No documentation."
  (setq ac-sources (cl-union '(ac-source-yasnippet ac-source-gtags)
                             ac-sources)))

(defun ac-ruby-mode-setup ()
  "No documentation."
  ;; TODO: Don't know this is commented; leave an empty function.
  )

(defun ac-css-mode-setup ()
  "No documentation."
  (setq ac-sources (cl-union '(ac-source-css-property)
                             ac-sources)))

;;;###autoload
(defun ac-config-default ()
  "No documentation."
  (setq-default ac-sources '(ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))
  (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
  (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
  (add-hook 'ruby-mode-hook 'ac-ruby-mode-setup)
  (add-hook 'css-mode-hook 'ac-css-mode-setup)
  (add-hook 'auto-complete-mode-hook 'ac-common-setup)
  (global-auto-complete-mode t))

(provide 'auto-complete-config)
;;; auto-complete-config.el ends here
