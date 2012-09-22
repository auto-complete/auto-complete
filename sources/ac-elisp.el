;;; ac-elisp.el --- auto-complete source for elisp

;; Copyright (C) 2012  Tomohiro Matsuyama, Christopher Monsanto

;; Author: Tomohiro Matsuyama <m2ym.pub@gmail.com>
;;         Christopher Monsanto <chris@monsanto>

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

;; elisp symbols source
;;

(require 'auto-complete)
(require 'help-mode)

(defvar ac-symbols-cache nil)
(ac-clear-variable-every-10-minutes 'ac-symbols-cache)

(defun ac-symbol-file (symbol type)
  (if (fboundp 'find-lisp-object-file-name)
      (find-lisp-object-file-name symbol type)
    (let ((file-name (with-no-warnings
                       (describe-simplify-lib-file-name
                        (symbol-file symbol type)))))
      (when (equal file-name "loaddefs.el")
        ;; Find the real def site of the preloaded object.
        (let ((location (condition-case nil
                            (if (eq type 'defun)
                                (find-function-search-for-symbol symbol nil
                                                                 "loaddefs.el")
                              (find-variable-noselect symbol file-name))
                          (error nil))))
          (when location
            (with-current-buffer (car location)
              (when (cdr location)
                (goto-char (cdr location)))
              (when (re-search-backward
                     "^;;; Generated autoloads from \\(.*\\)" nil t)
                (setq file-name (match-string 1)))))))
      (if (and (null file-name)
               (or (eq type 'defun)
                   (integerp (get symbol 'variable-documentation))))
          ;; It's a object not defined in Elisp but in C.
          (if (get-buffer " *DOC*")
              (if (eq type 'defun)
                  (help-C-file-name (symbol-function symbol) 'subr)
                (help-C-file-name symbol 'var))
            'C-source)
        file-name))))

(defun ac-symbol-documentation (symbol)
  (if (stringp symbol)
      (setq symbol (intern-soft symbol)))
  (ignore-errors
    (with-temp-buffer
      (let ((standard-output (current-buffer)))
        (prin1 symbol)
        (princ " is ")
        (cond
         ((fboundp symbol)
          (let ((help-xref-following t)
                (major-mode 'help-mode)) ; avoid error in Emacs 24
            (describe-function-1 symbol))
          (buffer-string))
         ((boundp symbol)
          (let ((file-name  (ac-symbol-file symbol 'defvar)))
            (princ "a variable")
            (when file-name
              (princ " defined in `")
              (princ (if (eq file-name 'C-source)
                         "C source code"
                       (file-name-nondirectory file-name))))
            (princ "'.\n\n")
            (princ (or (documentation-property symbol 'variable-documentation t)
                       "Not documented."))
            (buffer-string)))
         ((facep symbol)
          (let ((file-name  (ac-symbol-file symbol 'defface)))
            (princ "a face")
            (when file-name
              (princ " defined in `")
              (princ (if (eq file-name 'C-source)
                         "C source code"
                       (file-name-nondirectory file-name))))
            (princ "'.\n\n")
            (princ (or (documentation-property symbol 'face-documentation t)
                       "Not documented."))
            (buffer-string)))
         (t
          (let ((doc (documentation-property symbol 'group-documentation t)))
            (when doc
              (princ "a group.\n\n")
              (princ doc)
              (buffer-string)))))))))

(defun ac-symbol-candidates ()
  (or ac-symbols-cache
      (setq ac-symbols-cache
            (loop for x being the symbols
                  if (or (fboundp x)
                         (boundp x)
                         (symbol-plist x))
                  collect (symbol-name x)))))

(ac-define-source symbols
  '((candidates . ac-symbol-candidates)
    (document . ac-symbol-documentation)
    (symbol . "s")
    (cache)))

;; elisp functions source
;;

(defvar ac-functions-cache nil)
(ac-clear-variable-every-10-minutes 'ac-functions-cache)

(defun ac-function-candidates ()
  (or ac-functions-cache
      (setq ac-functions-cache
            (loop for x being the symbols
                  if (fboundp x)
                  collect (symbol-name x)))))

(ac-define-source functions
  '((candidates . ac-function-candidates)
    (document . ac-symbol-documentation)
    (symbol . "f")
    (prefix . "(\\(\\(?:\\sw\\|\\s_\\)+\\)")
    (cache)))


(defun ac-function-action ()
  (let* ((arglist (help-function-arglist (intern candidate) t))
         flag
         (template (apply 'concat
                          (loop for x in arglist
                                if (member x '(&optional &rest))
                                do (setq flag x)
                                unless (member x '(&optional &rest))
                                collect (let ((name (symbol-name x)))
                                          (format " ${%s}"
                                                  (cond
                                                   ((eq flag '&optional) (format "[%s]" name))
                                                   ((eq flag '&rest) (format "[%s...]" name))
                                                   (t name))))))))
    (yas-expand-snippet template)))

(ac-define-source functions-yas
  '((depends yasnippet)
    (candidates . ac-function-candidates)
    (document . ac-symbol-documentation)
    (action . ac-function-action)
    (symbol . "f")
    (prefix . "(\\(\\(?:\\sw\\|\\s_\\)+\\)")
    (cache)))

;; elisp variables source
;;

(defvar ac-variables-cache nil)
(ac-clear-variable-every-10-minutes 'ac-variables-cache)

(defun ac-variable-candidates ()
  (or ac-variables-cache
      (setq ac-variables-cache
            (loop for x being the symbols
                  if (boundp x)
                  collect (symbol-name x)))))

(ac-define-source variables
  '((candidates . ac-variable-candidates)
    (document . ac-symbol-documentation)
    (symbol . "v")
    (cache)))

;; elisp features source
;;

(defvar ac-emacs-lisp-features nil)
(ac-clear-variable-every-10-minutes 'ac-emacs-lisp-features)

(defun ac-emacs-lisp-feature-candidates ()
  (or ac-emacs-lisp-features
      (if (fboundp 'find-library-suffixes)
          (let ((suffix (concat (regexp-opt (find-library-suffixes) t) "\\'")))
            (setq ac-emacs-lisp-features
                  (append (mapcar 'prin1-to-string features)
                          (loop for dir in load-path
                                if (file-directory-p dir)
                                append (loop for file in (directory-files dir)
                                             if (string-match suffix file)
                                             collect (substring file 0 (match-beginning 0))))))))))

(ac-define-source features
  '((depends find-func)
    (candidates . ac-emacs-lisp-feature-candidates)
    (prefix . "require +'\\(\\(?:\\sw\\|\\s_\\)*\\)")
    (requires . 0)))

(defvaralias 'ac-source-emacs-lisp-features 'ac-source-features)

(defun ac-emacs-lisp-mode-setup ()
  (add-to-list ac-sources 'ac-source-symbols)
  (add-to-list ac-sources 'ac-source-variables)
  (add-to-list ac-sources 'ac-source-functions)
  (add-to-list ac-sources 'ac-source-features))

(provide 'ac-elisp)
