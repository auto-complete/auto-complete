;;; ac-semantic.el --- auto-complete source for semantic

;; Copyright (C) 2008, 2009, 2010, 2011, 2012  Tomohiro Matsuyama

;; Author: Tomohiro Matsuyama <m2ym.pub@gmail.com>

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

(defun ac-semantic-candidates (prefix)
  (with-no-warnings
    (delete ""            ; semantic sometimes returns an empty string
            (mapcar '(lambda (elem)
                       (cons (semantic-tag-name elem)
                             (semantic-tag-clone elem)))
                    (ignore-errors
                      (or (semantic-analyze-possible-completions
                           (semantic-analyze-current-context))
                          (senator-find-tag-for-completion prefix)))))))

(defun ac-semantic-doc (symbol)
  (let* ((proto (semantic-format-tag-summarize-with-file symbol nil t))
         (doc (semantic-documentation-for-tag symbol))
         (res proto))
    (when doc
      (setq res (concat res "\n\n" doc)))
    res))

(ac-define-source semantic
  '((available . (or (require 'semantic-ia nil t)
                     (require 'semantic/ia nil t)))
    (candidates . (ac-semantic-candidates ac-prefix))
    (document . ac-semantic-doc)
    (prefix . c-dot-ref)
    (requires . 0)
    (symbol . "m")))

(ac-define-source semantic-raw
  '((available . (or (require 'semantic-ia nil t)
                     (require 'semantic/ia nil t)))
    (candidates . (ac-semantic-candidates ac-prefix))
    (document . ac-semantic-doc)
    (symbol . "s")))

(provide 'ac-semantic)
