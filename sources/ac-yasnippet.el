;;; ac-yasnippet.el --- auto-complete source for yasnippet

;; Copyright (C) Tomohiro Matsuyama, Alex Murray, João Távora, Christopher Monsanto

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

(require 'yasnippet)

(defface ac-yasnippet-candidate-face
  '((t (:background "sandybrown" :foreground "black")))
  "Face for yasnippet candidate."
  :group 'auto-complete)

(defface ac-yasnippet-selection-face
  '((t (:background "coral3" :foreground "white")))
  "Face for the yasnippet selected candidate."
  :group 'auto-complete)

(defun ac-yasnippet-document (symbol)
  ;; expand snippet in a temporary buffer
  (let ((orig-major-mode major-mode)
        (templates (mapcan #'(lambda (table)
                               (yas--fetch table symbol))
                           (yas--get-snippet-tables))))
    (with-temp-buffer
      (let ((major-mode orig-major-mode))
        (insert (concatenate 'string "Snippet " symbol ":\n"))
        ;; expand to potentially multiple options and show each
        (dolist (template templates)
          (insert "\n")
          (yas-expand-snippet (yas--template-content (cdr template)))
          (yas-exit-all-snippets)
          ;; go to end of buffer to expand next snippet
          ;; following this one
          (goto-char (point-max))))
      (buffer-string))))

(defun ac-yasnippet-candidates ()
  (all-completions ac-prefix (yas-active-keys)))

(ac-define-source yasnippet
  '((depends yasnippet)
    (candidates . ac-yasnippet-candidates)
    (action . yas-expand)
    (document . ac-yasnippet-document)
    (candidate-face . ac-yasnippet-candidate-face)
    (selection-face . ac-yasnippet-selection-face)
    (symbol . "a")))

(provide 'ac-yasnippet)
