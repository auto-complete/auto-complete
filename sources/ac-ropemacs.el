;;; ac-ropemacs.el --- auto-complete source for ropemacs

;; Copyright (C) Tomohiro Matsuyama

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

(require 'auto-complete)

(defun ac-ropemacs-candidates ()
  (save-match-data 
    (loop for (name doc type) in (ignore-errors (rope-extended-completions))
          unless (string-match ":" name)
          collect (list (concat ac-prefix name) doc))))

(defun ac-ropemacs-document (item)
  (car item))

(ac-define-source ropemacs
  '((candidates . ac-ropemacs-candidates)
    (document . ac-ropemacs-document)))

(ac-define-source ropemacs-dot
  '((candidates . ac-ropemacs-candidates)
    (document . ac-ropemacs-document)
    (prefix . c-dot)
    (requires . 0)))

(defun ac-ropemacs-setup ()
  (add-to-list 'ac-sources 'ac-source-ropemacs)
  (add-to-list 'ac-sources 'ac-source-ropemacs-dot))

(provide 'ac-ropemacs)
