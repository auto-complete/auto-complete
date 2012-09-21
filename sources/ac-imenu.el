;;; ac-imenu.el --- auto-complete source for imenu

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

(defvar ac-imenu-index nil)

(ac-clear-variable-every-10-minutes 'ac-imenu-index)

(defun ac-imenu-candidates ()
  (loop with i = 0
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
              (incf i))))
        finally return (nreverse result)))

(ac-define-source imenu
  '((depends imenu)
    (candidates . ac-imenu-candidates)
    (symbol . "s")))

(provide 'ac-imenu)
