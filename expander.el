;;; expander.el --- Text expander interface

;; Copyright (C) 2009  Tomohiro Matsuyama

;; Author: Tomohiro Matsuyama <m2ym.pub@gmail.com>
;; Keywords: lisp

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

(defface expander-face
  '((t (:background "darkblue" :foreground "white")))
  "Default face for expander.")

(defstruct expander
  face overlay marker)

(defun* expander-create (&optional (face 'expander-face))
  (make-expander :face face
                 :marker (make-marker)))

(defun expander-delete (expander)
  (when (expander-live-p expander)
    (expander-hide expander)
    (delete-overlay (expander-overlay expander))
    (setf (expander-overlay expander))))

(defun expander-live-p (expander)
  (and expander (expander-overlay expander) t))

(defun expander-show (expander point string)
  (save-excursion
    (let ((overlay (expander-overlay expander))
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
        (set-marker (expander-marker expander) point)
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
      (setq string (propertize string 'face (expander-face expander)))
      (if overlay
          (progn
            (move-overlay overlay point (+ point width))
            (overlay-put overlay 'invisible nil))
        (setq overlay (make-overlay point (+ point width)))
        (setf (expander-overlay expander) overlay)
        (overlay-put overlay 'priority 9999))
      (overlay-put overlay 'display (substring string 0 1))
      ;; TODO no width but char
      (overlay-put overlay 'after-string (substring string 1))
      (overlay-put overlay 'string original-string))))

(defun expander-hide (expander)
  (let ((overlay (expander-overlay expander))
        (marker (expander-marker expander))
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
      (overlay-put overlay 'after-string nil))))

(defun expander-string (expander)
  (overlay-get (expander-overlay expander) 'string))

(provide 'expander)
;;; expander.el ends here
