;;; fuzzy.el --- Fuzzy matching utilities

;; Copyright (C) 2010  Tomohiro Matsuyama

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
(require 'regexp-opt)

(defgroup fuzzy nil
  "Fuzzy matching utilities."
  :group 'convenience
  :prefix "fuzzy-")

(defcustom fuzzy-accept-error-rate 0.10
  "Error threshold."
  :group 'fuzzy)

(defvar fuzzy-accept-length-difference 2)

(defvar fuzzy-regexp-some-char (format "\\w\\{0,%s\\}" fuzzy-accept-length-difference))



;;; Functions

(defun fuzzy-reverse-string (string)
  (apply 'string (nreverse (append string nil))))

(defun fuzzy-regexp-compile (string)
  (labels ((oddp (n) (eq (logand n 1) 1))
           (evenp (n) (eq (logand n 1) 0))
           (opt (n) (regexp-opt-charset (append (substring string
                                                           (max 0 (- n 1))
                                                           (min (length string) (+ n 2))) nil))))
    (concat
     "\\("
     (loop for i below (length string)
           for c = (if (evenp i) (opt i) fuzzy-regexp-some-char)
           concat c)
     "\\|"
     (loop for i below (length string)
           for c = (if (oddp i) (opt i) fuzzy-regexp-some-char)
           concat c)
     "\\)")))

(defalias 'fuzzy-edit-distance 'fuzzy-jaro-winkler-distance)

(defun fuzzy-jaro-winkler-distance (s1 s2)
  "http://en.wikipedia.org/wiki/Jaro-Winkler_distance"
  (let* ((l1 (length s1))
         (l2 (length s2))
         (r (max 1 (1- (/ (max l1 l2) 2))))
         (m 0)
         (tr 0)
         (p 0)
         cs1 cs2)
    (loop with seen = (make-vector l2 nil)
          for i below l1
          for c1 = (aref s1 i) do
          (loop for j from (max 0 (- i r)) below (min l2 (+ i r))
                for c2 = (aref s2 j)
                if (and (char-equal c1 c2)
                        (null (aref seen j))) do
                  (push c1 cs1)
                  (aset seen j c2)
                  (incf m)
                and return nil)
          finally
            (setq cs1 (nreverse cs1)
                  cs2 (loop for i below l2
                            for c = (aref seen i)
                            if c collect c)))
    (loop for c1 in cs1
          for c2 in cs2
          if (not (char-equal c1 c2)) do
            (incf tr))
    (loop for i below (min m 5)
          for c1 across s1
          for c2 across s2
          while (char-equal c1 c2) do
            (incf p))
    (if (eq m 0)
        0.0
      (setq m (float m))
      (let* ((dj (/ (+ (/ m l1) (/ m l2) (/ (- m (/ tr 2)) m)) 3))
             (dw (+ dj (* p 0.1 (- 1 dj)))))
        dw))))

;; this function should be compiled
(byte-compile 'fuzzy-jaro-winkler-distance)

(defun fuzzy-match (s1 s2 &optional function)
  (or function (setq function 'fuzzy-edit-distance))
  (and (<= (abs (- (length s1) (length s2)))
           fuzzy-accept-length-difference)
       (>= (funcall function s1 s2)
           (- 1 fuzzy-accept-error-rate))))

(defun fuzzy-all-completions (string collection)
  "all-completions family with fuzzy matching."
  (loop with length = (length string)
        for str in collection
        for s = (substring str 0 (min (length str)
                                      (+ length fuzzy-accept-length-difference)))
        if (fuzzy-match string s)
        collect str))



;;; Search and Incremental Search

(defvar fuzzy-search-cache nil)
(defvar fuzzy-search-cache-string nil)

(defun fuzzy-search-cache-activate ()
  (setq fuzzy-search-cache (make-hash-table))
  (setq fuzzy-search-cache-string nil))

(defun fuzzy-search-cache-deactive ()
  (setq fuzzy-search-cache nil)
  (setq fuzzy-search-cache-string nil))

(defun fuzzy-search-edit-distance (s1 s2)
  (or (and fuzzy-search-cache
           (cond
            ((null fuzzy-search-cache-string)
             (setq fuzzy-search-cache-string s1)
             nil)
            ((not (equal fuzzy-search-cache-string s1))
             (setq fuzzy-search-cache-string s1)
             (clrhash fuzzy-search-cache)
             nil)
            (t))
           (gethash s2 fuzzy-search-cache))
      (let ((d (fuzzy-edit-distance s1 s2)))
        (if fuzzy-search-cache
            (puthash s2 d fuzzy-search-cache))
        d)))

(defun fuzzy-search-match (s1 s2)
  (fuzzy-match s1 s2 'fuzzy-search-edit-distance))

(defun fuzzy-search-forward (string &optional bound noerror count)
  (let* ((regexp (fuzzy-regexp-compile string))
         match-data)
    (save-excursion
      (while (and (null match-data)
                  (re-search-forward regexp bound t))
        (if (fuzzy-search-match string (match-string 1))
            (setq match-data (match-data))
          (goto-char (1+ (match-beginning 1))))))
    (when match-data
      (store-match-data match-data)
      (goto-char (match-end 1)))))

(defun fuzzy-search-backward (string &optional bound noerror count)
  (let* ((regexp (fuzzy-regexp-compile string))
         match-data begin end)
    (save-excursion
      (while (and (null match-data)
                  (re-search-backward regexp bound t))
        (setq begin (match-beginning 1)
              end (match-end 1))
        (store-match-data nil)
        (goto-char (max (point-min) (- begin (* (length string) 2))))
        (while (re-search-forward regexp end t)
          (if (fuzzy-search-match string (match-string 1))
              (setq match-data (match-data))
            (goto-char (1+ (match-beginning 1)))))
        (unless match-data
          (goto-char begin)))
    (if match-data
        (progn
          (store-match-data match-data)
          (goto-char (match-beginning 1)))
      (store-match-data nil)))))

(defvar fuzzy-isearch nil)
(defvar fuzzy-isearch-failed-count 0)
(defvar fuzzy-isearch-enabled 'on-failed)
(defvar fuzzy-isearch-original-search-fun nil)
(defvar fuzzy-isearch-prefix "[FUZZY] ")

(defun fuzzy-isearch-activate ()
  (setq fuzzy-isearch t)
  (setq fuzzy-isearch-failed-count 0)
  (fuzzy-search-cache-activate))

(defun fuzzy-isearch-deactivate ()
  (setq fuzzy-isearch nil)
  (setq fuzzy-isearch-failed-count 0)
  (fuzzy-search-cache-deactive))

(defun fuzzy-isearch ()
  (cond (isearch-word
         (if isearch-forward 'word-search-forward 'word-search-backward))
        (isearch-regexp
         (if isearch-forward 're-search-forward 're-search-backward))
        ((or fuzzy-isearch
             (eq fuzzy-isearch-enabled 'always)
             (and (eq fuzzy-isearch-enabled 'on-failed)
                  (null isearch-success)
                  isearch-wrapped
                  (> (setq fuzzy-isearch-failed-count (1+ fuzzy-isearch-failed-count))
                     1)))
         (unless fuzzy-isearch
           ;(goto-char isearch-opoint)
           (fuzzy-isearch-activate))
         (if isearch-forward 'fuzzy-search-forward 'fuzzy-search-backward))
        (t
         (if isearch-forward 'search-forward 'search-backward))))

(defun fuzzy-isearch-end-hook ()
  (fuzzy-isearch-deactivate))

(defun turn-on-fuzzy-isearch ()
  (interactive)
  (setq fuzzy-isearch-original-search-fun isearch-search-fun-function)
  (setq isearch-search-fun-function 'fuzzy-isearch)
  (add-hook 'isearch-mode-end-hook 'fuzzy-isearch-end-hook))

(defun turn-off-fuzzy-isearch ()
  (interactive)
  (setq isearch-search-fun-function fuzzy-isearch-original-search-fun)
  (remove-hook 'isearch-mode-end-hook 'fuzzy-isearch-end-hook))

(defadvice isearch-message-prefix (after fuzzy-isearch-message-prefix activate)
  (if fuzzy-isearch
      (setq ad-return-value (concat fuzzy-isearch-prefix ad-return-value))
    ad-return-value))

(provide 'fuzzy)
;;; fuzzy.el ends here
