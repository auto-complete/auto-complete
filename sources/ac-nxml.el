;;; ac-nxml.el --- auto-complete source for nxml mode

;; Copyright (C) 2012 Christopher Monsanto

;; Author: Christopher Monsanto <chris@monsan.to>

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

(defun ac-nxml-candidate ()
  (and rng-validate-mode
       (let ((lt-pos (save-excursion (search-backward "<" nil t)))
             xmltok-dtd)
         (and lt-pos
              (= (rng-set-state-after lt-pos) lt-pos)
              (or (ac-nxml-tag-helper lt-pos)
                  (ac-nxml-attribute-name-helper lt-pos)
                  (ac-nxml-attribute-value-helper lt-pos))))))

(defun ac-nxml-tag-helper (lt-pos)
  (when (save-excursion
          (re-search-backward rng-in-start-tag-name-regex lt-pos t))
    (let ((rng-complete-target-names (rng-match-possible-start-tag-names))
          rng-complete-name-attribute-flag)
      (all-completions (buffer-substring-no-properties (1+ lt-pos) (point))
                       'rng-complete-qname-function))))

(defun ac-nxml-attribute-name-helper (lt-pos)
  (when (save-excursion
          (re-search-backward rng-in-attribute-regex lt-pos t))
    (let ((attribute-start (match-beginning 1)))
      (and (rng-adjust-state-for-attribute lt-pos attribute-start)
           (let ((rng-complete-target-names (rng-match-possible-attribute-names))
                 (rng-complete-extra-strings
                  (mapcar (lambda (prefix)
                            (if prefix
                                (concat "xmlns:" prefix)
                              "xmlns"))
                          rng-undeclared-prefixes))
                 (rng-complete-name-attribute-flag t))
             (all-completions (buffer-substring-no-properties attribute-start (point))
                       'rng-complete-qname-function))))))

(defun ac-nxml-attribute-value-helper (lt-pos)
  (when (save-excursion
          (re-search-backward rng-in-attribute-value-regex lt-pos t))
    (let ((name-start (match-beginning 1))
          (name-end (match-end 1))
          (colon (match-beginning 2))
          (value-start (1+ (match-beginning 3))))
      (and (rng-adjust-state-for-attribute lt-pos
                                           name-start)
           (if (string= (buffer-substring-no-properties name-start
                                                        (or colon name-end))
                        "xmlns")
               (all-completions (buffer-substring-no-properties value-start (point))
                                (rng-strings-to-completion-alist
                                 (rng-possible-namespace-uris
                                  (and colon
                                       (buffer-substring-no-properties (1+ colon) name-end)))))
             (rng-adjust-state-for-attribute-value name-start
                                                   colon
                                                   name-end)
             (all-completions (buffer-substring-no-properties value-start (point))
                              (rng-strings-to-completion-alist
                               (rng-match-possible-value-strings))))))))

(ac-define-source nxml
  '((depends nxml)
    (candidates . ac-nxml-candidate)
    (prefix . "^.*?\\([a-zA-Z=]+\\)")))

(provide 'ac-nxml)
