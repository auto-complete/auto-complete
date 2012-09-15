;; Usage:
;;
;;   emacs -Q -l tests/run-test.el           # interactive mode
;;   emacs -batch -Q -l tests/run-test.el    # batch mode


;; Utils
(defun ac-test-join-path (path &rest rest)
  "Join a list of PATHS with appropriate separator (such as /).

\(fn &rest paths)"
  (if rest
      (concat (file-name-as-directory path) (apply 'ac-test-join-path rest))
    path))

(defvar ac-test-dir (file-name-directory load-file-name))
(defvar ac-root-dir (concat ac-test-dir ".."))


;; Setup `load-path'
(mapc (lambda (p) (add-to-list 'load-path p))
      (list ac-test-dir
            ac-root-dir
            (ac-test-join-path ac-root-dir "lib" "popup")
            (ac-test-join-path ac-root-dir "lib" "fuzzy")))


;; Use ERT from github when this Emacs does not have it
(unless (locate-library "ert")
  (add-to-list
   'load-path
   (ac-test-join-path ac-root-dir "lib" "ert" "lisp" "emacs-lisp"))
  (require 'ert-batch)
  (require 'ert-ui))


;; Load tests
(load "auto-complete-test")


;; Run tests
(if noninteractive
    (ert-run-tests-batch-and-exit)
  (ert t))
