;; Usage:
;;
;;   cask exec emacs -Q -l tests/run-test.el           # interactive mode
;;   cask exec emacs -batch -Q -l tests/run-test.el    # batch mode

(defvar ac-test-dir (file-name-directory load-file-name))
(defvar ac-root-dir (concat ac-test-dir ".."))

;; Setup `load-path'
(mapc (lambda (p) (add-to-list 'load-path p))
      (list ac-test-dir
            ac-root-dir))

;; Load tests
(load "auto-complete-test")

;; Run tests
(if noninteractive
    (ert-run-tests-batch-and-exit)
  (ert t))
