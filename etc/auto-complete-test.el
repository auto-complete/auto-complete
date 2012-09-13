(require 'ert)

(require 'auto-complete)
(require 'auto-complete-config)

(defmacro auto-complete-test:common (&rest body)
  (declare (indent 0) (debug t))
  `(save-excursion
     ;; (with-current-buffer "*scratch*"
     (with-temp-buffer
       (switch-to-buffer (current-buffer))
       (auto-complete-mode 1)
       (emacs-lisp-mode)
       ,@body
       (ac-menu-delete)
       )))

(ert-deftest auto-complete-test ()
  (auto-complete-test:common
    (defvar ac-source-test
      '((candidates list "Foo" "FooBar" "Bar" "Baz" "LongLongLine")))
    (defvar ac-source-action-test
      '((candidates list "Action1" "Action2")
        (action . (lambda () (message "Done!")))))
    (setq ac-sources '(ac-source-test ac-source-action-test))
    (should-not (popup-live-p ac-menu))
    (should (eq ac-menu nil))
    (insert "Foo")
    (auto-complete)
    (should (popup-live-p ac-menu))
    (should (equal (popup-list ac-menu) '("Foo" "FooBar")))
    ))

(ert-deftest auto-complete-test2 ()
  (auto-complete-test:common
    (defvar ac-source-test
      '((candidates list "Foo" "FooBar" "Bar" "Baz" "LongLongLine")))
    (defvar ac-source-action-test
      '((candidates list "Action1" "Action2")
        (action . (lambda () (message "Done!")))))
    (setq ac-sources '(ac-source-test ac-source-action-test))
    (should-not (popup-live-p ac-menu))
    (should (eq ac-menu nil))
    (insert "Foo")
    (auto-complete)
    (execute-kbd-macro "B")
    (ac-update-greedy)
    (should (popup-live-p ac-menu))
    (should (equal (popup-list ac-menu) '("FooBar")))
    ))

(ert-run-tests-interactively t)
