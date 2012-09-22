(require 'ert)

(require 'auto-complete)
(require 'auto-complete-config)

;; Move this into test case or setup macro once we start testing with
;; non-default config.
(ac-config-default)

(defmacro ac-test-with-common-setup (&rest body)
  (declare (indent 0) (debug t))
  `(save-excursion
     (with-temp-buffer
       (switch-to-buffer (current-buffer))
       (auto-complete-mode 1)
       (emacs-lisp-mode)
       ,@body
       (ac-menu-delete)
       )))

(ert-deftest ac-test-simple-invocation ()
  (ac-test-with-common-setup
    (let ((ac-source-test
           '((candidates list "Foo" "FooBar" "Bar" "Baz" "LongLongLine")))
          (ac-source-action-test
           '((candidates list "Action1" "Action2")
             (action . (lambda () (message "Done!")))))
          (ac-sources '(ac-source-test ac-source-action-test)))
      (should-not (popup-live-p ac-menu))
      (should (eq ac-menu nil))
      (insert "Foo")
      (auto-complete)
      (should (popup-live-p ac-menu))
      (should (equal (popup-list ac-menu) '("Foo" "FooBar")))
      )))

(ert-deftest ac-test-simple-update ()
  (ac-test-with-common-setup
    (let ((ac-source-test
           '((candidates list "Foo" "FooBar" "Bar" "Baz" "LongLongLine")))
          (ac-source-action-test
           '((candidates list "Action1" "Action2")
             (action . (lambda () (message "Done!")))))
          (ac-sources '(ac-source-test ac-source-action-test)))
      (should-not (popup-live-p ac-menu))
      (should (eq ac-menu nil))
      (insert "Foo")
      (auto-complete)
      (execute-kbd-macro "B")
      (ac-update-greedy)
      (should (popup-live-p ac-menu))
      (should (equal (popup-list ac-menu) '("FooBar")))
      )))

(ert-deftest ac-test-complete-common-part-when-buffer-undo-list-is-t ()
  (ac-test-with-common-setup
   (let ((ac-source-test
          '((candidates list "Foo" "FooBar" "Bar" "Baz" "LongLongLine")))
         (ac-sources '(ac-source-test)))
     (execute-kbd-macro "Fo")
     (let ((last-command this-command)
           (buffer-undo-list t))
       (auto-complete))
     (ac-stop)
     (should (string= (buffer-string) "Foo"))
     )))
