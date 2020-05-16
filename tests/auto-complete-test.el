(require 'ert)

(require 'auto-complete)
(require 'auto-complete-config)
(require 'company)

;; Move this into test case or setup macro once we start testing with
;; non-default config.
(global-auto-complete-mode)

(defmacro ac-test-with-common-setup (&rest body)
  (declare (indent 0) (debug t))
  `(save-excursion
     (with-temp-buffer
       (switch-to-buffer (current-buffer))
       (auto-complete-mode 1)
       (emacs-lisp-mode)
       (unwind-protect
           (progn ,@body)
        (when ac-menu
          (ac-menu-delete)))
       )))

(ert-deftest ac-test-version-variable ()
  (should-not (null ac-version))
  (should (stringp ac-version)))

(ert-deftest ac-test-version-major ()
  (should-not (null ac-version-major))
  (should (numberp ac-version-major)))

(ert-deftest ac-test-version-minor ()
  (should-not (null ac-version-minor))
  (should (numberp ac-version-minor)))

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

(ert-deftest ac-test-single-candidate ()
  (ac-test-with-common-setup
    (let ((ac-source-test
           '((candidates list "Foo" "FooBar" "Bar" "Baz" "LongLongLine"))))
      (setq ac-sources '(ac-source-test))
      (should-not (popup-live-p ac-menu))
      (should (eq ac-menu nil))
      (insert "FooB")
      (auto-complete)
      (should-not (popup-live-p ac-menu))
      (should (eq ac-menu nil))
      (should (string= (buffer-string) "FooBar"))
      )))

(ert-deftest ac-test-complete-common-part ()
  (ac-test-with-common-setup
    (let ((ac-source-test
           '((candidates list "Foo" "FooBar" "Bar" "Baz" "LongLongLine"))))
      (setq ac-sources '(ac-source-test))
      (execute-kbd-macro "Fo")
      (auto-complete)
      (ac-stop)
      (should (string= (buffer-string) "Foo"))
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

(ert-deftest ac-test-update-no-candidate ()
  (ac-test-with-common-setup
    (let ((ac-source-test
           '((candidates list "Foo" "FooBar" "Bar" "Baz" "LongLongLine"))))
      (setq ac-sources '(ac-source-test))
      (should-not (popup-live-p ac-menu))
      (should (eq ac-menu nil))
      (insert "Foo")
      (auto-complete)
      (execute-kbd-macro "A")
      (ac-update-greedy)
      (should-not (popup-live-p ac-menu))
      (should (eq ac-menu nil))
      (should (string= (buffer-string) "FooA"))
      )))

(ert-deftest ac-test-default-selection ()
  (ac-test-with-common-setup
    (let ((ac-source-test
           '((candidates list "Action1" "Action2" "Action3"))))
      (setq ac-sources '(ac-source-test))
      (should-not (popup-live-p ac-menu))
      (should (eq ac-menu nil))
      (insert "Action")
      (auto-complete)
      (should (popup-live-p ac-menu))
      (should (equal (popup-list ac-menu) '("Action1" "Action2" "Action3")))
      (should (equal (ac-selected-candidate) "Action1"))
      )))

(ert-deftest ac-test-delete-duplicate-candidates ()
  (ac-test-with-common-setup
    (let ((ac-source-test
           '((candidates list "Action1" "Action1" "Action2"))))
      (setq ac-sources '(ac-source-test))
      (should-not (popup-live-p ac-menu))
      (should (eq ac-menu nil))
      (insert "Action")
      (auto-complete)
      (should (equal (popup-list ac-menu) '("Action1" "Action2")))
      )))

(ert-deftest ac-test-duplicate-candidates-but-different-properties ()
  (ac-test-with-common-setup
    (let ((ac-source-test
           '((candidates list
                         (propertize "Action1" 'action 'foo)
                         (propertize "Action1" 'action 'foo)
                         (propertize "Action1" 'action 'bar)))))
      (setq ac-sources '(ac-source-test))
      (should-not (popup-live-p ac-menu))
      (should (eq ac-menu nil))
      (insert "Action")
      (auto-complete)
      (should (equal (popup-list ac-menu) '("Action1" "Action1")))
      )))

(ert-deftest ac-test-crude-speed-ac ()
  (ac-test-with-common-setup
   (let ((tfile (make-temp-file "actest" nil ".C"))
	 (ac-delay 0.0)
	 (ac-auto-show-menu nil)
	 (ac-quick-help-delay 0.0))
     (unwind-protect
	 (save-window-excursion
	   (find-file tfile)
	   (auto-complete-mode 1)
	   (insert "cons")
	   (message "//   ac-test-crude-speed-ac: %s"
		    (benchmark-run (auto-complete-1)))
	   (should (member "const_cast" ac-candidates))
	   (set-buffer-modified-p nil)
	   (kill-buffer))
       (delete-file tfile)))))

(ert-deftest ac-test-crude-speed-company ()
  (ac-test-with-common-setup
   (let ((tfile (make-temp-file "actest" nil ".C"))
	 (company-idle-delay 0))
     (unwind-protect
	 (save-window-excursion
	   (find-file tfile)
	   (company-mode)
	   ;; "Priming the pump" made no difference
	   ;; (insert "stru")
	   ;; (call-interactively #'company-complete)
	   ;; (erase-buffer)
	   (insert "cons")
	   (message "//   ac-test-crude-speed-company: %s"
		    (benchmark-run (call-interactively #'company-complete)))
	   (should (member "constexpr" company-candidates))
	   (set-buffer-modified-p nil)
	   (kill-buffer))
       (delete-file tfile)))))
