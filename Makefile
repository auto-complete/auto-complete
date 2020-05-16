export EMACS ?= $(shell which emacs)
export CASK := $(shell which cask)
ifeq ($(CASK),)
$(error Please install CASK at https://cask.readthedocs.io/en/latest/guide/installation.html)
endif
CASK_DIR := $(shell $(CASK) package-directory || exit 1)
SRC = $(shell $(CASK) files)
VERSION = $(shell $(CASK) version)
ELCFILES = $(SRC:.el=.elc)
TESTS = $(shell ls tests/*test.el)
TESTSSRC = $(TESTS)
ELCTESTS = $(TESTSSRC:.el=.elc)
.DEFAULT_GOAL := test-compile

.PHONY: autoloads
autoloads:
	$(EMACS) -Q --batch --eval "(package-initialize)" --eval "(package-generate-autoloads \"auto-complete\" \".\")"

.PHONY: clean
clean:
	$(CASK) clean-elc
	rm -f README.html
	rm -f doc/*.html

README.html: README.md
	pandoc --standalone --to html --output $@ $^

.PHONY: cask
cask: $(CASK_DIR)

$(CASK_DIR): Cask
	$(CASK) install
	touch $(CASK_DIR)

.PHONY: test-compile
test-compile: cask autoloads
	! ($(CASK) eval \
	      "(cl-letf (((symbol-function (quote cask-files)) (lambda (&rest _args) (mapcar (function symbol-name) (quote ($(TESTSSRC))))))) \
	          (let ((byte-compile-error-on-warn t)) (cask-cli/build)))" 2>&1 | egrep -a "(Warning|Error):") ; (ret=$$? ; rm -f $(ELCTESTS) && exit $$ret)
	! ($(CASK) eval "(let ((byte-compile-error-on-warn t)) (cask-cli/build))" 2>&1 | egrep -a "(Warning|Error):") ; (ret=$$? ; $(CASK) clean-elc && exit $$ret)

TESTFILES = $(shell $(CASK) files)

.PHONY: test-unit
test-unit:
	$(CASK) exec ert-runner -L . -L tests $(TESTS)

.PHONY: test-clean
test-clean:
	rm -rf tests/.emacs*

.PHONY: test
test: test-compile test-unit

.PHONY: dist-clean
dist-clean:
	rm -rf dist

.PHONY: dist
dist: dist-clean
	$(CASK) package

.PHONY: backup-melpa
backup-melpa:
	$(EMACS) -Q --batch --eval "(package-initialize)" --eval \
	  "(with-temp-buffer \
	    (insert-file-contents-literally (car (file-expand-wildcards \"dist/auto-complete-$(VERSION).tar\"))) \
	    (tar-mode) \
	    (let* ((my-desc (package-tar-file-info)) \
	           (name (package-desc-name my-desc)) \
	           (other-pkgs (cdr (assq name package-alist)))) \
	      (when other-pkgs \
	        (mapcar (lambda (odesc) \
	                  (let* ((odir (package-desc-dir odesc)) \
	                         (parent (file-name-directory odir)) \
	                         (leaf (file-name-nondirectory odir))) \
	                    (if (equal (package-desc-version my-desc) \
	                               (package-desc-version odesc)) \
	                        (delete-directory odir t) \
	                      (rename-file odir \
	                                   (expand-file-name (format \"BACKUP-%s\" leaf) parent) \
	                                   t)))) \
	                other-pkgs))))"

.PHONY: install
install: dist backup-melpa
	$(EMACS) -Q --batch --eval "(package-initialize)" \
	  --eval "(add-to-list 'package-archives '(\"melpa\" . \"http://melpa.org/packages/\"))" \
	  --eval "(package-refresh-contents)" \
	  --eval "(package-install-file (car (file-expand-wildcards \"dist/auto-complete-$(VERSION).tar\")))"
