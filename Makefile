VERSION=`perl -ne 'print $$1 if /;; Version: (.*)/' auto-complete.el`
PACKAGE=auto-complete-${VERSION}
EMACS ?= emacs
CASK ?= cask
SITE=../auto-complete.github.com

ELPA_DIR = \
	.cask/$(shell $(EMACS) -Q --batch --eval '(princ emacs-version)')/elpa

test: elpa
	$(CASK) exec $(EMACS) -batch -Q -L . \
		-l tests/run-test.el \
		-f ert-run-tests-batch-and-exit

byte-compile: elpa
	$(CASK) exec $(EMACS) -Q -L . -batch -f batch-byte-compile auto-complete.el auto-complete-config.el

install: byte-compile
	$(CASK) exec $(EMACS) -Q -L . -batch -l etc/install ${DIR}

README.html: README.md
	pandoc --standalone --to html --output $@ $^

site: README.html
	(cd doc && make)
	cp README.html $(SITE)/index.html
	mkdir -p $(SITE)/doc
	cp doc/*.png doc/*.html doc/*.css $(SITE)/doc

clean:
	rm -f README.html
	rm -f *.elc
	rm -f doc/*.html
	rm -rf ${PACKAGE}
	rm -f ${PACKAGE}.zip ${PACKAGE}.tar ${PACKAGE}.tar.bz2

package: clean
	mkdir ${PACKAGE}
	cp -r *.el Makefile *.md COPYING.* doc dict ${PACKAGE}

package.tar: package
	tar cf ${PACKAGE}.tar ${PACKAGE}

package.tar.bz2: tar
	bzip2 ${PACKAGE}.tar

package.zip: package
	zip -r ${PACKAGE}.zip ${PACKAGE}

elpa: $(ELPA_DIR)
$(ELPA_DIR): Cask
	$(CASK) install
	touch $@
