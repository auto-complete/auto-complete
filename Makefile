VERSION=`perl -ne 'print $$1 if /;; Version: (.*)/' auto-complete.el`
PACKAGE=auto-complete-${VERSION}
EMACS ?= emacs
EASK ?= eask
SITE=../auto-complete.github.com

ELPA_DIR = $(shell EMACS=$(EMACS) $(CASK) package-directory)

ci: compile install test

test:
	$(EASK) install-deps --dev
	$(EASK) test ert ./tests/run-test.el

compile:
	$(EASK) compile

install:
	$(EASK) package
	$(EASK) install

README.html: README.md
	pandoc --standalone --to html --output $@ $^

site: README.html
	(cd doc && make)
	cp README.html $(SITE)/index.html
	mkdir -p $(SITE)/doc
	cp doc/*.png doc/*.html doc/*.css $(SITE)/doc

clean:
	$(EASK) clean-all
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
