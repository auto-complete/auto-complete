VERSION=`perl -ne 'print $$1 if /;; Version: (.*)/' auto-complete.el`
PACKAGE=auto-complete-${VERSION}
EMACS=emacs
SITE=../auto-complete.github.com

lib/popup/popup.el:
	@echo 'Please place popup.el in lib/popup or do "git submodule init; git submodule update".'
	@exit 1

lib/fuzzy/fuzzy.el:
	@echo 'Please place fuzzy.el in lib/fuzzy or do "git submodule init; git submodule update".'
	@exit 1

check-dependency: lib/popup/popup.el lib/fuzzy/fuzzy.el

byte-compile: check-dependency
	${EMACS} -Q -L . -L lib/popup -L lib/fuzzy -batch -f batch-byte-compile auto-complete.el auto-complete-config.el

test: check-dependency
	${EMACS} -batch -Q -l tests/run-test.el

install:
	${EMACS} -Q -L . -batch -l etc/install ${DIR}

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

travis-ci:
	${EMACS} --version
	${EMACS} -batch -Q -l tests/run-test.el
