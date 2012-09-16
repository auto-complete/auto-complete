VERSION=`perl -ne 'print $$1 if /;; Version: (.*)/' auto-complete.el`
PACKAGE=auto-complete-${VERSION}
EMACS=emacs

byte-compile:
	${EMACS} -Q -L . -batch -f batch-byte-compile auto-complete.el auto-complete-config.el

install:
	${EMACS} -Q -L . -batch -l etc/install ${DIR}

clean:
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
