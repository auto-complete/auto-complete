VERSION=`perl -ne 'print $$1 if /;; Version: (.*)/' auto-complete.el`
PACKAGE=auto-complete-${VERSION}

byte-compile:
	emacs -Q -L . -batch -f batch-byte-compile *.el

clean:
	rm -f *.elc
	rm -f doc/*.html
	rm -rf ${PACKAGE}
	rm -f ${PACKAGE}.zip ${PACKAGE}.tar.bz2 *.elc

package: clean
	rm -rf ${PACKAGE}
	mkdir ${PACKAGE}
	cp -r *.el Makefile README TEST TODO doc ${PACKAGE}

install: byte-compile
	for f in *.el *.elc; do cp -f $$f ${DIR}/; done
	cp -r dict ${DIR}/

tar.bz2: package
	tar cjf ${PACKAGE}.tar.bz2 ${PACKAGE}

zip: package
	zip -r ${PACKAGE}.zip ${PACKAGE}
