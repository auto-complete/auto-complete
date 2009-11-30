VERSION=`perl -ne 'print $$1 if /;; Version: (.*)/' auto-complete.el`
PACKAGE=auto-complete-${VERSION}

clean:
	@rm -rf ${PACKAGE}
	@rm -f ${PACKAGE}.zip ${PACKAGE}.tar.bz2 *.elc

package:
	@rm -rf ${PACKAGE}
	@mkdir ${PACKAGE}
	@cp *.el Makefile README TEST TODO ${PACKAGE}

tar.bz2: package
	@tar cjf ${PACKAGE}.tar.bz2 ${PACKAGE}

zip: package
	@zip -r ${PACKAGE}.zip ${PACKAGE}
