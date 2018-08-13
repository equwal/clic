# clic – a simple gopher client in lisp
# See the LICENSE file for copyright and license details.
.POSIX:


BIN    = clic
LISP   = ecl
PREFIX = /usr
BINDIR = ${PREFIX}/bin
MANDIR = ${PREFIX}/share/man

all: ${BIN}

${BIN}:	clic.lisp make-binary.lisp
	ecl -load make-binary.lisp

install: ${BIN}
	@echo installing executable to "${DESTDIR}${PREFIX}/bin"
	@mkdir -p "${DESTDIR}${BINDIR}"
	@cp -f clic "${DESTDIR}${BINDIR}/${BIN}"
	@chmod 755 "${DESTDIR}${BINDIR}/${BIN}"
	@echo installing manual page to "${DESTDIR}${MANDIR}/man1"
	@mkdir -p "${DESTDIR}${MANDIR}/man1"
	@cp -f clic.1 "${DESTDIR}${MANDIR}/man1/clic.1"
	@chmod 644 "${DESTDIR}${MANDIR}/man1/clic.1"

uninstall:
	@echo removing executable file from "${DESTDIR}${PREFIX}/bin"
	@rm -f "${DESTDIR}${BINDIR}/${BIN}"
	@echo removing manual page from "${DESTDIR}${MANDIR}/man1"
	@rm -f "${DESTDIR}${MANDIR}/man1/clic.1"

clean:
	rm -f "${BIN}" clic.o clic.eclh clic.cxx

test: clean all
	@sh run-test.sh ${LISP}


.PHONY: all install uninstall clean
