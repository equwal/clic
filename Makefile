# clic – a simple gopher client in lisp
# See the LICENSE file for copyright and license details.
.POSIX:

BIN    = clic
LISP   = ecl
PREFIX = /usr
BINDIR = ${PREFIX}/bin
MANDIR = ${PREFIX}/share/man/man1

all: ${BIN}

${BIN}: clic.lisp
	${LISP} --load make-binary.lisp

install: ${BIN}
	@echo installing executable to "${DESTDIR}${PREFIX}/bin"
	@mkdir -p "${DESTDIR}${BINDIR}"
	@cp -f clic "${DESTDIR}${BINDIR}/${BIN}"
	@chmod 755 "${DESTDIR}${BINDIR}/${BIN}"

uninstall:
	@echo removing executable file from "${DESTDIR}${PREFIX}/bin"
	@rm -f "${DESTDIR}${BINDIR}/${BIN}"

clean:
	rm -f "${BIN}" clic.o clic.eclh clic.cxx

test:
	${LISP} --load clic.lisp --load test.lisp

.PHONY: all install uninstall clean
