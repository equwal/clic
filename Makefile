# clic – a simple gopher client in lisp
# See the LICENSE file for copyright and license details.
.POSIX:

BIN    = clic
LISP   = ecl
PREFIX = /usr
BINDIR = ${PREFIX}/bin
MANDIR = ${PREFIX}/share/man/man1

all: ${BIN}

${BIN}: clic.lisp make-binary.lisp
	${LISP} --load make-binary.lisp

standalone: clic.lisp extension make-binary.lisp
	${MAKE} -e LISP=sbcl

extension: extension.c
	cc -fPIC -c extension.c
	ld -shared -o extension.so extension.o

install: ${BIN}
	@echo installing executable to "${DESTDIR}${PREFIX}/bin"
	@mkdir -p "${DESTDIR}${BINDIR}"
	@cp -f clic "${DESTDIR}${BINDIR}/${BIN}"
	@chmod 755 "${DESTDIR}${BINDIR}/${BIN}"

uninstall:
	@echo removing executable file from "${DESTDIR}${PREFIX}/bin"
	@rm -f "${DESTDIR}${BINDIR}/${BIN}"

clean:
	rm -f "${BIN}" clic.o clic.eclh clic.cxx bookmark-test extension.so

test: clean all
	@sh run-test.sh ${LISP}


.PHONY: all install uninstall clean
