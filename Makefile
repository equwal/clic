# clic – a simple gopher client in lisp
# See the LICENSE file for copyright and license details.
NAME = clic

PREFIX ?= /usr
BINDIR ?= ${PREFIX}/bin
MANDIR ?= ${PREFIX}/share/man/man1

all: bin

bin:
	ecl --load make-binary.lisp

install: all
	@echo installing executable to ${DESTDIR}${PREFIX}/bin
	@mkdir -p ${DESTDIR}${BINDIR}
	@cp -f ${NAME} ${DESTDIR}${BINDIR}
	@chmod 755 ${DESTDIR}${BINDIR}/${NAME}

uninstall:
	@echo removing executable file from ${DESTDIR}${PREFIX}/bin
	@rm -f ${DESTDIR}${BINDIR}/${NAME}

clean:
	rm -f clic clic.o clic.eclh clic.cxx

.PHONY: all install uninstall clean

