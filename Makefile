all: clean bin

bin:
	ecl -load make-binary.lisp

clean:
	rm clic clic.o

