;; ecl produces a linked binary to ecl shared library
(require 'asdf)
#+ecl
(require 'cmp)
#+ecl
(require 'sockets)
#+ecl
(progn
  (compile-file "clic.lisp" :system-p t)
  (c:build-program "clic" :epilogue-code '(progn (handler-case (main)  (condition () (quit)))) :lisp-files '("clic.o")))

(format t "INFO => Compilation done (or at least it should be)~%")
(quit)
