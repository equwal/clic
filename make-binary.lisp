;; ecl produces a linked binary
;; while sbcl produces a static binary (but huge ~ 10Mb)

#+ecl
(require 'cmp)
#+ecl
(require 'sockets)
#+ecl
(progn
  (compile-file "clic.lisp" :system-p t)
  (c:build-program "clic" :epilogue-code '(progn (main)) :lisp-files '("clic.o")))
#+sbcl
(progn
  (require 'sb-bsd-sockets)
  (sb-ext:disable-debugger)
  (load "clic.lisp")
  #+sb-core-compression
  (sb-ext:save-lisp-and-die "clic"
			    :executable t
			    :compression 5
			    :toplevel 'main)
  #-sb-core-compression
  (sb-ext:save-lisp-and-die "clic"
			    :executable t
			    :toplevel 'main))

(format t "INFO => Compilation done (or at least it should)~%")
(quit)
