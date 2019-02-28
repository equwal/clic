;; ecl produces a linked binary to ecl shared library
(load "3rdparties/bundle.lisp")
(require 'usocket)
(require 'cl+ssl)
(require 'asdf)
(require 'cmp)

(progn
  (compile-file "clic.lisp" :system-p t)
  (c:build-program "clic"
                   :prologue-code '(ext:set-signal-handler ext:+sigint+ nil)
                   :epilogue-code '(progn (handler-case (main)
                                            (condition () (quit))))
                   :lisp-files '("clic.o")))

(format t "~%~%Compilation finished~%")
(quit)
