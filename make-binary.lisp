(require 'cmp)
(require 'sockets)

(loop for file in '("clic" "clic.o")
      do (and (probe-file file) (delete-file file)))

(compile-file "clic.lisp" :system-p t)
(c:build-program "clic" :lisp-files '("clic.o"))
(delete-file "clic.o")

(format t "INFO => Compilation done (or at least it should)~%")
(quit)
