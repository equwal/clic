(load "3rdparties/bundle.lisp")
(require :asdf)

;; added since ecl update otherwise ecl doesn't compile them
(require :usocket)
(require :cl+ssl)

;; load clic which is in $PWD
(push '*default-pathname-defaults* asdf:*central-registry*)

(in-package :cl-user)

(asdf:make-build "clic" :type :program
                 :monolithic t
                 :move-here "."
                 :prologue-code '(progn 
				   (ext:set-signal-handler ext:+sigint+ nil)
				   (require :asdf)
				   (require :sb-bsd-sockets))
                 :epilogue-code '(progn (handler-case (main)
                                          (condition () (quit)))))
(quit)
