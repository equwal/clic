(load "3rdparties/bundle.lisp")
(require :asdf)

;; load clic which is in $PWD
(push '*default-pathname-defaults* asdf:*central-registry*)

(in-package :cl-user)

(asdf:make-build "clic" :type :program
                 :monolithic t
                 :move-here "."
                 :prologue-code '(ext:set-signal-handler ext:+sigint+ nil)
                 :epilogue-code '(progn (handler-case (main)
                                          (condition () (quit)))))
(quit)
