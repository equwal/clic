;;(defpackage :clic
;;  (:use :cl :asdf)
;;  (:export #:main))
;;
;;(in-package :clic)

(defsystem "clic"
    :description "Command Line Interface Client"
    :version "1.1.1"
    :author "Solene Rapenne <solene@perso.pw>"
    :licence "MIT"
    :depends-on (:usocket :cl+ssl)
    :components ((:file "clic")))
