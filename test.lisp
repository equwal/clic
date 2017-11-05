;; we can write scenario here

(print (parse-url "gopher://perso.pw:70/0/"))
(print (parse-url "gopher://perso.pw/0/a"))
(print (parse-url "perso.pw/0/some/uri.txt"))
(print (parse-url "perso.pw"))
(print (parse-url "perso.pw:70"))

(p)
(g 2)
(getpage "bitreich.org" 70 "/")
(g 11) ;; going to radio
(g 35) ;; going back
(g 55) ;; banana !
(g 0) ;; going back
(g 26)
(g 1)
(p)
(p)

(print *history*)
(format t "~%")


(quit)
