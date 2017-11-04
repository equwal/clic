;; we can write scenario here

(p)
(g 2)
(getpage "bitreich.org" 70 "/")
(g 11) ;; going to radio
(g 35) ;; going back
(g 55) ;; banana !
(g 0) ;; going back
(p)
(p)

(print *history*)


(quit)
