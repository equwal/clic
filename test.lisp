;; we can write scenario here

(print (parse-url "gopher://perso.pw:70/0/"))
(print (parse-url "gopher://perso.pw/0/a"))
(print (parse-url "perso.pw/0/some/uri.txt"))
(print (parse-url "perso.pw"))
(print (parse-url "perso.pw:70"))


(setf *bookmark-file* "bookmark-test")
(load-bookmark)
(p)
(g 1)
(add-bookmark)
(getpage "bitreich.org" 70 "/")
(display-buffer "1")
(g 7) ;; going to radio
(g 1) ;; going back
(g 21) ;; banana !
(p) ;; going back
(g 15)
(g 1)
(p)
(p)
(add-bookmark)
(show-bookmarks)
(g 1)

(print *links*)

(print *history*)
(format t "~%")


(quit)
