;;;; let's hide the loading
(let ((*standard-output* (make-broadcast-stream)))
  #+sbcl
  (require 'sb-bsd-sockets)
  #+ecl
  (require 'sockets))

;;;; C binding to get terminal informations
;;;; SBCL only
#+sbcl
(progn
  (load-shared-object #p"./extension.so")
  (declaim (inline getTerminalHeight))
  (sb-alien:define-alien-routine "getTerminalHeight" unsigned-int)
  (defun c-termsize ()
    "return terminal height"
    (sb-alien:with-alien ((res unsigned-int (getTerminalHeight))))))

#+ecl
(progn
  (ffi:clines "
    #include <sys/ioctl.h>
    #include <limits.h>
    unsigned int getTerminalHeight()  {
      struct winsize w; 
      return ioctl(1,TIOCGWINSZ,&w)<0?UINT_MAX:w.ws_row;}")
  (ffi:def-function
      ("getTerminalHeight" c-termsize)
      () :returning :unsigned-int))
;;;; END C binding

;; structure to store links
(defstruct location host port type uri)

;;;; BEGIN GLOBAL VARIABLES

;;; array of lines in buffer
(defparameter *buffer* nil)

;;; a list containing the last viewed pages
(defparameter *history*   '())

;;; a list containing the bookmarks
;;; altered by (add-bookmark) and (load-bookmark)
(defparameter *bookmarks* nil)

;;; when clic loads a type 1 page, we store location structures here
;;; when clic display the bookmark, we store bookmarks locations here
(defparameter *links*     (make-hash-table))

;;; Colors for use in the code
(defparameter *colors*    (make-hash-table))

;;; List of allowed item types
(defparameter *allowed-selectors*
  (list "0" "1" "2" "3" "4" "5" "6" "i"
	"h" "7" "8" "9" "+" "T" "g" "I"))

;;;; BEGIN CUSTOMIZABLE
;;; keep files visited on disk when t
(defparameter *offline* nil)

;;; name/location of the bookmark file
(defparameter *bookmark-file* "bookmark.lisp")
;;;; END CUSTOMIZABLE

;;;; END GLOBAL VARIABLES

;;;; BEGIN ANSI colors
(defun add-color(name type hue)
  "Storing a ANSI color string into *colors*"
  (setf (gethash name *colors*)
	(format nil "~a[~a;~am" #\Escape type hue)))

(defun get-color(name) (gethash name *colors*))
(add-color 'red    1 31)
(add-color 'white  0 70)
(add-color 'folder 4 34)
(add-color 'green  1 32)
(add-color 'file   0 35)
(add-color 'cyan   0 46)
(add-color 'http   0 33)
;;;; END ANSI colors

(defun print-with-color(text &optional (color 'white) (line-number nil))
  "Used to display a line with a color"
  (format t "~3A| ~a~a~a~%" (if line-number line-number "") (get-color color) text (get-color 'white)))

(defmacro check(identifier &body code)
  "Macro to define a new syntax to make 'when' easier for formatted-output function"
  `(progn (when (string= ,identifier line-type) ,@code)))

(defun split(text separator)
  "this function split a string with separator and return a list"
  (let ((text (concatenate 'string text (string separator))))
    (loop for char across text
	  counting char into count
	  when (char= char separator)
	  collect
	  ;; we look at the position of the left separator from right to left
	  (let ((left-separator-position (position separator text :from-end t :end (- count 1))))
	    (subseq text
		    ;; if we can't find a separator at the left of the current, then it's the start of
		    ;; the string
		    (if left-separator-position (+ 1 left-separator-position) 0)
		    (- count 1))))))

(defun formatted-output(line)
  "Used to display gopher response with color one line at a time"
  (let ((line-type (subseq line 0 1))
	(infos (split (subseq line 1) #\Tab)))

    ;; see RFC 1436
    ;; section 3.8
    (when (and
	   (= (length infos) 4)
	   (member line-type *allowed-selectors* :test #'equal))

      (let ((line-number (+ 1 (hash-table-count *links*)))
	    (text (car infos))
	    (uri  (cadr infos))
	    (host (caddr infos))
	    (port (parse-integer (cadddr infos))))

	;; RFC, page 4
	(check "i"
	       (print-with-color text))

	;; 0 text file
	(check "0"
	       (setf (gethash line-number *links*)
		     (make-location :host host :port port :uri uri :type line-type ))
	       (print-with-color text 'file line-number))

	;; 1 directory
	(check "1"
	       (setf (gethash line-number *links*)
		     (make-location :host host :port port :uri uri :type line-type ))
	       (print-with-color text 'folder line-number))

	;; 2 CSO phone-book
	;; WE SKIP
	(check "2")

	;; 3 Error
	(check "3"
	       (print-with-color "error" 'red line-number))

	;; 4 BinHexed Mac file
	(check "4"
	       (print-with-color text))

	;; 5 DOS Binary archive
	(check "5"
	       (print-with-color "selector 5 not implemented" 'red))

	;; 6 Unix uuencoded file
	(check "6" 
	       (print-with-color "selector 6 not implemented" 'red))

	;; 7 Index search server
	(check "7" 
	       (print-with-color "selector 7 not implemented" 'red))

	;; 8 Telnet session
	(check "8" 
	       (print-with-color "selector 8 not implemented" 'red))

	;; 9 Binary
	(check "9" 
	       (print-with-color "selector 9 not implemented" 'red))

	;; + redundant server
	(check "+" 
	       (print-with-color "selector + not implemented" 'red))

	;; T text based tn3270 session
	(check "T" 
	       (print-with-color "selector T not implemented" 'red))

	;; g GIF file
	(check "g" 
	       (print-with-color "selector g not implemented" 'red))

	;; I image
	(check "I"
	       (print-with-color "selector I not implemented" 'red))

	;; h http link
	(check "h"
	       (print-with-color (concatenate 'string
					      text " " uri)
				 'http "url"))))))

(defun getpage(host port uri)
  "connect and display"

  ;; we reset the buffer
  (setf *buffer*
	(make-array 200
		    :fill-pointer 0
		    :initial-element nil
		    :adjustable t))

  ;; we prepare informations about the connection
  (let* ((address (sb-bsd-sockets:get-host-by-name host))
	 (host (car (sb-bsd-sockets:host-ent-addresses address)))
	 (socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))

    (sb-bsd-sockets:socket-connect socket host port)

    ;; we open a stream for input/output
    (let ((stream (sb-bsd-sockets:socket-make-stream socket :input t :output t)))

      ;; sending the request here
      ;; if the selector is 1 we omit it
      (format stream "~a~%" uri)
      (force-output stream)

      ;; for each line we receive we display it
      (loop for line = (read-line stream nil nil)
	 while line
	 do
	   (vector-push line *buffer*)))))

(defun g(key)
  "browse to the N-th link"
  (let ((destination (gethash key *links*)))
    (when destination
      (visit destination))))

(defun p()
  "browse to the previous link"
  (when (<= 2 (length *history*))
    (pop *history*)
    (visit (pop *history*))))

(defun load-bookmark()
  "Restore the bookmark from file"
  (when (probe-file *bookmark-file*)
    (with-open-file (x *bookmark-file* :direction :input)
    (setf *bookmarks* (read x)))))

(defun save-bookmark()
  "Dump the bookmark to file"
  (with-open-file (x *bookmark-file*
		     :direction :output
		     :if-does-not-exist :create
		     :if-exists :supersede)
    (print *bookmarks* x)))

(defun add-bookmark()
  "Add a new bookmark"
  (push (car *history*) *bookmarks*)
  (save-bookmark))

(defun show-bookmarks()
  "display the bookmarks like a page"
  (setf *links* (make-hash-table))

  ;; for each bookmark we add it to *links*
  ;; and display it
  (loop for bookmark in *bookmarks*
     counting bookmark into line-number
     while bookmark do
       (progn
	 (setf (gethash line-number *links*)  bookmark)
	 (print-with-color (concatenate 'string
					(location-host bookmark)
					" "
					(location-type bookmark)
					(location-uri bookmark))
			   'file line-number))))
(defun help-shell()
  "show help for the shell"
  (format t "number : go to link n~%")
  (format t "p      : go to previous page~%")
  (format t "h      : display history~%")
  (format t "b      : display bookmarks and choose a link from it~%")
  (format t "a      : add a bookmark~%")
  (format t "help   : show this help~%")
  (format t "x or q : exit the shell, go back to REPL~%"))

(defun parse-url(url)
  "parse a gopher url and return a location"
  (let ((url (if (and
		  ;; if it contains more chars than gopher://
		  (<= (length "gopher://") (length url))
		  ;; if it starts with gopher// with return without it
		  (string= "gopher://" (subseq url 0 9)))
		 ;; we keep the url as is
		 (subseq url 9)
	       url)))

    ;; splitting by / to get host:port and uri
    (let ((infos (split url #\/)))

      ;; splitting host and port to get them
      (let ((host-port (split (pop infos) #\:)))

	;; create the location to visit
	(make-location  :host (pop host-port)

			;; default to port 70 if not supplied
			:port (if host-port
				  (parse-integer (car host-port))
				70)

			;; if type is empty we use "1"
			:type (let ((type (pop infos)))
				(if (< 0 (length type)) type "1"))

			;; glue remaining args between them
			:uri (format nil "~{/~a~}" infos))))))


(defun get-argv()
  "Parse argv and return it"
  #+sbcl
  (cadr *posix-argv*)
  #+ecl
  (car (last (cdr (si::command-args)))))

(defun user-input(input)
  (cond
    ;; show help
    ((string= "HELP" input)
     (help-shell))

    ;; bookmark current link
    ((string= "A" input)
     (add-bookmark))

    ;; show bookmarks
    ((string= "B" input)
     (show-bookmarks))

    ;; go to previous page
    ((string= "P" input)
     (p))

    ;; exit
    ((or (string= "X" input)
	 (string= "Q" input))
     (quit))

    ;; show history
    ((string= "H" input)
     (format t "~{~a~%~}" *history*))

    ;; follow a link
    (t
     ;; we ignore error in case of bad input
     ;; just do nothing
     (ignore-errors
       (g (parse-integer input))))))

(defun display-buffer(type)
  "display the buffer"
  (let ((rows (c-termsize)))
    (let ((input nil))
      (loop for line across *buffer*
	 counting line into row
	 do
	   (when (= row (- rows 1)) ; -1 for text displayed
	     (setf row 0)
	     (format t "~a   press enter or a shell command ~a : "
		     (get-color 'cyan)
		     (get-color  'white))
	     (force-output)
	     (let ((first-input (read-char)))
	       (when (not (char= #\NewLine first-input))
		 (unread-char first-input)
		 (let ((input-text (format nil "~a" (read))))
		   (setf input input-text)
		   (loop-finish)))))
	 (cond
	   ((string= "1" type)
	    (formatted-output line))
	   ((string= "0" type)
	    (format t "~a~%" line))))
      (when input
	(user-input input)))))

(defun visit(destination)
  "visit a location"

  (getpage (location-host destination)
	   (location-port destination)
	   (location-uri  destination))

  ;; we reset the links table ONLY if we have a new folder
  (when (string= "1" (location-type destination))
    (setf *links* (make-hash-table)))

  ;; goes to the history !
  (push destination *history*)

  (display-buffer (location-type destination))


  (when *offline*
    (let ((path (concatenate 'string
			     "history/" (location-host destination)
			     "/" (location-uri destination) "/")))
      (ensure-directories-exist path)

      (with-open-file
	  (save-offline (concatenate
			 'string  path (location-type destination))
			:direction :output
			:if-does-not-exist :create
			:if-exists :supersede)

	(loop for line in *buffer*
	   while line
	   do
	     (format save-offline "~a~%" line))))))

(defun shell()
  "Shell for user interaction"
  (format t "clic => ")
  (force-output)

  ;; we loop until X or Q is typed
  (loop for input = (format nil "~a" (read nil nil))
     while (not (or
		 (string= "X" input)
		 (string= "Q" input)))
     do
       (user-input input)
       (format t "clic => ")
       (force-output)))

(defun main()
  "fetch argument, display page and go to shell if type is 1"
  (let ((destination
	 (let ((argv (get-argv)))
	   (if argv
	       ;; url as argument
	       (parse-url argv)
	     ;; default url
	     (make-location :host "bitreich.org" :port 70 :uri "/" :type "1")))))
    (visit destination)
    (when (string= "1" (location-type destination))
      (shell))))

;; we allow ecl to use a new kind of argument
;; not sure how it works but that works
#+ecl
(defconstant +uri-rules+
  '(("*DEFAULT*" 1 "" :stop)))

(load-bookmark)
