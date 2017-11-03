;; let's hide the loading
(let ((*standard-output* (make-broadcast-stream)))
  #+sbcl
  (require 'sb-bsd-sockets)
  #+ecl
  (require 'sockets))

(defstruct location host port uri type)

(defun color(num1 num2)
  "generate string used to put ANSI color"
  (format nil "~a[~a;~am" #\Escape num1 num2))

(defparameter *links* (make-hash-table))
(defparameter *types* (list "0" "1" "2" "3" "4" "5" "6" "i"
			    "h" "7" "8" "9" "+" "T" "g" "I"))

;; ansi colors
(defparameter *red*           (color 1 31))
(defparameter *white*         (color 0 70))
(defparameter *color-folder*  (color 4 34))
(defparameter *green*         (color 1 32))
(defparameter *color-file*    (color 0 33))
(defparameter *cyan*          (color 0 46))

(defun print-with-color(text &optional (color *white*) (line-number nil))
  "Used to display a line with a color"
  (format t "~3A| ~a~a~a~%" (if line-number line-number "") color text *white*))

(defmacro check(identifier &body code)
  "Syntax to make a when easier for formatted-output func"
  `(progn (when (string= ,identifier line-type) ,@code)))

(defun split(text separator)
  "this function split a string with separator and return a list"
  (let ((text (concatenate 'string text (string separator))))
    (loop for char across text
	  counting char into count
	  when (char= char separator)
	  collect
	  
	  ;; we look at the position of the left separator
	  (let ((left-separator-position (position separator text :from-end t :end (- count 1))))
	    (subseq text
		    ;; if we can't find a separator at the left of the current, then it's the start of
		    ;; the string
		    (if left-separator-position (+ 1 left-separator-position) 0) 
		    (- count 1))))))

(defun formatted-output(line line-number)
  "Used to display gopher response with color one line at a time"
  (let ((line-type (subseq line 0 1))
	(infos (split (subseq line 1) #\Tab)))

    ;; see RFC 1436
    ;; section 3.8
    (when (and
	   (= (length infos) 4)
	   (member line-type *types* :test #'equal))

      (let ((text (car infos))
	    (uri  (cadr infos))
	    (host (caddr infos))
	    (port (parse-integer (cadddr infos))))

	
	;; RFC, page 4
	(check "i"
	       (print-with-color text))
	
	;; 0 file
	(check "0"
	       (setf (gethash line-number *links*)
		     (make-location :host host :port port :uri uri :type line-type ))
	       (print-with-color text *color-file* line-number))
	
	;; 1 directory
	(check "1"
	       (setf (gethash line-number *links*)
		     (make-location :host host :port port :uri uri :type line-type ))
	       (print-with-color text *color-folder* line-number))
	
	;; 2 CSO phone-book
	;; WE SKIP
	(check "2")
	
	;; 3 Error
	(check "3"
	       (print-with-color "error" *red* line-number))
	
	;; 4 BinHexed Mac file
	(check "4"
	       (print-with-color text))
	
	;; 5 DOS Binary archive
	(check "5" 'unimplemented)
	
	;; 6 Unix uuencoded file
	(check "6" 'unimplemented)
	
	;; 7 Index search server
	(check "7" 'unimplemented)
	
	;; 8 Telnet session
	(check "8" 'unimplemented)
	
	;; 9 Binary
	(check "9" 'unimplemented)
	
	;; + redundant server
	(check "+" 'unimplemented)
	
	;; T text based tn3270 session
	(check "T" 'unimplemented)
	
	;; g GIF file
	(check "g" 'unimplemented)
	
	;; h html link
	(check "h"
	       (print-with-color text *color-file* "url"))
	
	;; I image
	(check "I" 'unimplemented)))))

(defun getpage(host port uri &optional (type "1"))
  "connect and display"
  
  
  
  ;; we reset the links table
  ;; if we have a new folder
  (when (string= "1" type)
    (setf *links* (make-hash-table))
    (setf (gethash 0 *links*)
	  (make-location :host host :port port :uri uri :type type)))
  
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
	    counting line into line-number
	    while line
	    do
	    (cond ((string= "1" type)
		   (formatted-output line line-number))
		  
		  ((string= "0" type)
		   (format t "~a~%" line))))))
  (format t "~aRequested gopher://~a:~a/~a~a~a~%" *cyan* host port type uri *white*))

(defun g(key)
  "browse to the N-th link"
  (let ((destination (gethash key *links*)))
    (getpage (location-host destination)
	     (location-port destination)
	     (location-uri  destination)
	     (location-type destination))))

(defun help()
  "show help"
  (format t "HOW TO USE CLI !~%")
  (format t "(getpage \"host\" port \"uri\")~%")
  (format t "~%~%"))

(defun help-shell()
  "show help for the shell"
  (format t "number : go to link n~%")
  (format t "p      : go to previous menu~%")
  (format t "help   : show this help~%")
  (format t "x      : exit the shell, go back to REPL~%"))

(defun shell()
  "gNUM p h x"
  (format t "clic => ")
  (force-output)
  (loop for user-input = (format nil "~a" (read nil nil))
	while (not (string= "X" user-input))
	do
	(cond
	 ((string= "HELP" user-input)
	  (help-shell))
	 ((string= "P" user-input)
	  (g 0))
	 (t
	  (when user-input
	    (g (parse-integer user-input)))))
	(format t "clic => ")
	(force-output)))

(defun start()
  (getpage "bitreich.org" 70 "/")
  (shell))


