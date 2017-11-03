#+sbcl
(require 'sb-bsd-sockets)
#+ecl
(require 'sockets)


(defun color(num1 num2)
  "generate string used to put ANSI color"
  (format nil "~a[~a;~am" #\Escape num1 num2))

(defparameter *links* (make-hash-table))
(defparameter *types* (list "0" "1" "2" "3" "4" "5" "6" "i"
			    "h" "7" "8" "9" "+" "T" "g" "I"))

;; ansi colors
(defparameter *red*    (color 1 31))
(defparameter *white*  (color 0 70))
(defparameter *blue*   (color 4 34))
(defparameter *green*  (color 1 32))
(defparameter *yellow* (color 0 33))
(defparameter *cyan*   (color 0 46))

(defun print-with-color(text &optional (color *white*) (line-number nil))
  "Used to display a line with a color"
  (format t "~3A| ~a~a~a~%" (if line-number line-number "") color text *white*))

(defmacro check(identifier &body code)
  "Syntax to make a when easier for formatted-output func"
  `(progn
     (when (string= ,identifier line-type)
       ,@code)))

(defun split-tab(text)
  (if (position #\Tab text)
      (append
       (loop for char across text
	     counting char into count
	     when (char= char #\Tab)
	     collect
	     (subseq text
		     (let ((res (position #\Tab text :from-end t :end (- count 1))))
		       (if res
			   (+ 1 res)
			 0))
		     (- count 1)))
       (list
	(subseq text
		(+ 1 (position #\Tab text :from-end t))
		(- (length text) 1))))
    nil))

(defun formatted-output(line line-number)
  "Used to display gopher response with color one line at a time"
  (let ((line-type (subseq line 0 1))
	(infos (split-tab (subseq line 1))))

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
	       (setf (gethash line-number *links*) (list host port uri line-type ))
	       (print-with-color text *yellow* line-number))
	
	;; 1 directory
	(check "1"
	       (setf (gethash line-number *links*) (list host port uri line-type))
	       
	       (print-with-color text *blue* line-number))
	
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
	       (print-with-color text *blue* "url"))
	
	;; I image
	(check "I" 'unimplemented)))))

(defun getpage(host port uri &optional (type "1"))
  "connect and display"

  (format t "Asking gopher://~a:~a/~a~a~%" host port type uri)
  
  ;; we reset the links table
  ;; if we have a new folder
  (when (string= "1" type)
    (setf *links* (make-hash-table))
    (setf (gethash 0 *links*) (list host port uri type)))
  
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
  (format t "   ~a~80a~a~%" *cyan* " " *white*))

(defun g(key)
  "browse to the N-th link"
  (let ((infos (gethash key *links*)))
    (apply 'getpage infos)))


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

(defun start()
  (getpage "bitreich.org" 70 "/")
  (shell))

(defun shell()
  "gNUM p h x"
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
	    (g (parse-integer user-input)))))))

(help)
(help-shell)
(start)
