;;; let's hide the loading
(let ((*standard-output* (make-broadcast-stream)))
  (require 'asdf)
  #+sbcl
  (require 'sb-bsd-sockets)
  #+ecl
  (require 'sockets))

;;;; C binding to get terminal informations
;;;; SBCL only
#+sbcl
(progn
  (load-shared-object #p"./extension.so")
  ;; getTerminalHeight
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
    #include <unistd.h>
    int ttyPredicate() {
      return isatty(fileno(stdout)); }
    unsigned int getTerminalHeight()  {
      struct winsize w;
      return ioctl(1,TIOCGWINSZ,&w)<0?UINT_MAX:w.ws_row;}")
  (ffi:def-function
      ("getTerminalHeight" c-termsize)
      () :returning :unsigned-int)
  (ffi:def-function
      ("ttyPredicate" c-ttyp)
      () :returning :int))
;;;; END C binding

;; structure to store links
(defstruct location host port type uri
           :predicate)

;;;; BEGIN GLOBAL VARIABLES

;;; array of lines in buffer
(defparameter *buffer* nil)
;;; array of lines of last menu
(defparameter *previous-buffer* nil)

;;; boolean if we are interactive or not
(defparameter *not-interactive* nil)

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
(add-color 'red        1 31)
(add-color 'reset      0 70)
(add-color 'bg-black   0 40)
(add-color 'folder     4 34)
(add-color 'green      1 32)
(add-color 'file       0 35)
(add-color 'cyan       0 46)
(add-color 'http       0 33)
;;;; END ANSI colors

;;;; is the output interactive or a pipe ?
(defun ttyp()
  "return t if the output is a terminal"
  ;; we use this variable in case we don't want to be interactive
  ;; like when we use a cmd arg to get an image
  #+sbcl
  (interactive-stream-p *standard-output*)
  #+ecl
  (if (= 1 (c-ttyp))
      t
      nil))

(defun copy-array(from)
  "return a new array containing the same elements as the parameter"
  (let ((dest (make-array 200
                          :fill-pointer 0
                          :initial-element nil
                          :adjustable t)))
    (loop for element across from
       do
         (vector-push element dest))
    dest))

(defun print-with-color(text &optional (color 'reset) (line-number nil))
  "Used to display a line with a color"
  (format t "~3A| ~a~a~a~%" (if line-number line-number "") (get-color color) text (get-color 'reset)))

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
        (field     (split (subseq line 1) #\Tab)))

    ;; if split worked
    (when (= (length field) 4)
      (let ((line-number (+ 1 (hash-table-count *links*)))
            (text (car field))
            (uri  (cadr field))
            (host (caddr field))
            (port (parse-integer (cadddr field))))

        ;; see RFC 1436
        ;; section 3.8
        (if (member line-type *allowed-selectors* :test #'equal)
            (progn

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
                     (setf (gethash line-number *links*)
                           (make-location :host host :port port :uri uri :type line-type ))
                     (print-with-color text 'red line-number))

              ;; 8 Telnet session
              (check "8"
                     (print-with-color "selector 8 not implemented" 'red))

              ;; 9 Binary
              (check "9"
                     (setf (gethash line-number *links*)
                           (make-location :host host :port port :uri uri :type line-type ))
                     (print-with-color text 'red line-number))

              ;; + redundant server
              (check "+"
                     (print-with-color "selector + not implemented" 'red))

              ;; T text based tn3270 session
              (check "T"
                     (print-with-color "selector T not implemented" 'red))

              ;; g GIF file
              (check "g"
                     (setf (gethash line-number *links*)
                           (make-location :host host :port port :uri uri :type line-type))
                     (print-with-color text 'red line-number))

              ;; I image
              (check "I"
                     (setf (gethash line-number *links*)
                           (make-location :host host :port port :uri uri :type line-type ))
                     (print-with-color text 'red line-number))

              ;; h http link
              (check "h"
                     (setf (gethash line-number *links*) uri)
                     (print-with-color text 'http line-number))) ;;;; end of known types

            ;; unknown type
            (print-with-color (format nil
                                      "invalid type ~a : ~a" line-type text)
                              'red))))))

(defun getpage(host port uri &optional (binary nil) (search nil))
  "send a request and store the answer (in *buffer* if text or save a file if binary)"

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
    (let ((stream (sb-bsd-sockets:socket-make-stream socket
                                                     :input t
                                                     :output t
                                                     :element-type :default)))
      ;; sending the request to the server
      (if search
          (progn
            (format t "Input : ")
            (let ((user-input (read-line nil nil)))
              (format stream "~a	~a~a~a" uri user-input #\Return #\Newline)))
          (format stream "~a~a~a" uri #\Return #\Newline))
      (force-output stream)

      (if binary
          ;; binary

          ;; in terminal = save the file
          ;; not terminal = write to stdio
          (if (ttyp)
              ;; save into a file in /tmp
              (let* ((filename (subseq uri (1+ (position #\/ uri :from-end t))))
                     (path (concatenate 'string "/tmp/" filename)))
                (with-open-file (output path
                                        :element-type '(unsigned-byte 8)
                                        :direction :output :if-exists :supersede)
                  (let ((buf (make-array 4096 :element-type '(unsigned-byte 8))))
                    (loop for pos = (read-sequence buf stream)
                       while (plusp pos)
                       do
                         (format t ".")
                         (force-output)
                         (write-sequence buf output :end pos)))
                  (format t "~%File downloaded into ~a (~a bytes)~%" path (file-length output))))

              ;; write to the standard output
              (let ((buf (make-array 4096 :element-type '(unsigned-byte 8))))
                (loop for pos = (read-sequence buf stream)
                   while (plusp pos)
                   do
                     (write-sequence buf *standard-output* :end pos))))
          ;; not binary
          ;; for each line we receive we store it in *buffer*
          (loop for line = (read-line stream nil nil)
             while line
             do
               (vector-push line *buffer*))))))

(defun g(key)
  "browse to the N-th link"
  (let ((destination (gethash key *links*)))
    (when destination
      (cond
        ;; visit a gopher link
        ((location-p destination)
         (visit destination))
        ;; visit http link
        ((search "URL:" destination)
         (uiop:run-program (list "xdg-open"
                                 (subseq destination 4))))))))

(defun filter-line(text)
  "display only lines containg text"
  (setf *previous-buffer* (copy-array *buffer*))
  (setf *buffer* (make-array 200
                             :fill-pointer 0
                             :initial-element nil
                             :adjustable t))
  ;; we create a new buffer from the current
  ;; with only lines matching the string (no regex)
  (loop for line across *previous-buffer*
     do
       (when (search text (car (split (subseq line 1) #\Tab)) :test #'char-equal)
         (vector-push line *buffer*)))

  (display-buffer "1"))


(defun p()
  "browse to the previous link"
  (when (<= 2 (length *history*))
    (pop *history*)
    (visit (pop *history*))))

(defun r()
  "browse to the previous link"
  (when (<= 1 (length *history*))
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
     while bookmark
     do
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
  (format t "number            : go to link n~%")
  (format t "p or /            : go to previous page~%")
  (format t "h                 : display history~%")
  (format t "b or -            : display bookmarks and choose a link from it~%")
  (format t "a or +            : add a bookmark~%")
  (format t "r or *            : reload the page~%")
  (format t "help              : show this help~%")
  (format t "d                 : dump the raw reponse~%")
  (format t "/ text            : display online lines matching text~%")
  (format t "^D or x or q or . : quit clic~%"))

(defun parse-url(url)
  "parse a gopher url and return a location"
  (let ((url (if (search "gopher://" url)
                 (subseq url 9)
                 url)))

    ;; splitting with / to get host:port and uri
    ;; splitting host and port to get them
    (let* ((infos      (split url #\/))
           (host-port (split (pop infos) #\:)))

      ;; create the location to visit
      (make-location  :host (pop host-port)

                      ;; default to port 70 if not supplied
                      :port (if host-port ;; <- empty if no port given
                                (parse-integer (car host-port))
                                70)

                      ;; if type is empty we default to "1"
                      :type (let ((type (pop infos)))
                              (if (< 0 (length type)) type "1"))

                      ;; glue remaining args between them
                      :uri (format nil "~{/~a~}" infos)))))

(defun get-argv()
  "Parse argv and return it"
  #+sbcl
  (cadr *posix-argv*)
  #+ecl
  (car (last (cdr (si::command-args)))))

(defun user-input(input)
  (cond
    ;; show help
    ((string= "help" input)
     (help-shell))

    ;; bookmark current link
    ((or
      (string= "a" input)
      (string= "+" input))
     (add-bookmark))

    ;; show bookmarks
    ((or
      (string= "b" input)
      (string= "-" input))
     (show-bookmarks))

    ((or
      (string= "*" input)
      (string= "ls" input)
      (string= "r" input))
     (r))

    ;; go to previous page
    ((or
      (string= "/" input)
      (string= "cd .." input)
      (string= "p" input))
     (p))

    ;; search a pattern in a menu
    ;; syntax /pattern
    ((and
      (search "/" input)
      (> (length input) 1))
     (filter-line (subseq input 1)))

    ;; same as previously
    ;; but with syntax / pattern
    ((= 0 (or (search "/ " input) 1))
     (filter-line (subseq input 2)))

    ;; dump raw informations
    ((string= "d" input)
     (loop for c across *buffer*
        do
          (format t "~a~%" c)))

    ;; exit
    ((or
      (eql nil input)
      (string= "." input)
      (string= "exit" input)
      (string= "x" input)
      (string= "q" input))
     'end)

    ;; show history
    ((string= "h" input)
     (format t "~{~a~%~}" *history*))

    ;; follow a link
    (t
     ;; we ignore error in case of bad input
     ;; just do nothing
     (ignore-errors
       (g (parse-integer input))))))

(defun display-buffer(type)
  "display the buffer"

  ;;;; stdout is a terminal or not ?
  (if (ttyp)
      ;;;; we are in interactive mode
      (cond
        ;;;; output is a text file ?
        ;;;; call the $PAGER !
       ((string= "0" type)
	         ;;; generate a string from *buffer* array
	(let* ((uri (location-uri (car *history*)))
	       (filename (subseq uri (1+ (position #\/ uri :from-end t))))
               (path (concatenate 'string "/tmp/" filename)))
          (with-open-file (output path
                                  :direction :output
                                  :if-does-not-exist :create
                                  :if-exists :supersede)
            (loop for line across *buffer*
               do
                 (format output "~a~%" line)))
          (uiop:run-program (list (or (uiop:getenv "PAGER") "less") path)
                            :input :interactive
                            :output :interactive))
	 ;; display last menu
	 (pop *history*)
	 (when *previous-buffer*
	   (setf *buffer* (copy-array *previous-buffer*))
	   (setf *links* (make-hash-table))
	   (display-buffer "1")))

        ;; image
        ((or
          (string= "I" type)
          (string= "9" type))
         (let ((location (car *history*)))
           (uiop:run-program (list "xdg-open"
                                   (concatenate 'string
                                                "/tmp/"
                                                (subseq ;; get the text after last /
                                                 (location-uri location)
                                                 (1+ (position #\/
                                                               (location-uri location)
                                                               :from-end t)))))))
         (pop *history*)
         (when *previous-buffer*
           (setf *buffer* (copy-array *previous-buffer*))
           (setf *links* (make-hash-table))
           (display-buffer "1")))


        ;;;; output is a menu ?
        ;;;; display the menu and split it in pages if needed
        ((or
          (string= "1" type)
          (string= "7" type))

         ;; we store the user input outside of the loop
         ;; so if the user doesn't want to scroll
         ;; we break the loop and then execute the command
         (let ((input nil))
           (let ((rows (- (c-termsize) 1))) ; -1 for command bar

             (loop for line across *buffer*
                counting line into row
                do
                  (formatted-output line)

                ;; split and ask to scroll or to type a command
                  (when (= row rows)
                    (setf row 0)
                    (format t "~a   press enter or a shell command ~a : "
                            (get-color 'bg-black)
                            (get-color 'reset))
                    (force-output)
                    (let ((first-input (read-char *standard-input* nil nil t)))
                      (cond
		       ((not first-input)
			(format t "~%") ;; display a newline
			(setf input "x") ;; we exit
			(loop-finish))
		       ((char= #\NewLine first-input)
			;; we hide previous line (prompt)
			(format t "'~C[A~C[K~C" #\Escape #\Escape #\return))
		       (t
			(unread-char first-input)
			(let ((input-text (format nil "~a" (read-line nil nil))))
			  (setf input input-text)
			  (loop-finish)))))))

             ;; in case of shell command, do it
             (if input
                 (user-input input)
                 (when (< (length *buffer*) rows)
                   (dotimes (i (- rows (length *buffer*)))
                     (format t "~%"))))))))

      ;; display and quit
      (loop for line across *buffer*
         do
           (format t "~a~%" line))))

(defun visit(destination)
  "visit a location"

  (cond

    ;; we retrieve text / lines
    ;; when type is 1 or 0
    ((or
      (string= "1" (location-type destination))
      (string= "0" (location-type destination)))
  
     (getpage (location-host destination)
              (location-port destination)
              (location-uri  destination)))

    ((string= "7" (location-type destination))
     (getpage (location-host destination)
              (location-port destination)
              (location-uri  destination)
              nil t))

    (t
     (getpage (location-host destination)
              (location-port destination)
              (location-uri  destination)
              t)))


  ;; we reset the links table ONLY if we have a new folder
  ;; we also keep the last menu buffer
  (when (string= "1" (location-type destination))
    (setf *previous-buffer* (copy-array *buffer*))
    (setf *links* (make-hash-table)))
  
  ;; goes to the history !
  (push destination *history*)
  
  (display-buffer (location-type destination)))

(defun display-prompt()
  (let ((last-page (car *history*)))
    (format t "gopher://~a:~a/~a~a / (P)rev (R)eload (B)ookmark (H)istory : "
            (location-host last-page)
            (location-port last-page)
            (location-type last-page)
            (location-uri last-page)))
  (force-output))

(defun shell()
  "Shell for user interaction"
  (display-prompt)

  ;; we loop until X or Q is typed
  (loop for input = (format nil "~a" (read-line nil nil))
	while (not (or
		    (string= "NIL" input) ;; ^D
		    (string= "exit" input)
		    (string= "x" input)
		    (string= "q" input)))
	do
	(when (eq 'end (user-input input))
	  (loop-finish))
	(display-prompt)))

(defun main()
  "fetch argument, display page and go to shell if type is 1"
  (let ((destination
         (let ((argv (get-argv)))
           (if argv
               ;; url as argument
               (parse-url argv)
               ;; default url
               (make-location :host "gopherproject.org" :port 70 :uri "/" :type "1")))))

    ;; if we don't ask a menu, not going interactive
    (if (not (string= "1" (location-type destination)))
        ;; not interactive
        (visit destination)

        ;; if user want to drop from first page we need
        ;; to look it here
      (when (not (eq 'end (visit destination)))
	;; we continue to the shell if we are in a terminal
	(when (ttyp)
	  (shell))))))

;; we allow ecl to use a new kind of argument
;; not sure how it works but that works
#+ecl
(defconstant +uri-rules+
  '(("*DEFAULT*" 1 "" :stop)))

(load-bookmark)
