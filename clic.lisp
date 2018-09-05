;;; let's hide the loading
(let ((*standard-output* (make-broadcast-stream)))
  (require 'asdf)
  #+ecl
  (require 'sockets))

;;;; C binding to get terminal informations
#+ecl
(progn
  (ffi:clines "
    #include <sys/ioctl.h>
    #include <limits.h>
    #include <unistd.h>

    #ifdef __OpenBSD__
    void gotoPledge() {
       pledge(\"dns inet stdio rpath tty wpath cpath proc exec\",NULL);
    }

    void kioskPledge() {
       pledge(\"dns inet stdio tty rpath\",NULL);
    }
    #endif

    int ttyPredicate() {
      return isatty(fileno(stdout)); }
    unsigned int getTerminalHeight()  {
      struct winsize w;
      return ioctl(1,TIOCGWINSZ,&w)<0?UINT_MAX:w.ws_row;}")
  #+openbsd
  (progn
    (ffi:def-function
     ("kioskPledge" c-kiosk-pledge)
     () :returning :void)
    (ffi:def-function
     ("gotoPledge" c-pledge)
     () :returning :void))
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

;;;; kiosk mode 
(defparameter *kiosk-mode* nil)

(defmacro kiosk-mode(&body code)
  "prevent code if kiosk mode is enabled"
  `(progn
     (when (not *kiosk-mode*)
       ,@code)))

;;;; BEGIN GLOBAL VARIABLES

;;; array of lines in buffer
(defparameter *buffer* nil)
;;; array of lines of last menu
(defparameter *previous-buffer* nil)

;;; bandwidth usage counter
(defparameter *total-bandwidth-in* 0)
(defparameter *last-bandwidth-in* 0)

;;; a list containing the last viewed pages
(defparameter *history*   '())

;;; contain duration of the last request
(defparameter *duration* 0)

;;; when clic loads a type 1 page, we store location structures here
(defparameter *links*     (make-hash-table))

;;; Colors for use in the code
(defparameter *colors*    (make-hash-table))

;;; List of allowed item types
(defparameter *allowed-selectors*
  (list "0" "1" "2" "3" "4" "5" "6" "i"
        "h" "7" "8" "9" "+" "T" "g" "I"))

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

(defun clear()
  "Clear the screen"
  (format t "~A[H~@*~A[J" #\escape))

;;;; is the output interactive or a pipe ?
(defun ttyp()
  "return t if the output is a terminal"
  ;; we use this variable in case we don't want to be interactive
  ;; like when we use a cmd arg to get an image
  #+ecl
  (if (= 1 (c-ttyp))
      t
      nil))

(defun copy-array(from)
  "return a new array containing the same elements as the parameter"
  (let ((dest (make-array (length from)
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

(defmacro foreach-buffer(&body code)
  `(progn
     (loop for line across *buffer* do ,@code)))          

(defmacro easy-socket(&body code)
  "avoid duplicated code used for sockets"
  `(progn
     (let* ((address (sb-bsd-sockets:get-host-by-name host))
            (host (car (sb-bsd-sockets:host-ent-addresses address)))
            (socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))

       (sb-bsd-sockets:socket-connect socket host port)

       ;; we open a stream for input/output
       (let ((stream (sb-bsd-sockets:socket-make-stream socket
                                                        :input t
                                                        :output t
                                                        :element-type :default)))
         ,@code))))

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
  
  ;; we check that the line is longer than 1 char and that it has tabs
  (when (and
         (< 1 (length line))
         (position #\Tab line))
    (let ((line-type (subseq line 0 1))
          (field      (split (subseq line 1) #\Tab)))

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
                                'red)))))))

(defun download-binary(host port uri)
  (easy-socket
   ;; sending the request to the server
   (format stream "~a~a~a" uri #\Return #\Newline)
   (force-output stream)

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
       (format t "~%File downloaded into ~a (~a bytes)~%" path (file-length output))))))


(defun getpage(host port uri &optional (search nil))
  "send a request and store the answer (in *buffer* if text or save a file if binary)"

  ;; we reset the buffer
  (setf *buffer*
        (make-array 200
                    :fill-pointer 0
                    :initial-element nil
                    :adjustable t))
  (setf *last-bandwidth-in* 0)

  (let ((real-time (get-internal-real-time)))
    ;; we prepare informations about the connection
    (easy-socket
     ;; sending the request to the server
     (if search
         (format stream "~a	~a~a~a" uri search #\Return #\Newline)
         (format stream "~a~a~a"       uri #\Return #\Newline))
     (force-output stream)

     ;; not binary
     ;; for each line we receive we store it in *buffer*
     (loop for line = (read-line stream nil nil)
        count line into lines
        while line
        do
	;; count bandwidth usage
        (incf *total-bandwidth-in* (length line))
        (incf *last-bandwidth-in* (length line))
        ;; increase array size if needed
          (when (= lines (- (array-total-size *buffer*) 1))
            (adjust-array *buffer* (+ 200 (array-total-size *buffer*))))
          (vector-push line *buffer*)))


    ;; we store the duration of the connection
    (setf *duration* (float (/ (- (get-internal-real-time) real-time)
                               internal-time-units-per-second)))))

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
         (kiosk-mode
	  (uiop:run-program (list "xdg-open"
				  (subseq destination 4)))))))))

(defun filter-line(text)
  "display only lines containg text"
  (setf *previous-buffer* (copy-array *buffer*))
  (setf *buffer* (make-array 400
                             :fill-pointer 0
                             :initial-element nil
                             :adjustable t))
  ;; we create a new buffer from the current
  ;; with only lines matching the string (no regex)
  (loop for line across *previous-buffer*
     do
       (when (search text (car (split (subseq line 1) #\Tab)) :test #'char-equal)
         (vector-push line *buffer*)))
  (display-interactive-menu))

(defun load-file-menu(path)
  "load a local file with a gophermap syntax and display it as a menu"
  ;; we set the buffer
  (setf *buffer*
        (make-array 200
                    :fill-pointer 0
                    :initial-element nil
                    :adjustable t))

  (with-open-file (stream path
                          :direction :input)
    (loop for line = (read-line stream nil nil)
       while line
       do
         (vector-push line *buffer*))))

(defun p()
  "browse back to previous menu"
  (when (<= 2 (length *history*))
    (pop *history*)
    (visit (pop *history*))))

(defun r()
  "reload the previous menu"
  (when (<= 1 (length *history*))
    (visit (pop *history*))))

(defun s(number)
  "show url for the link $NUMBER"
  (let ((destination (gethash number *links*)))
    (if (not destination)
        (format t "No link ~a~%" number)
        (format t "gopher://~a~a/~a~a~%"
                (location-host destination)
                (let ((port (location-port destination)))
                  (if (= 70 port)
                      ""
                      (format nil ":~a" port)))
                (location-type destination)
                (location-uri destination)))))

(defun help-shell()
  "show help for the shell"
  (format t "number            : go to link n~%")
  (format t "p or /            : go to previous page~%")
  (format t "h                 : display history~%")
  (format t "sNUMBER           : show url for link $NUMBER~%")
  (format t "r or *            : reload the page~%")
  (format t "help              : show this help~%")
  (format t "d                 : dump the raw reponse~%")
  (format t "/ text            : display online lines matching text~%")
  (format t "^D or x or q or . : quit clic~%"))

(defun parse-url(url)
  "parse a gopher url and return a location"
  (cond ((or
          (string= "--help" url)
          (string= "-h"     url))
         (help-shell)
         (quit))

        ((string= "-k" url)
         #+openbsd
         (c-kiosk-pledge)
         (setf *kiosk-mode* t))

        ((= 0 (or (search "file://" url) 1))
         (load-file-menu (subseq url 7))
         (make-location :host 'local-file
                        :port nil
                        :type "1"
                        :uri url))

        (t
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
                             :uri (format nil "~{/~a~}" infos)))))))

(defun get-argv()
  "Parse argv and return it"
  #+ecl
  (cdr (si::command-args)))

(defun user-input(input)
  (cond
    ;; show help
    ((string= "help" input)
     (help-shell))

    ((search "s" input)
     (s (parse-integer (subseq input 1))))

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
     (foreach-buffer
      (format t "~a~%" line)))

    ;; exit
    ((or
      (eql nil input)
      (string= "NIL" input)
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

(defun display-interactive-binary-file()
  "call xdg-open on the binary file"
  (kiosk-mode
   (let* ((location (car *history*))
	  (filename (subseq ;; get the text after last /
		     (location-uri location)
		     (1+ (position #\/
				   (location-uri location)
				   :from-end t))))
	  (filepath (concatenate 'string "/tmp/" (or filename "index"))))
     (uiop:run-program (list "xdg-open" filepath)))))
  
(defun display-text-stdout()
  "display the buffer to stdout"
  (foreach-buffer
   (format t "~a~%" line)))

(defun display-with-pager()
  "display the buffer using $PAGER"
  (let* ((uri (location-uri (car *history*)))
         (filename (subseq uri (1+ (position #\/ uri :from-end t))))
         (path (concatenate 'string "/tmp/" (or filename "index"))))
    (with-open-file (output path
                            :direction :output
                            :if-does-not-exist :create
                            :if-exists :supersede)
      (foreach-buffer
       (format output "~a~%" line)))
    (uiop:run-program (nconc
                       (if (uiop:getenv "PAGER")
                           (split (uiop:getenv "PAGER") #\Space)
                           (list "less"))
                       (list path))
                      :input :interactive
                      :output :interactive)))

;; display a text file using the pager by piping
;; the data to out, no temp file
(defun display-with-pager-kiosk()
  "display the buffer to stdout, we don't use system() in kiosk mode"
  (loop for line across *buffer*
	do
	(format t "~a~%" line)))

(defun display-interactive-menu()
  "display a menu"
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
             (format t "~a   press enter or a shell command: "
                     (if *kiosk-mode* "KIOSK" ""))
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
              (format t "~%")))))))

(defun pipe-text(host port uri)
  "pipe text to stdout, with stdout not a TTY output"
  (getpage host port uri)
  (foreach-buffer
   (format t "~a~%" line)))

(defun pipe-binary(host port uri)
  "pipe data to stdout, with stdout not a TTY output"
  (easy-socket
   (format stream "~a~a~a" uri #\Return #\Newline)
   (force-output stream)

   ;; write to the standard output
   (let ((buf (make-array 4096 :element-type '(unsigned-byte 8))))
     (loop for pos = (read-sequence buf stream)
        while (plusp pos)
        do
          (write-sequence buf *standard-output* :end pos)))))

(defun pipe-to-stdout(destination)
  "fetch data and output to stdout without storing anything"

  (if (or
       (string= "0" (location-type destination))
       (string= "1" (location-type destination))
       (string= "7" (location-type destination)))

      (pipe-text (location-host destination)
                 (location-port destination)
                 (location-uri  destination))

      (pipe-binary (location-host destination)
                   (location-port destination)
                   (location-uri  destination))))

(defun visit(destination)
  "fetch and display content interactively"

  (let ((type
         (cond

           ;; fetch a menu
           ((string= "1" (location-type destination))
            (if (eql 'local-file (location-host destination))
                'menu
                (getpage (location-host destination)
                         (location-port destination)
                         (location-uri  destination)))
            'menu)

           ;; fetch a text file
           ((string= "0" (location-type destination))
            (getpage (location-host destination)
                     (location-port destination)
                     (location-uri  destination))
            'text)

           ;; fetch a menu after search
           ((string= "7" (location-type destination))
            (format t "Input : ")
            (let ((user-input (read-line nil nil)))
              (getpage (location-host destination)
                       (location-port destination)
                       (location-uri  destination)
                       user-input))
            'menu)

           ;; if not type 0 1 7 then it's binary
           (t
	    (kiosk-mode
	     (download-binary (location-host destination)
			      (location-port destination)
			      (location-uri destination)))
            'binary))))


    ;; we reset the links table ONLY if we have a new menu
    ;; we also keep the last menu buffer
    (when (eql type 'menu)
      (setf *previous-buffer* (copy-array *buffer*))
      (setf *links* (make-hash-table)))

    ;; add it to the history !
    (push destination *history*)

    (if (eql type 'menu)
        (display-interactive-menu)
        (progn
          (if (eql type 'text)
              (if *kiosk-mode*
                  (display-with-pager-kiosk)
		  (display-with-pager))
	    (kiosk-mode (display-interactive-binary-file)))
	   ;; redraw last menu
	   ;; we need to get previous buffer and reset links numbering
	   (pop *history*)
	   (when (and
		  *previous-buffer*
		  (not *kiosk-mode*))
	     (setf *buffer* (copy-array *previous-buffer*))
	     (setf *links* (make-hash-table))
	     (display-interactive-menu))))))


(defun display-prompt()
  "show the prompt and helper"
  (let ((last-page (car *history*)))
    (format t "~agopher://~a:~a/~a~a (~as, ~aKb) / (p)rev (r)edisplay (h)istory : "
            (if *kiosk-mode* "KIOSK " "")
            (location-host last-page)
            (location-port last-page)
            (location-type last-page)
            (location-uri last-page)
            *duration*
	    (floor (/ *last-bandwidth-in* 1024.0))))
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
  "entry function of clic, we need to determine if the usage is one of
  the 3 following cases : interactive, not interactive or
  piped. Interactive is the state where the user will browse clic for
  multiple content. Not interactive is the case where clic is called
  with a parameter not of type 1, so it will fetch the content,
  display it and exit and finally, the redirected case where clic will
  print to stdout and exit."

  ;; pledge support on OpenBSD
  #+openbsd
  (c-pledge)

  ;; re-enable SIGINT (Ctrl+C) disabled for loading clic
  (ext:set-signal-handler ext:+sigint+ 'quit)

  (ignore-errors ;; lisp is magic
    (let ((destination (car (last
                             (loop for element in (get-argv)
                                collect (parse-url element))))))

      ;; if we didn't passed a url as parameter, use a default
      (if (not (location-p destination))
          (setf destination (make-location :host "gopherproject.org" :port 70 :uri "/" :type "1")))

      ;; is there an output redirection ?
      (if (ttyp)
          (progn
            (clear)
            ;; if we don't ask a menu, not going interactive
            (if (not (string= "1" (location-type destination)))
                ;; not interactive
                (visit destination)
                ;; if user want to drop from first page we need
                ;; to look it here
                (when (not (eq 'end (visit destination)))
                  ;; we continue to the shell if we are in a terminal
                  (shell)))
	    (format t "~a kB in.~%" (floor (/ *total-bandwidth-in* 1024.0))))
          (pipe-to-stdout destination)))))

;; we allow ecl to use a new kind of argument
;; not sure how it works but that works
#+ecl
(defconstant +uri-rules+
  '(("*DEFAULT*" 1 "" :stop)))
