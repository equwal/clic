Introduction
============

Clic (Common LISP Interactive Client) is a terminal based gopher
client. The name is a bad pun, **clic** is the sound of a mouse click
but the software is keyboard driven...

Clic supports TLS connection by first trying to speak TLS to the remote
server, if it doesn't work it fallback to plaintext. The status prompt
will show either "**TLS**" or "UNSECURE" depending on how the communication
has been negociated.


Requirements
============

clic requires a few dependencies :

   + ANSI compatible terminal emulator
   + ecl common lisp interpreter
   + C compiler
   + Linux/OpenBSD/FreeBSD/NetBSD


How to build
============

`clic` binary must be compiled.

To compile it with **ecl**, it's really easy type the following
command :

    make

then you can use `make install` to deploy it in `/usr/bin/`.

The binary will be linked to ecl shared library. You need to install
ecl if you want to deploy clic binary on others systems.


How to use clic
===============

By default *clic* will load the page **gopherproject/1/** with a
number on the left of each link. Please type the number of a link to
follow it. If it's a text, the $PAGER program will be called to show
it, if it's a binary file (types g,I and 9) it will be downloaded into
`/tmp/` and then `xdg-open` will be called on the filename.


Keyboard bindings
-----------------

+ 1-999 : follow the link "number"
+ p : previous page
+ h : display history
+ r : reload the page
+ x or q or ^D : quit
+ sNUMBER : show the gopher url for link $number
+ / pattern : redisplay the menu only with lines containing string (no regex)
+ d : display the raw response

In addition to the previous keybinding, a different layout coexists,
permitting to use clic with the numpad with only one hand :

+ "a number" : follow the link "number
+ / : previous page
+ * : reload the page
+ . : quit


Command line usage
==================

clic [-k] [url|file]

If you start clic with -k parameter, then kiosk mode is enabled, which
mean it won't call any external program or save any data on the
disk. Texts (type 0) will be shown as-this in the output. It only
allow to use texts, menus and searches.

If you pass a gopher url to clic (gopher:// isn't mandatory for the
url), the behavor will change depending on two parameters :

1. is the output a pipe/redirection ?
2. is the url type a menu ? (types 1 or 7)

If the output is a pipe or a redirection, clic will send the raw data
to stdout (text for type 0 and binary for others types)

If the output is the terminal, clic will download the file in /tmp/
folder and then call $PAGER if the type is 0 (text) or xdg-open for
others types.

If the url is a type 0 or 7 and the output is a terminal, it will open
clic and stay in interactive mode.

Clic can open a local file respecting the gopher menu protocol, this
can be used to create a bookmark file and load it locally without a
gopher server. In order to proceed, you need to pass the file path as
a parameter beginning with file://, like the following example :

    clic file://path/to/my_file.txt
