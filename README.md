Introduction
============

Clic (Common LISP Interactive Client) is a terminal based gopher
client. The name is a bad pun, **clic** is the sound of a mouse click
but the software is keyboard driven...


Requirements
============

clic requires a few dependencies :

   + ANSI compatible terminal emulator
   + a Common LISP interpreter
   + C compiler
   + Linux/OpenBSD/FreeBSD/NetBSD

Both **ecl** and **sbcl** Common LISP compilers are supported.


How to build
============

`clic` binary must be compiled.

To compile it with **ecl** :

    make

To compile it with **sbcl** :

    make LISP=sbcl

then you can use `make install` to deploy it in `/usr/bin/`.

Note : when using sbcl, a shared library extension.o is created and
then sbcl creates a binary linked against the library. But ecl will
translate the whole lisp code to C and then compile it, but linking
against ecl.

**I (the author) recommend using ecl**.


Information about the binary
----------------------------
If you compile clic with ecl, you will need ecl library installed on
the computer, the startup time is really fast. While compiling clic
with SBCL will provide a standalone binary embedding the whole SBCL
compiler, weighting approximately 10 Mb with a slower startup time.

If you use OpenBSD and SBCL, you will need wxallowed mountflag on the
partition from where you try to start clic standalone because sbcl has
a W^X issue.


How to use clic
===============

By default *clic* will load the page **gopherproject/1/** with a
number on the left of each link. Pleas type the number of a link to
follow it. If it's a text, the $PAGER program will be called to show
it, if it's a binary file (types g,I and 9) it will be downloaded into
`/tmp/` and then `xdg-open` will be called on the filename.


Keyboard bindings
-----------------

+ 1-999 : follow the link "number"
+ a : add to bookmark (it saves the file too)
+ b : display bookmarks and choose a link
+ p : previous page
+ h : display history
+ r : reload the page
+ x : quit shell mode
+ q : quit shell mode
+ / pattern : redisplay the menu only with lines containing pattern (no regex)
+ d : display the raw response

In addition to the previous keybinding, a different layout coexists,
permitting to use clic with the numpad with only one hand :

+ "a number" : follow the link "number
+ / : previous page
+ * : reload the page
+ - : display bookmarks
+ + : add to bookmark
+ . : quit


Command line usage
==================

If you pass a gopher url to clic (gopher:// isn't mandatory for the
url), the behavor will change depending on two parameters :

1. is the output a pipe/redirection ?
2. is the url type a menu ? (types 1 or 4)

If the output is a pipe or a redirection, clic will send the raw data
to stdout (text for type 0 and binary for others types)

If the output is the terminal, clic will download the file in /tmp/
folder and then call $PAGER if the type is 0 (text) or xdg-open for
others types.

If the url is a type 0 or 4 and the output is a terminal, it will open
clic and stay in interactive mode.
