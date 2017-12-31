# Introduction

Clic (Common LISP Interactive Client) is a gopher client. The name is
a bad pun because **clic** is the sound of a mouse click while this
client is keyboard only...

It currently works with **ecl** and **sbcl** compilers. 

`clic` binary must be compiled.

To compile it with **ecl** :

    make

To compile it with **sbcl** :

    make LISP=sbcl

then you can use `make install` to deploy it in `/usr/bin/`.

**I (the author) recommend using ecl**.

# Requirements

You need a Common LISP interpreter like ecl or sbcl to use Clic. This
is only tested with Linux, OpenBSD and FreeBSD, it should works fine
on any Unix system.

If you want to use ecl, you will need a C compiler.

# Information about the binary

If you compile clic with ecl, you will need ecl library installed on
the computer, the startup time is really fast. While compiling clic
with SBCL will provide a standalone binary embedding the whole SBCL
compiler, weighting approximately 10 Mb with a slower startup time.

If you use OpenBSD and SBCL, you will need wxallowed mountflag on the
partition from where you try to start clic standalone because sbcl has
a W^X issue.

# Use it

By default *clic* will load the page **gopherproject/1/** with a
number on the left of each link. Pleas type the number of a link to
follow it. If it's a text, the $PAGER program will be called to show
it, if it's a binary file (types g,I and 9) it will be downloaded into
`/tmp/` and then `xdg-open` will be called on the filename.

## Keyboard bindings

- 1-999 : follow the link "number"
- a : add to bookmark (it saves the file too)
- b : display bookmarks and choose a link
- p : previous page
- h : display history
- r : reload the page
- x : quit shell mode
- q : quit shell mode

In addition to the previous keybinding, a different layout coexists,
permitting to use clic with the numpad with only one hand :

- "a number" : follow the link "number
- / : previous page
- * : reload the page
- - : display bookmarks
- + : add to bookmark
- . : quit

# Command line

If you call clic with an argument which is a request for a binary
type, clic will output the data to stdout.

Using "gopher://" at the start of an url isn't mandatory.

