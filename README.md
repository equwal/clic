# Introduction

Clic (Common LISP Interactive Client) is a gopher client. The name is
a bad pun because **clic** is the sound of a mouse click while this
client is keyboard only...

It currently works with **ecl** and **sbcl** compilers. Just load it
with sbcl like this

    sbcl --load clic.lisp

or with ecl

    ecl --load clic.lisp**.

# Requirements

You need a Common LISP interpreter like ecl or sbcl to use Clic.

If you want to produce a binary, you need ecl and a C compiler.

# Make a binary

## Linked binary requiring ECL

If you have ecl and a C or C++ compiler, just type `make`. You will
get a binary named *clic*. The makefile is a wrapper that call
**make-binary.lisp** with ecl.

## Static binary (standalone)

If you want to deploy **clic** without installing sbcl or ecl, you can
create a standalone executable (10 Mb approximately) with sbcl.

    make standalone

# Use it

By default *clic* will load the page **bitreich.org/1/** and make you
in "shell mode". Just type the number of a link to follow the link. If
you have seen a long text or multiple texts and you don't know what
links you can use, type **p** to show again the latest page with the
links. You can exit shell mode with **x**.

## Shell mode

- "a number" : follow the link "number"
- p : previous page
- a : add to bookmark (it saves the file too)
- b : display bookmarks and choose a link
- h : display history
- x : quit shell mode
- q : quit shell mode

## Non shell-mode ##

### Fetch a page

- use `(getpage "hostname" 70 "/")` to fetch the root of a gopherspace
- one can use `(getpage "hostname" 70 "/sometext.txt" 0)` to tell it's a type 0
- using *getpage* you will have numbers on links, use (g numer) to request the link

## Variables

There is a hash-table named *links* with links available.

