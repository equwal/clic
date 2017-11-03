# Introduction

Clic (Common LISP Interactive Client) is a gopher client.

It currently works with **ecl** and **sbcl** compilers. Just load it
with sbcl like this

    sbcl --load clic.lisp

or with ecl

    ecl -load clic.lisp**.


# Requirements

You need a Common LISP interpreter like ecl or sbcl to use Clic.

If you want to produce a binary, you need ecl and a C compiler.

# Make a binary

If you have ecl and a C compiler, just type `make`

You will get a binary named *clic*.

The makefile is a wrapper that call **make-binary.lisp** with ecl.


I don't provide a way to generate a binary with sbcl because it
creates binaries larger than 10 Mb.

# Use it

By default *clic* will load the page **bitreich.org/1/** and make you
in "shell mode". Just type the number of a link to follow the link. If
you have seen a long text or multiple texts and you don't know what
links you can use, type **p** to show again the latest page with the
links. You can exit shell mode with **x**.

## Shell mode

- "a number" : follow the link "number"
- p : display the latest page with links you have seen
- x : quit shell mode

## Non shell-mode ##

### Fetch a page

- use `(getpage "hostname" 70 "/")` to fetch the root of a gopherspace
- one can use `(getpage "hostname" 70 "/sometext.txt" 0)` to tell it's a type 0
- using *getpage* you will have numbers on links, use (g numer) to request the link

## Variables

There is a hash-table named *links* with links available.

