#!/bin/sh

LISP=$1

${LISP} --load clic.lisp --load test.lisp

./clic gopher://bitreich.org:70/0/  | md5sum -
echo "/" | nc bitreich.org 70 | md5sum -

