smart-print
===========
*The pretty-printing library which feels natural to use.*

Inspired by [PPrint](http://gallium.inria.fr/~fpottier/pprint/) of [François Pottier](http://pauillac.inria.fr/~fpottier/) and [Nicolas Pouillard](http://nicolaspouillard.fr/). What it gives you:
* a generic pretty-printing library for OCaml
* a simple set of document combinators
* no multiple space and no trailing space
* three printing modes: no splitting, splitting only when necessary, splitting all spaces

Install
-------
Download and run:

    make
    make install

You can also `make doc` and `make test`. To compile your program with SmartPrint:

    ocamlbuild my_program.native -libs smart_print

Do not forget to open the SmartPrint module in your code:

    open SmartPrint

Examples
--------
Open an OCaml toplevel:

    ocaml

(or `rlwrap ocaml` for better key shorcuts). Load SmartPrint:

    #use "topfind";;
    #require "smart_print";;
    open SmartPrint;;

The classic *hello word*:

    to_stdout 25 (!^ "hello word");;

The maximal number of spaces per line is 25 so the output is:

    hello word

But if we try:

    to_stdout 6 (!^ "hello word");;

we also get:

    hello word

We need to specify where the breakable spaces are:

    to_stdout 6 (!^ "hello" ^^ !^ "word");;

gives:

    hello
    word

With an endline:

    to_stdout 6 (!^ "hello" ^^ !^ "word" ^^ newline);;

*SmartPrint needs to be aware of where spaces and newlines are, so you always need to tell him explicitly.*

If the string is longer:

    to_stdout 25 (words "A long string with many spaces." ^^ newline);;

gives:

    A long string with many
    spaces.


Concepts
--------

Differences with PPrint
-----------------------

Documentation
--------------
