smart-print
===========
*The pretty-printing library which feels natural to use.*

Inspired by [PPrint](http://gallium.inria.fr/~fpottier/pprint/) of [François Pottier](http://pauillac.inria.fr/~fpottier/) and [Nicolas Pouillard](http://nicolaspouillard.fr/). What it gives you:
* a generic pretty-printing library in OCaml
* a simple set of document combinators
* no multiple space and no trailing space
* three printing modes: no splitting, splitting only when necessary, splitting at all spaces

Install
-------
Download and run:

    make
    make install

You can also `make doc` and `make test`. To compile your program with SmartPrint:

    ocamlbuild my_program.native -libs smart_print

Do not forget to open the SmartPrint module in your code:

    open SmartPrint

Hello world
-----------
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

Useless spaces are automatically removed:

    to_stdout 25 (!^ "hello" ^^ space ^^ space ^^ !^ "word" ^^ space ^^ newline);;

is also:

    hello word

(no trailing space). If the string is longer:

    to_stdout 25 (words "A long string with many spaces." ^^ newline);;

gives:

    A long string with many
    spaces.

Complete example
----------------
We now build a pretty-printer for a small functional language. Let its syntax be:

    type t =
      | Var of string
      | App of t * t
      | Fun of string * t
      | Let of string * t * t
      | Tuple of t list;;

A pretty-printer is a recursive function:

    let rec pp (e : t) : SmartPrint.t =
      match e with
      | Var x -> !^ x
      | App (e1, e2) -> !^ "(" ^-^ pp e1 ^^ pp e2 ^-^ !^ ")"
      | _ -> failwith "TODO";;

The `^-^` concatenates with no space, `^^` with one space. `a ^^ b` is like `a ^-^ space ^-^ b`.

    to_stdout 25 (pp (Var "x"));;
    to_stdout 25 (pp (App (Var "f", Var "x")));;
    to_stdout 25 (pp (App (Var "fdsgoklkmeee", App (Var "ffedz", Var "xetgred"))));;

displays:

    x
    (f x)
    (fdsgoklkmeee (ffedz
    xetgred))

We would prefer to indent the last line:

    let rec pp (e : t) : SmartPrint.t =
      match e with
      | Var x -> !^ x
      | App (e1, e2) -> !^ "(" ^-^ pp e1 ^^ nest 2 (pp e2) ^-^ !^ ")"
      | _ -> failwith "TODO";;
    
    to_stdout 25 (pp (App (Var "fdsgoklkmeee", App (Var "ffedz", Var "xetgred"))));;

gives:

    (fdsgoklkmeee
      (ffedz xetgred))

The `nest 2` idents by two spaces and groups its parameter. Groups are like parenthesis for pretty-printing: they control the priority of spaces to know where to cut first. When we indent we always group. To make it perfect, group everything and use the more idiomatic `parens`:

    let rec pp (e : t) : SmartPrint.t =
      match e with
      | Var x -> !^ x
      | App (e1, e2) -> group (parens (pp e1 ^^ nest 2 (pp e2)))
      | _ -> failwith "TODO";;

Now the `Fun` and `Let` cases are easy for you:

    let rec pp (e : t) : SmartPrint.t =
      match e with
      | Var x -> !^ x
      | App (e1, e2) -> group (parens (pp e1 ^^ nest 2 (pp e2)))
      | Fun (x, e) -> group (parens (!^ "fun" ^^ !^ x ^^ !^ "->" ^^ nest 2 (pp e)))
      | Let (x, e1, e2) ->
        group (!^ "let" ^^ !^ x ^^ !^ "=" ^^ nest 2 (pp e1) ^^ !^ "in" ^^ newline ^^ pp e2)
      | _ -> failwith "TODO";;

    to_stdout 25 (pp (Fun ("f", Fun ("x", App (Var "f", Var "x")))));;
    to_stdout 25 (pp (Let ("x", Var "x", Var "y")));;
    to_stdout 25 (pp (Let ("x", Fun ("x", App (Var "fdsgo", App (Var "x", Var "xdsdg"))), Var "y")));;

writes:

    (fun f ->
      (fun x -> (f x)))
    let x = x in
    y
    let x =
      (fun x ->
        (fdsgo (x xdsdg))) in
    y

To display the `Tuple` we need repetition. It is possible to cheat using `OCaml.list`:

    let rec pp (e : t) : SmartPrint.t =
      match e with
      | Var x -> !^ x
      | App (e1, e2) -> group (parens (pp e1 ^^ nest 2 (pp e2)))
      | Fun (x, e) -> group (parens (!^ "fun" ^^ !^ x ^^ !^ "->" ^^ nest 2 (pp e)))
      | Let (x, e1, e2) ->
          group (!^ "let" ^^ !^ x ^^ !^ "=" ^^ nest 2 (pp e1) ^^ !^ "in" ^^ newline ^^ pp e2)
      | Tuple es -> OCaml.list pp es;;
    
    to_stdout 25 (pp (Tuple []));;
    to_stdout 25 (pp (Tuple [Var "x"; Var "y"]));;
    to_stdout 25 (pp (Tuple (List.map (fun x -> Var x) ["kjh"; "lj"; "iop"; "rt"; "vbn"; "hjk"; "gkgytuuhi"])));;

shows:

    [ ]
    [ x; y ]
    [
      kjh;
      lj;
      iop;
      rt;
      vbn;
      hjk;
      gkgytuuhi
    ]

Or we can use the `separate` combinator, which separates each element in a list of documents by a separator:

    let rec pp (e : t) : SmartPrint.t =
      match e with
      | Var x -> !^ x
      | App (e1, e2) -> group (parens (pp e1 ^^ nest 2 (pp e2)))
      | Fun (x, e) -> group (parens (!^ "fun" ^^ !^ x ^^ !^ "->" ^^ nest 2 (pp e)))
      | Let (x, e1, e2) ->
          group (!^ "let" ^^ !^ x ^^ !^ "=" ^^ nest 2 (pp e1) ^^ !^ "in" ^^ newline ^^ pp e2)
      | Tuple es -> parens (nest 2 (space ^^ separate (!^ "," ^^ space) (List.map pp es) ^^ space));;

We now get:

    ( )
    ( x, y )
    ( kjh, lj, iop, rt, vbn,
      hjk, gkgytuuhi )

We may prefer to get the last tuple on a column rather than on two lines. Change the splitting policy to "all" to break all spaces using `nest_all`:

    let rec pp (e : t) : SmartPrint.t =
      match e with
      | Var x -> !^ x
      | App (e1, e2) -> group (parens (pp e1 ^^ nest 2 (pp e2)))
      | Fun (x, e) -> group (parens (!^ "fun" ^^ !^ x ^^ !^ "->" ^^ nest 2 (pp e)))
      | Let (x, e1, e2) ->
          group (!^ "let" ^^ !^ x ^^ !^ "=" ^^ nest 2 (pp e1) ^^ !^ "in" ^^ newline ^^ pp e2)
      | Tuple es -> parens (nest_all 2 (space ^^ separate (!^ "," ^^ space) (List.map pp es) ^^ space));;

We correclty get:

    ( )
    ( x, y )
    (
      kjh,
      lj,
      iop,
      rt,
      vbn,
      hjk,
      gkgytuuhi
    )

Concepts
--------
Internally, documents are represented by a tree:

    type t =
      | String of string * int * int
      | Break of Break.t
      | GroupOne of int * t list
      | GroupAll of int * t list

Breaks can be spaces or newlines. During evaluation, spaces can be lifted to newlines to make each line shorter than the maximal width. They are two kinds of groups for two grouping policies: split only when necessary (default behavior), or try to print on one line else split everything. Each group can have an indentation level.

During printing, each space appearing after another space is ignored (no multiple space), as well as trailing spaces.

Differences with PPrint
-----------------------
If you come from [PPrint](http://gallium.inria.fr/~fpottier/pprint/), here are some differences you should notice:
* `^^` stands for `^/^`
* `^-^` stands for `^^`
* spaces are collapsing
* a `nest` is also a `group` (actually a `group` is just `nest 0`)
* there are three printing modes: flat, on need (by default, split only when necessary) and all (like the normal mode of PPrint)

Documentation
--------------
