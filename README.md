SmartPrint
==========
A simple pretty-printing library.

Inspired by [PPrint](http://gallium.inria.fr/~fpottier/pprint/) of [François Pottier](http://pauillac.inria.fr/~fpottier/) and [Nicolas Pouillard](http://nicolaspouillard.fr/). What it gives you:
* a generic pretty-printing library in OCaml
* a simple set of document combinators
* multiple spaces collapsing and no trailing space
* two printing modes: splitting only when necessary, splitting at all spaces
* automatic indentation on splitting (if necessary)

Install
-------
Using the package manager [OPAM](http://opam.ocaml.org/):

    $ opam install smart-print

Else download the sources and run:

    $ make
    # make install

You can also `make doc` and `make test`. To compile your programs with SmartPrint
add `smart-print` to the `libraries` stanza in your `dune` file(s).

Do not forget to open the SmartPrint module in your code:

```ocaml
open SmartPrint
```

Hello world
-----------
Open an OCaml toplevel:

    $ ocaml

(or `rlwrap ocaml` for better key shorcuts). Load SmartPrint:

```ocaml
#use "topfind";;
#require "smart-print";;
open SmartPrint;;
```

The classic *hello word*:

```ocaml
to_stdout 25 2 (!^ "hello word");;
```

The maximal number of spaces per line is 25 and the tabulation size 2, so the output is:

    hello word

But if we try:

```ocaml
to_stdout 6 2 (!^ "hello word");;
```

we also get:

    hello word

We need to specify where the breakable spaces are:

```ocaml
to_stdout 6 2 (!^ "hello" ^^ !^ "word");;
```

gives:

    hello
    word

With an endline:

```ocaml
to_stdout 6 2 (!^ "hello" ^^ !^ "word" ^^ newline);;
```

*SmartPrint needs to be aware of where spaces and newlines are, so you always need to tell him explicitly.*

Useless spaces are automatically removed:

```ocaml
to_stdout 25 2 (!^ "hello" ^^ space ^^ space ^^ !^ "word" ^^ space ^^ newline);;
```

is also:

    hello word

(no trailing space). If the string is longer:

```ocaml
to_stdout 25 2 (words "A long string with many spaces." ^^ newline);;
```

gives:

    A long string with many
    spaces.

Complete example
----------------
We now build a pretty-printer for a small functional language. Let its syntax be:

```ocaml
type t =
  | Var of string
  | App of t * t
  | Fun of string * t
  | Let of string * t * t
  | Tuple of t list;;
```

A pretty-printer is a recursive function:

```ocaml
let rec pp (e : t) : SmartPrint.t =
  match e with
  | Var x -> !^ x
  | App (e1, e2) -> !^ "(" ^-^ pp e1 ^^ pp e2 ^-^ !^ ")"
  | _ -> failwith "TODO";;
```

The `^-^` concatenates with no space, `^^` with one space. `a ^^ b` is like `a ^-^ space ^-^ b`.

```ocaml
to_stdout 25 2 (pp (Var "x"));;
to_stdout 25 2 (pp (App (Var "f", Var "x")));;
to_stdout 25 2 (pp (App (Var "fdsgoklkmeee", App (Var "ffedz", Var "xetgred"))));;
```

displays:

    x
    (f x)
    (fdsgoklkmeee (ffedz
    xetgred))

We would prefer to indent the last line:

```ocaml
let rec pp (e : t) : SmartPrint.t =
  match e with
  | Var x -> !^ x
  | App (e1, e2) -> nest (!^ "(" ^-^ pp e1 ^^ pp e2 ^-^ !^ ")")
  | _ -> failwith "TODO";;

to_stdout 25 2 (pp (App (Var "fdsgoklkmeee", App (Var "ffedz", Var "xetgred"))));;
```

gives:

    (fdsgoklkmeee
      (ffedz xetgred))

The `nest` groups its parameter and indent each new line by two spaces. Groups are like parenthesis for pretty-printing: they control the priority of spaces to know where to cut first. To make it perfect, use the more idiomatic `parens`:

```ocaml
let rec pp (e : t) : SmartPrint.t =
  match e with
  | Var x -> !^ x
  | App (e1, e2) -> nest (parens (pp e1 ^^ pp e2))
  | _ -> failwith "TODO";;
```

Now the `Fun` and `Let` cases are easy for you:

```ocaml
let rec pp (e : t) : SmartPrint.t =
  match e with
  | Var x -> !^ x
  | App (e1, e2) -> nest (parens (pp e1 ^^ pp e2))
  | Fun (x, e) -> nest (parens (!^ "fun" ^^ !^ x ^^ !^ "->" ^^ pp e))
  | Let (x, e1, e2) ->
    nest (!^ "let" ^^ !^ x ^^ !^ "=" ^^ pp e1 ^^ !^ "in" ^^ newline ^^ pp e2)
  | _ -> failwith "TODO";;

to_stdout 25 2 (pp (Fun ("f", Fun ("x", App (Var "f", Var "x")))));;
to_stdout 25 2 (pp (Let ("x", Var "x", Var "y")));;
to_stdout 25 2 (pp (Let ("x", Fun ("x", App (Var "fdsgo",
  App (Var "x", Var "xdsdg"))), Var "y")));;
```

writes:

    (fun f ->
      (fun x -> (f x)))
    let x = x in
    y
    let x =
      (fun x ->
        (fdsgo (x xdsdg))) in
    y

To display the `Tuple` we need repetition. It is possible to cheat using `OCaml.list` to print a list in OCaml's syntax:

```ocaml
let rec pp (e : t) : SmartPrint.t =
  match e with
  | Var x -> !^ x
  | App (e1, e2) -> nest (parens (pp e1 ^^ pp e2))
  | Fun (x, e) -> nest (parens (!^ "fun" ^^ !^ x ^^ !^ "->" ^^ pp e))
  | Let (x, e1, e2) ->
    nest (!^ "let" ^^ !^ x ^^ !^ "=" ^^ pp e1 ^^ !^ "in" ^^ newline ^^ pp e2)
  | Tuple es -> OCaml.list pp es;;

to_stdout 25 2 (pp (Tuple []));;
to_stdout 25 2 (pp (Tuple [Var "x"; Var "y"]));;
to_stdout 25 2 (pp (Tuple (List.map (fun x -> Var x)
  ["kjh"; "lj"; "iop"; "rt"; "vbn"; "hjk"; "gkgytuuhi"])));;
```

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

```ocaml
let rec pp (e : t) : SmartPrint.t =
  match e with
  | Var x -> !^ x
  | App (e1, e2) -> nest (parens (pp e1 ^^ pp e2))
  | Fun (x, e) -> nest (parens (!^ "fun" ^^ !^ x ^^ !^ "->" ^^ pp e))
  | Let (x, e1, e2) ->
    nest (!^ "let" ^^ !^ x ^^ !^ "=" ^^ pp e1 ^^ !^ "in" ^^ newline ^^ pp e2)
  | Tuple es ->
    nest (parens (space ^^ separate (!^ "," ^^ space) (List.map pp es) ^^ space));;
```

We now get:

    ( )
    ( x, y )
    ( kjh, lj, iop, rt, vbn,
      hjk, gkgytuuhi )

We may prefer to get the last tuple on a column rather than on two lines. Change the splitting policy to "all" to break all spaces using `nest_all`:

```ocaml
  | Tuple es ->
    nest_all (parens (space ^^ separate (!^ "," ^^ space) (List.map pp es) ^^ space));;
```

We get:

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

In order not to indent the last parenthesis, we can put the parenthesis outside the `nest_all`:

```ocaml
let rec pp (e : t) : SmartPrint.t =
  match e with
  | Var x -> !^ x
  | App (e1, e2) -> nest (parens (pp e1 ^^ pp e2))
  | Fun (x, e) -> nest (parens (!^ "fun" ^^ !^ x ^^ !^ "->" ^^ pp e))
  | Let (x, e1, e2) ->
    nest (!^ "let" ^^ !^ x ^^ !^ "=" ^^ pp e1 ^^ !^ "in" ^^ newline ^^ pp e2)
  | Tuple es ->
    parens (nest_all (space ^^ separate (!^ "," ^^ space) (List.map pp es) ^^ space));;
```

We correctly get:

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

```ocaml
type t =
  | String of string * int * int
  | Break of Break.t
  | GroupOne of bool * t list
  | GroupAll of bool * t list
  | Indent of int * t
```

Breaks can be spaces or newlines. During evaluation, spaces can be lifted to newlines to make each line shorter than the maximal width. They are two kinds of groups for two grouping policies: split only when necessary (default behavior), or try to print on one line else split everything. Each group can indent when spaces are broken.

During printing, each space appearing after another space is ignored (no multiple space), as well as trailing spaces.

Differences with PPrint
-----------------------
If you come from [PPrint](http://gallium.inria.fr/~fpottier/pprint/), here are some differences you should notice:
* `^^` stands for `^/^`
* `^-^` stands for `^^`
* spaces are collapsing
* a `nest` is also a `group`, but indenting when spaces are broken
* there are internally three printing modes: flat, on need (by default, split only when necessary) and all (like the normal mode of PPrint)

Documentation
--------------
You can also generate the documentation running `make doc`.

* `type t` The document's abstract type.

Basics:
* `empty : t` The empty document.
* `string : string -> t` A non-breakable string. The string should be newlines free.
* `sub_string : string -> int -> int -> t` A non-breakable string pointing to the sub-string of an existing string. Does not duplicate the sub-string. The sub-string is indexed by its offset and its length.
* `(!^) : string -> t` A shortcut for `string`.
* `space : t` A breakable space.
* `newline : t` A newline.
* `append : t -> t -> t` Concatenation of two documents.
* `(^-^) : t -> t -> t` A shortcut for `append`.
* `concat_with_space : t -> t -> t` Concatenation of two documents with a breakable space in between. Like `d1 ^-^ space ^-^ d2`.
* `(^^) : t -> t -> t` A shortcut for `concat_with_space`. Like `d1 ^-^ space ^-^ d2`.

Text:
* `words : string -> t` Split a non-unicode string into words and breakable spaces.
* `lines : string -> t` Split a non-unicode string into lines at each newline.

Indentation and grouping:
* `indent : t -> t` Add one level of indentation.
* `nest : t -> t` Group a document, breaking spaces only when necessary. Indent when spaces are broken.
* `nest_all : t -> t` Group a document, breaking all spaces if the line is full. Indent when spaces are broken.
* `group : t -> t` Group a document, breaking spaces only when necessary. Do not indent when spaces are broken.
* `group_all : t -> t` Group a document, breaking all spaces if the line is full. Do not indent when spaces are broken.

Enclosing:
* `parens : t -> t` Enclose the document in parenthesis ( ).
* `braces : t -> t` Enclose the document in braces { }.
* `brakets : t -> t` Enclose the document in brakets [ ].
* `angle_brakets : t -> t` Enclose the document in angle brakets < >.
* `single_quotes : t -> t` Enclose the document in single quotes ' '.
* `double_quotes : t -> t` Enclose the document in double quotes " ".

Lists:
* `concat : t list -> t` Concatenate the list of documents with no space. `concat [d1; ...; dn]` is like `d1 ^-^ ... ^-^ dn`.
* `separate : t -> t list -> t` Concatenate the list of documents with no space but adding a separator in between. `separate sep [d1; ...; dn]` is like `d1 ^-^ sep ^-^ d2 ^-^ sep ... sep ^-^ dn`.

OCaml values:
* `OCaml.unit : unit -> t` Pretty-print the unit value.
* `OCaml.bool : bool -> t` Pretty-print a `bool`.
* `OCaml.int : int -> t` Pretty-print an `int`.
* `OCaml.float : float -> t` Pretty-print a `float`.
* `OCaml.string : string -> t` Pretty-print a `string`.
* `OCaml.option : ('a -> t) -> 'a option -> t` Pretty-print an `option`.
* `OCaml.list : ('a -> t) -> 'a list -> t` Pretty-print a `list`.
* `OCaml.tuple : t list -> t` Pretty-print a tuple of values.

A pretty-printer for the pretty-printer itself:
* `Debug.pp_document : t -> t` Pretty-print a document's structure.
* `Debug.pp_document_after_rendering : int -> t -> t` Pretty-print a document's structure after rendering (transformation of some spaces to newlines).

Rendering:
* `to_something : int -> int -> (char -> unit) -> (string -> unit) -> (string -> int -> int -> unit) -> t -> unit` : Render a document with a maximal width per line and a tabulation size. Uses the functions `add_char`, `add_string` and `add_sub_string` given in parameters.
* `to_buffer : int -> int -> Buffer.t -> t -> unit` Render a document in a buffer with a maximal width per line and a tabulation size.
* `to_string : int -> int -> t -> string` Render a document in a string with a maximal width per line and a tabulation size.
* `to_out_channel : int -> int -> out_channel -> t -> unit` Render a document in an output channel with a maximal width per line and a tabulation size.
* `to_stdout : int -> int -> t -> unit` Render a document on stdout with a maximal width per line and a tabulation size.
