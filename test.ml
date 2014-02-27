(** A small program to test the library on different samples.
    Run with [make test]. Compare its output to the reference in [test.out]. *)
open SmartPrint

(** The syntax tree: variables, applications, functions, let-definitions, tuples. *)
type t =
  | Var of string
  | App of t * t
  | Fun of string * t
  | Let of string * t * t
  | Tuple of t list

(** Pretty-print an expression, enclosing it in parenthesis if [paren] flag is set. *)
let rec pp (paren : bool) (e : t) : SmartPrint.t =
  let if_parens d =
    if paren then parens d else d in
  match e with
  | Var x -> !^ x
  | App (e1, e2) -> nest @@ if_parens (pp true e1 ^^ pp true e2)
  | Fun (x, e) -> nest @@ parens (!^ "fun" ^^ !^ x ^^ !^ "->" ^^ pp false e)
  | Let (x, e1, e2) ->
    nest (!^ "let" ^^ !^ x ^^ !^ "=" ^^ pp false e1 ^^ !^ "in" ^^ newline ^^ pp false e2)
  | Tuple es -> OCaml.list (pp false) es

(** A sample of expressions. *)
let es = [
  App (Var "f", Var "x");
  App (Var "fdsgoklkmeee", Var "xdsgsdg");
  Fun ("x", App (Var "fdsgo", App (Var "x", Var "xdsgsdg")));
  Fun ("x", App (Var "fdsgvbcvvfo", App (Var "xffd", Var "xdsgsdg")));
  Tuple [];
  Tuple [Var "x"; Var "y"];
  Tuple (List.map (fun x -> Var x) ["kjh"; "lj"; "iop"; "rt"; "vbn"; "hjk"; "gkgytuuhi"]);
  Let ("x", Var "x", Var "y");
  Let ("x", Fun ("x", App (Var "fdsgo", App (Var "x", Var "xdsgsdg"))), Var "y")]

(** Display the contents of a document and its AST. *)
let print_document (d : SmartPrint.t) : unit =
  let width = 25 in
  let tab = 2 in
  to_stdout 160 tab @@ Debug.pp_document_after_rendering width tab d;
  print_newline ();
  print_endline @@ String.make width '*';
  print_endline @@ to_string width tab d;
  print_endline @@ String.make width '*'

(** The main function. *)
let main () =
  print_document (!^ "hello" ^^ !^ "world" ^^ newline ^^ nest (!^ "gre" ^^ nest_all (!^ "arg" ^^ !^ "arg")));
  es |> List.iter (fun e ->
    print_document @@ pp false e);
  print_document @@ sub_string "hfgs_kjl_df" 5 3;
  print_document @@ words "Lorem     ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.";
  print_document @@ lines "adipisicing elit,\nsed do eiusmod tempor\nincididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud ";
  print_document @@ nest @@ lines "adipisicing elit,\n\nsed do eiusmod tempo incididunt ut labore et dolore";
  print_document @@ indent @@ nest @@ lines "adipisicing elit,\n\nsed do eiusmod tempo incididunt ut labore et dolore";
  print_document @@ indent @@ pp false @@ Let ("x", Var "x", Let ("x", Var "x", Var "y"));
  print_document @@ pp false @@ App (Var "f", Let ("x", Var "x", Var "y"));
  print_document @@ OCaml.unit () ^^ OCaml.tuple [!^ "x"; OCaml.int 0]

;;main ()
