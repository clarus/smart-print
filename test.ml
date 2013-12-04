(** A small program to test the library on different inputs. Run with [make test]. *)
open SmartPrint

(** A pretty-printer for the pretty-printer itself. *)
module Debug = struct
  open Atom

  (** Pretty-print an atom. *)
  let rec pp_atom (a : Atom.t) : Document.t =
    match a with
    | String (s, o, l) -> OCaml.string (String.sub s o l)
    | Break Break.Space -> !^ "Space"
    | Break Break.Newline -> !^ "Newline"
    | GroupOne (i, _as) -> !^ "GroupOne" ^^ parens (OCaml.int i ^-^ !^ "," ^^ pp_atoms _as)
    | GroupAll (i, _as) -> !^ "GroupAll" ^^ parens (OCaml.int i ^-^ !^ "," ^^ pp_atoms _as)

  (** Pretty-print a list of atoms. *)
  and pp_atoms (_as : Atom.t list) : Document.t =
    group_all (separate (!^ "," ^^ space) (List.map (fun a -> nest 2 (pp_atom a)) _as))

  let pp_document (d : Document.t) : Document.t =
    OCaml.list pp_atom (Document.unsafe_to_atoms d)

  let pp_document_after_rendering (width : int) (d : Document.t) : Document.t =
    pp_atom @@ Atom.render width @@ Document.unsafe_to_atoms d
end

(** A pretty-printer for a small functional language. *)
module Example = struct
  (** The syntax tree: variables, applications, functions, let-definitions, tuples. *)
  type t =
    | Var of string
    | App of t * t
    | Fun of string * t
    | Let of string * t * t
    | Tuple of t list

  (** Pretty-print an expression, enclosing it in parenthesis if [paren] flag is set. *)
  let rec pp (paren : bool) (e : t) : Document.t =
    let if_parens d =
      if paren then parens d else d in
    match e with
    | Var x -> !^ x
    | App (e1, e2) -> group (if_parens (pp true e1 ^^ nest 2 @@ pp true e2))
    | Fun (x, e) -> group (parens (!^ "fun" ^^ !^ x ^^ !^ "->" ^^ nest 2 @@ pp false e))
    | Let (x, e1, e2) ->
      group (!^ "let" ^^ !^ x ^^ !^ "=" ^^ nest 2 (pp false e1) ^^ !^ "in" ^^ newline ^^ pp false e2)
    | Tuple es -> (*group_all @@ parens @@ separate (!^ "," ^^ space) (es |> List.map (fun e -> nest 2 @@ pp false e))*)
      OCaml.list (pp false) es

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
end

(** Display the contents of a document and its AST. *)
let print_document (d : Document.t) : unit =
  let width = 25 in
  to_stdout 160 @@ Debug.pp_document_after_rendering width d;
  print_newline ();
  print_endline @@ String.make width '*';
  print_endline @@ to_string width d;
  print_endline @@ String.make width '*'

(** The main function. *)
let main () =
  print_document (!^ "hello" ^^ !^ "world" ^^ newline ^^ nest 2 (!^ "gre" ^^ nest_all 2 (!^ "arg" ^^ !^ "arg")));
  Example.es |> List.iter (fun e ->
    print_document @@ Example.pp false e);
  print_document (sub_string "hfgs_kjl_df" 5 3);
  print_document (words "Lorem     ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.");
  print_document (lines "adipisicing elit,\nsed do eiusmod tempor\nincididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud ")

;;main ()
