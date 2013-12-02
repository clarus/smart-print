open Document

type t =
  | Var of string
  | App of t * t
  | Fun of string * t
  | Let of t * string * t
  | Tuple of t list

let rec pp (e : t) : Document.t =
  match e with
  | Var x -> !^ x
  | App (e1, e2) -> group (parens (pp e1 ^^ nest 2 (pp e2)))
  | Fun (x, e) -> parens (!^ "fun" ^^ !^ x ^^ !^ "->" ^^ nest 2 (pp e))

let es = [
  App (Var "f", Var "x");
  App (Var "fdsgoklkmeee", Var "xdsgsdg");
  Fun ("x", App (Var "fdsgo", Var "xdsgsdg"))]