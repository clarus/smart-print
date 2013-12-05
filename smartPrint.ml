(* Separators. *)
module Break = struct
  (* A break can be a whitespace or a newline if the text has to be splited. *)
  type t =
    | Space
    | Newline
end

(* The internal representation of a document and the engine. *)
module Atom = struct
  (* An atom is the low-level tree describing a document. *)
  type t =
    | String of string * int * int
      (* A non-breaking string. It should be newlines free. Represented as a
          sub-string of an other string, with an offset and a length. *)
    | Break of Break.t (* A separator. *)
    | GroupOne of int * t list
      (* A list of atoms at a given identation level. Only the necessary number
          of breaks are splited. *)
    | GroupAll of int * t list
      (* A list of atoms at a given identation level. No or all the breaks are
          splited. *)

  exception Overflow

  let rec eval (width : int) (i : int) (a : t) (p : int) (last_break : Break.t option)
    : t * int * Break.t option =
    match a with
    | String (_, _, l) ->
      (a, (if last_break = Some Break.Newline then p + i + l else p + l), None)
    | Break Break.Space ->
      if last_break = None then
        (a, p + 1, Some Break.Space)
      else
        (a, p, last_break)
    | Break Break.Newline -> (a, 0, Some Break.Newline)
    | GroupOne (i', _as) ->
      let (_as, p, last_break) = try_eval_list_one width (i + i') _as p last_break false in
      (GroupOne (i', _as), p, last_break)
    | GroupAll (i', _as) ->
      let (_as, p, last_break) = try try_eval_list_flat width (i + i') _as p last_break with
        | Overflow -> eval_list_all width i _as p last_break in
      (GroupAll (i', _as), p, last_break)

  and try_eval_flat (width : int) (i : int) (a : t) (p : int) (last_break : Break.t option)
    : t * int * Break.t option =
    let try_return (a, p, last_break) =
      if p > width then
        raise Overflow
      else
        (a, p, last_break) in
    match a with
    | String (_, _, l) ->
      try_return (a, (if last_break = Some Break.Newline then p + i + l else p + l), None)
    | Break Break.Space ->
      if last_break = None then
        try_return (a, p + 1, Some Break.Space)
      else
        try_return (a, p, last_break)
    | Break Break.Newline -> (a, 0, Some Break.Newline)
    | GroupOne (i', _as) ->
      let (_as, p, last_break) = try_eval_list_flat width (i + i') _as p last_break in
      (GroupOne (i', _as), p, last_break)
    | GroupAll (i', _as) ->
      let (_as, p, last_break) = try_eval_list_flat width (i + i') _as p last_break in
      (GroupAll (i', _as), p, last_break)

  and try_eval_list_flat (width : int) (i : int) (_as : t list) (p : int) (last_break : Break.t option)
    : t list * int * Break.t option =
    match _as with
    | [] -> (_as, p, last_break)
    | a :: _as ->
      let (a, p, last_break) = try_eval_flat width i a p last_break in
      let (_as, p, last_break) = try_eval_list_flat width i _as p last_break in
      (a :: _as, p, last_break)

  and try_eval_list_one (width : int) (i : int) (_as : t list) (p : int) (last_break : Break.t option)
    (can_fail : bool) : t list * int * Break.t option =
    match _as with
    | [] -> (_as, p, last_break)
    | Break Break.Space :: _as ->
      if last_break = None then
        (try let (_as, p, last_break) = try_eval_list_one width i _as (p + 1) (Some Break.Space) true in
          (Break Break.Space :: _as, p, last_break) with
        | Overflow -> try_eval_list_one width i (Break Break.Newline :: _as) p last_break false)
      else
        try_eval_list_one width i _as p last_break can_fail
    | a :: _as ->
      let (a, p, last_break) =
        if can_fail
        then try_eval_flat width i a p last_break
        else eval width i a p last_break in
      let (_as, p, last_break) = try_eval_list_one width i _as p last_break can_fail in
      (a :: _as, p, last_break)

  and eval_list_all (width : int) (i : int) (_as : t list) (p : int) (last_break : Break.t option)
    : t list * int * Break.t option =
    match _as with
    | [] -> (_as, p, last_break)
    | Break Break.Space :: _as ->
      if last_break = None then
        eval_list_all width i (Break Break.Newline :: _as) p last_break
      else
        eval_list_all width i _as p last_break
    | a :: _as ->
      let (a, p, last_break) = eval width i a p last_break in
      let (_as, p, last_break) = eval_list_all width i _as p last_break in
      (a :: _as, p, last_break)

  (* Evaluate the breaks with a maximal [width] per line. *)
  let render (width : int) (_as : t list) : t =
    let (a, _, _) = eval width 0 (GroupOne (0, _as)) 0 (Some Break.Newline) in
    a

  (* Write to something, given the [add_char] and [add_string] functions. *)
  let to_something (add_char : char -> unit) (add_string : string -> unit)
    (add_sub_string : string -> int -> int -> unit) (a : t) : unit =
    let rec aux (a : t) (i : int) (last_break : Break.t option) : Break.t option =
      match a with
      | String (s, o, l) ->
        if last_break = Some Break.Newline then
          add_string (String.make i ' ');
        add_sub_string s o l; None
      | Break Break.Space ->
        if last_break <> Some Break.Space then
          add_char ' ';
        Some Break.Space
      | Break Break.Newline -> add_char '\n'; Some Break.Newline
      | GroupOne (i', _as) | GroupAll (i', _as) ->
        let last_break = ref last_break in
        _as |> List.iter (fun a ->
          last_break := aux a (i + i') !last_break);
        !last_break in
    ignore (aux a 0 (Some Break.Newline))

  (* Write in a buffer the contents of an atom. *)
  let to_buffer (b : Buffer.t) (a : t) : unit =
    to_something (Buffer.add_char b) (Buffer.add_string b) (Buffer.add_substring b) a

  (* Write in a channel the contents of an atom. *)
  let to_out_channel (c : out_channel) (a : t) : unit =
    let output_sub_string (s : string) (o : int) (l : int) : unit =
      output_string c (String.sub s o l) in
    to_something (output_char c) (output_string c) output_sub_string a
end

(* A document is a binary tree of atoms so that concatenation happens in O(1). *)
type t =
  | Empty
  | Leaf of Atom.t
  | Node of t * t

let empty : t = Empty

let string (s : string) : t =
  if s = "" then
    empty
  else
    Leaf (Atom.String (s, 0, String.length s))

let (!^) = string

let sub_string (s : string) (o : int) (l : int) : t =
  Leaf (Atom.String (s, o, l))

let space : t = Leaf (Atom.Break Break.Space)

let newline : t = Leaf (Atom.Break Break.Newline)

let append (d1 : t) (d2 : t) : t =
  Node (d1, d2)

let (^-^) = append

let concat_with_space (d1 : t) (d2 : t) : t =
  d1 ^-^ space ^-^ d2

let (^^) = concat_with_space

(* Convert a document, which is a tree of atoms, to a list of atoms. In O(n). *)
let to_atoms (d : t) : Atom.t list =
  let rec aux (d : t) (l : Atom.t list) : Atom.t list =
    match d with
    | Empty -> l
    | Leaf a -> a :: l
    | Node (d1, d2) -> aux d1 (aux d2 l) in
  aux d []

let nest (i : int) (d : t) : t =
  Leaf (Atom.GroupOne (i, to_atoms d))

let nest_all (i : int) (d : t) : t =
  Leaf (Atom.GroupAll (i, to_atoms d))

let group (d : t) : t =
  nest 0 d

let group_all (d : t) : t =
  nest_all 0 d

let parens (d : t) : t =
  !^ "(" ^-^ d ^-^ !^ ")"

let braces (d : t) : t =
  !^ "{" ^-^ d ^-^ !^ "}"

let brakets (d : t) : t =
  !^ "[" ^-^ d ^-^ !^ "]"

let angle_brakets (d : t) : t =
  !^ "<" ^-^ d ^-^ !^ ">"

let single_quotes (d : t) : t =
  !^ "'" ^-^ d ^-^ !^ "'"

let double_quotes (d : t) : t =
  !^ "\"" ^-^ d ^-^ !^ "\""

let concat (ds : t list) : t =
  List.fold_left append empty ds

let separate (separator : t) (ds : t list) : t =
  let rec aux ds =
    match ds with
    | [] -> empty
    | d :: ds -> separator ^-^ d ^-^ aux ds in
  match ds with
  | [] -> empty
  | d :: ds -> d ^-^ aux ds

let words (s : string) : t =
  group @@ separate space @@ List.map string @@ Str.split (Str.regexp "[ \t\n]") s

let lines (s : string) : t =
  separate newline @@ List.map string @@ Str.split (Str.regexp "\n") s

module OCaml = struct
  let bool (b : bool) : t =
    !^ (string_of_bool b)

  let int (i : int) : t =
    !^ (string_of_int i)

  let float (f : float) : t =
    !^ (string_of_float f)

  let string (s : string) : t =
    double_quotes (!^ (String.escaped s))

  let option (d : 'a -> t) (o : 'a option) : t =
    match o with
    | None -> !^ "None"
    | Some x -> !^ "Some" ^^ nest 2 (d x)

  let list (d : 'a -> t) (l : 'a list) : t =
    brakets (nest_all 2 @@ separate (!^ ";" ^^ space) (List.map d l))
end

module Debug = struct
  (* Pretty-print an atom. *)
  let rec pp_atom (a : Atom.t) : t =
    match a with
    | Atom.String (s, o, l) -> OCaml.string (String.sub s o l)
    | Atom.Break Break.Space -> !^ "Space"
    | Atom.Break Break.Newline -> !^ "Newline"
    | Atom.GroupOne (i, _as) -> !^ "GroupOne" ^^ parens (OCaml.int i ^-^ !^ "," ^^ pp_atoms _as)
    | Atom.GroupAll (i, _as) -> !^ "GroupAll" ^^ parens (OCaml.int i ^-^ !^ "," ^^ pp_atoms _as)

  (* Pretty-print a list of atoms. *)
  and pp_atoms (_as : Atom.t list) : t =
    group_all (separate (!^ "," ^^ space) (List.map (fun a -> nest 2 (pp_atom a)) _as))

  let pp_document (d : t) : t =
    OCaml.list pp_atom (to_atoms d)

  let pp_document_after_rendering (width : int) (d : t) : t =
    pp_atom @@ Atom.render width @@ to_atoms d
end

let to_buffer (width : int) (b : Buffer.t) (d : t) : unit =
  Atom.to_buffer b @@ Atom.render width @@ to_atoms d

let to_string (width : int) (d : t) : string =
  let b = Buffer.create 10 in
  to_buffer width b d;
  Buffer.contents b

let to_out_channel (width : int) (c : out_channel) (d : t) : unit =
  Atom.to_out_channel c @@ Atom.render width @@ to_atoms d

let to_stdout (width : int) (d : t) : unit =
  to_out_channel width stdout d