(** The document type and the user operators. *)

(** A document is a binary tree of atoms so that concatenation happens in O(1). *)
type t =
  | Empty
  | Leaf of Atom.t
  | Node of t * t

(** {1 Basics} *)
(** The empty document. *)
let empty : t = Empty

(** A non-breaking string. The string should be newlines free. *)
let string (s : string) : t =
  Leaf (Atom.String s)

(** A shortcut for [string]. *)
let (!^) = string

(** A breaking space. *)
let space : t = Leaf (Atom.Break Break.Space)

(** A newline. *)
let newline : t = Leaf (Atom.Break Break.Newline)

(** Concatenation of two documents. In O(1). *)
let append (d1 : t) (d2 : t) : t =
  Node (d1, d2)

(** A shortcut for [append]. *)
let (^-^) = append

(** Concatenation of two documents with a breaking space in between. In O(1). Like [d1 ^-^ space ^-^ d2]. *)
let concat_with_space (d1 : t) (d2 : t) : t =
  d1 ^-^ space ^-^ d2

(** A shortcut for [concat_with_space]. Like [d1 ^-^ space ^-^ d2]. *)
let (^^) = concat_with_space

(** {1 Indentation and grouping} *)
(** Convert a document, which is a tree of atoms, to a list of atoms. In O(n). *)
let flatten (d : t) : Atom.t list =
  let rec aux (d : t) (l : Atom.t list) : Atom.t list =
    match d with
    | Empty -> l
    | Leaf a -> a :: l
    | Node (d1, d2) -> aux d1 (aux d2 l) in
  aux d []

(** Indent a document, breaking spaces only when necessary. In O(n) with n the number of grouped documents. *)
let nest (i : int) (d : t) : t =
  Leaf (Atom.GroupOne (i, flatten d))

(** Indent a document, breaking no space or all spaces if the line is full. In O(n) with n the number of grouped documents. *)
let nest_all (i : int) (d : t) : t =
  Leaf (Atom.GroupAll (i, flatten d))

(** Like [nest 0]. In O(n) with n the number of grouped documents. *)
let group (d : t) : t =
  nest 0 d

(** Like [nest_all 0]. In O(n) with n the number of grouped documents. *)
let group_all (d : t) : t =
  nest_all 0 d

(** {1 Enclosing} *)
(** Enclose the document in parenthesis ( ). In O(1). *)
let parens (d : t) : t =
  !^ "(" ^-^ d ^-^ !^ ")"

(** Enclose the document in braces \{ \}. In O(1). *)
let braces (d : t) : t =
  !^ "{" ^-^ d ^-^ !^ "}"

(** Enclose the document in brakets \[ \]. In O(1). *)
let brakets (d : t) : t =
  !^ "[" ^-^ d ^-^ !^ "]"

(** Enclose the document in angle brakets < >. In O(1). *)
let angle_brakets (d : t) : t =
  !^ "<" ^-^ d ^-^ !^ ">"

(** Enclose the document in single quotes ' '. In O(1). *)
let single_quotes (d : t) : t =
  !^ "'" ^-^ d ^-^ !^ "'"

(** Enclose the document in double quotes " ". In O(1). *)
let double_quotes (d : t) : t =
  !^ "\"" ^-^ d ^-^ !^ "\""

(** {1 Lists} *)
(** Concatenate the list of documents with no space. In O(n).
    [concat [d1; ...; dn] = d1 ^-^ ... ^-^ dn] *)
let concat (ds : t list) : t =
  List.fold_left append empty ds

(** Concatenate the list of documents with no space but adding a separator in between. In O(n).
    [separate sep [d1; ...; dn] = d1 ^-^ sep ^-^ d2 ^-^ sep ... sep ^-^ dn] *)
let separate (separator : t) (ds : t list) : t =
  let rec aux ds =
    match ds with
    | [] -> empty
    | d :: ds -> separator ^-^ d ^-^ aux ds in
  match ds with
  | [] -> empty
  | d :: ds -> d ^-^ aux ds

(** {1 OCaml values} *)
(** Pretty-printing of OCaml values. *)
module OCaml = struct
  (** Pretty-print a [bool]. *)
  let bool (b : bool) : t =
    !^ (string_of_bool b)

  (** Pretty-print an [int]. *)
  let int (i : int) : t =
    !^ (string_of_int i)

  (** Pretty-print a [float]. *)
  let float (f : float) : t =
    !^ (string_of_float f)

  (** Pretty-print a [string]. *)
  let string (s : string) : t =
    double_quotes (!^ (String.escaped s))

  (** Pretty-print an [option]. *)
  let option (d : 'a -> t) (o : 'a option) : t =
    match o with
    | None -> !^ "None"
    | Some x -> !^ "Some" ^^ nest 2 (d x)

  (** Pretty-print a [list]. *)
  let list (d : 'a -> t) (l : 'a list) : t =
    brakets (nest_all 2 @@ separate (!^ ";" ^^ space) (List.map d l))
end
