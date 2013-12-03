(** Separators. *)

(** A break can be a whitespace or a newline if the text has to be splited. *)
type t =
  | Space
  | Newline