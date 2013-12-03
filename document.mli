(** The document type and the user operators. *)

(** The document abstract type. *)
type t

(** {1 Basics} *)
(** The empty document. *)
val empty : t

(** A non-breaking string. The string should be newlines free. *)
val string : string -> t

(** A non-breaking string pointing to the sub-string of an existing string.
    Does not duplicate the sub-string. The sub-string is indexed by its
    index and its length. *)
val sub_string : string -> int -> int -> t

(** A shortcut for [string]. *)
val (!^) : string -> t

(** A breaking space. *)
val space : t

(** A newline. *)
val newline : t

(** Concatenation of two documents. In O(1). *)
val append : t -> t -> t

(** A shortcut for [append]. In O(1). *)
val (^-^) : t -> t -> t

(** Concatenation of two documents with a breaking space in between. In O(1). Like [d1 ^-^ space ^-^ d2]. *)
val concat_with_space : t -> t -> t

(** A shortcut for [concat_with_space]. In O(1). Like [d1 ^-^ space ^-^ d2]. *)
val (^^) : t -> t -> t

(** Convert a document, which is a tree of atoms, to a list of atoms. In O(n). *)
val unsafe_to_atoms : t -> Atom.t list

(** {1 Indentation and grouping} *)
(** Indent a document, breaking spaces only when necessary. In O(n) with n the number of grouped documents. *)
val nest : int -> t -> t

(** Indent a document, breaking no space or all spaces if the line is full. In O(n) with n the number of grouped documents. *)
val nest_all : int -> t -> t

(** Like [nest 0]. In O(n) with n the number of grouped documents. *)
val group : t -> t

(** Like [nest_all 0]. In O(n) with n the number of grouped documents. *)
val group_all : t -> t

(** {1 Enclosing} *)
(** Enclose the document in parenthesis ( ). In O(1). *)
val parens : t -> t

(** Enclose the document in braces \{ \}. In O(1). *)
val braces : t -> t

(** Enclose the document in brakets \[ \]. In O(1). *)
val brakets : t -> t

(** Enclose the document in angle brakets < >. In O(1). *)
val angle_brakets : t -> t

(** Enclose the document in single quotes ' '. In O(1). *)
val single_quotes : t -> t

(** Enclose the document in double quotes " ". In O(1). *)
val double_quotes : t -> t

(** {1 Lists} *)
(** Concatenate the list of documents with no space. In O(n).
    [concat [d1; ...; dn] = d1 ^-^ ... ^-^ dn] *)
val concat : t list -> t

(** Concatenate the list of documents with no space but adding a separator in between. In O(n).
    [separate sep [d1; ...; dn] = d1 ^-^ sep ^-^ d2 ^-^ sep ... sep ^-^ dn] *)
val separate : t -> t list -> t

(** {1 Text} *)
(**  *)
val words : string -> t

(** {1 OCaml values} *)
(** Pretty-printing of OCaml values. *)
module OCaml : sig
  (** Pretty-print a [bool]. *)
  val bool : bool -> t

  (** Pretty-print an [int]. *)
  val int : int -> t

  (** Pretty-print a [float]. *)
  val float : float -> t

  (** Pretty-print a [string]. *)
  val string : string -> t

  (** Pretty-print an [option]. *)
  val option : ('a -> t) -> 'a option -> t

  (** Pretty-print a [list]. *)
  val list : ('a -> t) -> 'a list -> t
end

(** {1 Rendering} *)
(** Render a document in a buffer with a maximal [width] per line. *)
val to_buffer : int -> Buffer.t -> t -> unit

(** Render a document in a string with a maximal [width] per line. *)
val to_string : int -> t -> string

(** Render a document in an output channel with a maximal [width] per line. *)
val to_out_channel : int -> out_channel -> t -> unit

(** Render a document on [stdout] with a maximal [width] per line. *)
val to_stdout : int -> t -> unit