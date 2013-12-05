(** SmartPrint library. *)

(** The document's abstract type. *)
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

(** Concatenation of two documents. *)
val append : t -> t -> t

(** A shortcut for [append]. *)
val (^-^) : t -> t -> t

(** Concatenation of two documents with a breaking space in between. Like [d1 ^-^ space ^-^ d2]. *)
val concat_with_space : t -> t -> t

(** A shortcut for [concat_with_space]. Like [d1 ^-^ space ^-^ d2]. *)
val (^^) : t -> t -> t

(** {1 Text} *)
(** Split a non-unicode string into words and breaking spaces. *)
val words : string -> t

(** Split a non-unicode string into lines at each newline. *)
val lines : string -> t

(** {1 Indentation and grouping} *)
(** Indent a document, breaking spaces only when necessary. *)
val nest : int -> t -> t

(** Indent a document, breaking no space or all spaces if the line is full. *)
val nest_all : int -> t -> t

(** Like [nest 0]. *)
val group : t -> t

(** Like [nest_all 0]. *)
val group_all : t -> t

(** {1 Enclosing} *)
(** Enclose the document in parenthesis ( ). *)
val parens : t -> t

(** Enclose the document in braces \{ \}. *)
val braces : t -> t

(** Enclose the document in brakets \[ \]. *)
val brakets : t -> t

(** Enclose the document in angle brakets < >. *)
val angle_brakets : t -> t

(** Enclose the document in single quotes ' '. *)
val single_quotes : t -> t

(** Enclose the document in double quotes " ". *)
val double_quotes : t -> t

(** {1 Lists} *)
(** Concatenate the list of documents with no space.
    [concat [d1; ...; dn] = d1 ^-^ ... ^-^ dn] *)
val concat : t list -> t

(** Concatenate the list of documents with no space but adding a separator in between.
    [separate sep [d1; ...; dn] = d1 ^-^ sep ^-^ d2 ^-^ sep ... sep ^-^ dn] *)
val separate : t -> t list -> t

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

(** {1 Debugging} *)
(** A pretty-printer for the pretty-printer itself. *)
module Debug : sig
  (** Pretty-print a document's structure. *)
  val pp_document : t -> t

  (** Pretty-print a document's structure after rendering (transformation of
      some spaces to newlines). *)
  val pp_document_after_rendering : int -> t -> t
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