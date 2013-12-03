(** The internal representation of a document and the engine. *)

(** An atom is the low-level tree describing a document. *)
type t =
  | String of string (** A non-breaking string. It should be newlines free. *)
  | Break of Break.t (** A separator. *)
  | GroupOne of int * t list
    (** A list of atoms at a given identation level. Only the necessary number
        of breaks are splited. *)
  | GroupAll of int * t list
    (** A list of atoms at a given identation level. No or all the breaks are
        splited. *)

let rec squeeze_breaks (_as : t list) : Break.t list * t list * Break.t list =
  let lift bs =
    List.map (fun b -> Break b) bs in
  match _as with
  | [] -> ([], [], [])
  | a :: _as ->
    let (previous_breaks, _as, next_breaks) = squeeze_breaks _as in
    let return_group group _as' =
      let (previous_breaks', _as', next_breaks') = squeeze_breaks _as' in
      if _as' = [] then
        (previous_breaks' @ next_breaks' @ previous_breaks, _as, next_breaks)
      else
        (previous_breaks', group _as' :: lift next_breaks' @ lift previous_breaks @ _as, next_breaks) in
    (match a with
    | String s -> ([], String s :: lift previous_breaks @ _as, next_breaks)
    | Break b -> (b :: previous_breaks, _as, next_breaks)
    | GroupOne (i, _as') -> return_group (fun _as' -> GroupOne (i, _as')) _as'
    | GroupAll (i, _as') -> return_group (fun _as' -> GroupAll (i, _as')) _as')

let rec merge_breaks (_as : t list) : t list =
  match _as with
  | [] -> _as
  | (Break Break.Newline as a1) :: (Break Break.Newline as a2) :: _as ->
    a1 :: merge_breaks (a2 :: _as)
  | Break Break.Space :: Break b2 :: _as -> merge_breaks (Break b2 :: _as)
  | Break b1 :: Break Break.Space :: _as -> merge_breaks (Break b1 :: _as)
  | GroupOne (i, _as') :: _as -> GroupOne (i, merge_breaks _as') :: merge_breaks _as
  | GroupAll (i, _as') :: _as -> GroupAll (i, merge_breaks _as') :: merge_breaks _as
  | a :: _as -> a :: merge_breaks _as

exception Overflow

let rec eval (width : int) (i : int) (a : t) (p : int) : t * int =
  match a with
  | String s ->
    let l = String.length s in
    (a, if p = 0 then p + i + l else p + l)
  | Break Break.Space -> (a, p + 1)
  | Break Break.Newline -> (a, 0)
  | GroupOne (i', _as) ->
    let (_as, p) = try_eval_list_one width (i + i') _as p false in
    (GroupOne (i', _as), p)
  | GroupAll (i', _as) ->
    let (_as, p) = try try_eval_list_flat width (i + i') _as p with
      | Overflow -> eval_list_all width i _as p in
    (GroupAll (i', _as), p)

and try_eval_flat (width : int) (i : int) (a : t) (p : int) : t * int =
  let try_return (a, p) =
    if p > width then
      raise Overflow
    else
      (a, p) in
  match a with
  | String s ->
    let l = String.length s in
    try_return (a, if p = 0 then p + i + l else p + l)
  | Break Break.Space -> try_return (a, p + 1)
  | Break Break.Newline -> (a, 0)
  | GroupOne (i', _as) ->
    let (_as, p) = try_eval_list_flat width (i + i') _as p in
    (GroupOne (i', _as), p)
  | GroupAll (i', _as) ->
    let (_as, p) = try_eval_list_flat width (i + i') _as p in
    (GroupAll (i', _as), p)

and try_eval_list_flat (width : int) (i : int) (_as : t list) (p : int) : t list * int =
  match _as with
  | [] -> (_as, p)
  | a :: _as ->
    let (a, p) = try_eval_flat width i a p in
    let (_as, p) = try_eval_list_flat width i _as p in
    (a :: _as, p)

and try_eval_list_one (width : int) (i : int) (_as : t list) (p : int) (can_fail : bool) : t list * int =
  match _as with
  | [] -> (_as, p)
  | Break Break.Space :: _as ->
    (try let (_as, p) = try_eval_list_one width i _as (p + 1) true in
      (Break Break.Space :: _as, p) with
    | Overflow -> try_eval_list_one width i (Break Break.Newline :: _as) p false)
  | a :: _as ->
    let (a, p) =
      if can_fail
      then try_eval_flat width i a p
      else eval width i a p in
    let (_as, p) = try_eval_list_one width i _as p can_fail in
    (a :: _as, p)

and eval_list_all (width : int) (i : int) (_as : t list) (p : int) : t list * int =
  match _as with
  | [] -> (_as, p)
  | Break Break.Space :: _as ->
    eval_list_all width i (Break Break.Newline :: _as) p
  | a :: _as ->
    let (a, p) = eval width i a p in
    let (_as, p) = eval_list_all width i _as p in
    (a :: _as, p)

(** Squeeze, merge and evaluate the breaks with a maximal [width] per line. *)
let render (width : int) (_as : t list) : t =
  let (previous_breaks, _as, next_breaks) = squeeze_breaks _as in
  let lift = List.map (fun b -> Break b) in
  let _as = lift previous_breaks @ _as @ lift next_breaks in
  let _as = merge_breaks _as in
  fst @@ eval width 0 (GroupOne (0, _as)) 0

(** Write to something, given the [add_char] and [add_string] functions. *)
let to_something (add_char : char -> unit) (add_string : string -> unit) (a : t) : unit =
  let rec aux (a : t) (i : int) (is_newline : bool) : bool =
    match a with
    | String s ->
      if is_newline then
        add_string (String.make i ' ');
      add_string s; false
    | Break Break.Space -> add_char ' '; false
    | Break Break.Newline -> add_char '\n'; true
    | GroupOne (i', _as) | GroupAll (i', _as) ->
      let b = ref is_newline in
      _as |> List.iter (fun a -> b := aux a (i + i') !b);
      !b in
  ignore (aux a 0 true)

(** Write in a buffer the contents of an atom. *)
let to_buffer (b : Buffer.t) (a : t) : unit =
  to_something (Buffer.add_char b) (Buffer.add_string b) a

(** Write in a channel the contents of an atom. *)
let to_out_channel (c : out_channel) (a : t) : unit =
  to_something (output_char c) (output_string c) a