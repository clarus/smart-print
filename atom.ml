type t =
  | String of string
  | Space
  | NewLine
  | GroupOne of int * t list
  | GroupAll of int * t list

let concat (a1 : t) (a2 : t) : t list =
  match (a1, a2) with
  | Space, Space -> [Space]
  | Space, NewLine | NewLine, Space -> [NewLine]
  | _ -> [a1; a2]

(*module Mode = struct
  type t =
    | Flat
    | One
    | All
end*)

exception CannotBeFlat

(** We suppose [p] < [width] for the input and the output. *)
let rec try_eval_spaces_flat (width : int) (a : t) (p : int) : t * int =
  let try_return size =
    if p + size >= width
    then raise CannotBeFlat
    else (a, p + size) in
  let try_return_group _as group =
    let (_as, p) = try_eval_spaces_flat_list width _as p in
    (group _as, p) in
  match a with
  | String s -> try_return (String.length s)
  | Space -> try_return 1
  | NewLine -> raise CannotBeFlat
  | GroupOne (i, _as) -> try_return_group _as (fun _as -> GroupOne (i, _as))
  | GroupAll (i, _as) -> try_return_group _as (fun _as -> GroupAll (i, _as))

and try_eval_spaces_flat_list (width : int) (_as : t list) (p : int) : t list * int =
  match _as with
  | [] -> ([], p)
  | a :: _as ->
    let (a, p) = try_eval_spaces_flat width a p in
    let (_as, p) = try_eval_spaces_flat_list width _as p in
    (a :: _as, p)

(*let eval_spaces (width : int) (a : t) : t list =
  let rec aux (_as : t list) (p : int) (m : Mode.t) : (t list * int) option =
    match m with
    | Mode.Flat ->
      let next a p _as =
        match aux _as p Mode.Flat with
        | None -> None
        | Some (_as, p) -> Some (a :: _as, p) in
      if p >= width then
        None
      else
        (match _as with
        | [] -> Some ([], 0)
        | a :: _as ->
          (match a with
          | String s ->
            let l = String.length s in
            next (String s) (p + l) _as)) in
  aux [a] 0 Mode.Flat*)

(** Write in a buffer the contents of an atom where all spaces have been evaluated. *)
let to_buffer (b : Buffer.t) (a : t) : unit =
  let rec aux (a : t) (i : int) (is_new_line : bool) : bool =
    if is_new_line then
      Buffer.add_string b (String.make i ' ');
    match a with
    | String s -> Buffer.add_string b s; false
    | Space -> Buffer.add_char b ' '; false
    | NewLine -> Buffer.add_char b '\n'; true
    | GroupOne (i', _as) | GroupAll (i', _as) ->
      let b = ref is_new_line in
      _as |> List.iter (fun a -> b := aux a (i + i') !b);
      !b in
  ignore (aux a 0 true)

let to_string (a : t) : string =
  let b = Buffer.create 10 in
  to_buffer b a;
  Buffer.contents b