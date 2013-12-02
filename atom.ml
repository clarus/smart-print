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

exception Overflow

let rec eval (width : int) (i : int) (a : t) (p : int) : t * int =
  match a with
  | String s ->
    let l = String.length s in
    (a, if p = 0 then p + i + l else p + l)
  | Space -> (a, p + 1)
  | NewLine -> (a, 0)
  | GroupOne (i', _as) ->
    let (_as, p) = try_eval_list_one width (i + i') _as p false in
    (GroupOne (i', _as), p)
  | GroupAll (i', _as) ->
    let (_as, p) = try try_eval_list_flat width (i + i') _as p with
      | Overflow -> eval_list_all width i _as p in
    (GroupAll (i', _as), p)

and try_eval_flat (width : int) (i : int) (a : t) (p : int) : t * int =
  let try_return (a, p) =
    if p >= width then
      raise Overflow
    else
      (a, p) in
  match a with
  | String s ->
    let l = String.length s in
    try_return (a, if p = 0 then p + i + l else p + l)
  | Space -> try_return (a, p + 1)
  | NewLine -> (a, 0)
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
  | Space :: _as ->
    (try let (_as, p) = try_eval_list_one width i _as (p + 1) true in
      (Space :: _as, p) with
    | Overflow -> try_eval_list_one width i (NewLine :: _as) p false)
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
  | Space :: _as -> eval_list_all width i (NewLine :: _as) p
  | a :: _as ->
    let (a, p) = eval width i a p in
    let (_as, p) = eval_list_all width i _as p in
    (a :: _as, p)

(** Write in a buffer the contents of an atom where all spaces have been evaluated. *)
let to_buffer (b : Buffer.t) (a : t) : unit =
  let rec aux (a : t) (i : int) (is_new_line : bool) : bool =
    match a with
    | String s ->
      if is_new_line then
        Buffer.add_string b (String.make i ' ');
      Buffer.add_string b s; false
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