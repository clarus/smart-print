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

module Mode = struct
  type t =
    | Flat
    | One
    | All
end

let flatten (width : int) (a : t) : t list =
  let rec aux (_as : t list) (p : int) (m : Mode.t) : (t list * int) option =
    match m with
    | Mode.Flat ->
      if p >= width then
        None
      else
        (match _as with
        | [] -> Some ([], 0)
        | a :: _as ->
          let next a s _as =
            match aux  with
            | [] in
          (match a with
          | String s ->
            let l = String.length s in
            aux _as ))
    
    match a with
    | String s -> s
    | Space -> " "
    | NewLine -> "\n"
    | GroupOne (_, _as) -> failwith "TODO"
    | GroupAll (_, _as) -> String.concat "" (List.map to_string _as) in
  aux [a] 0 Mode.Flat

(** A first naive approach. *)
let rec to_string (a : t) : string =
  match a with
  | String s -> s
  | Space -> " "
  | NewLine -> "\n"
  | GroupOne (_, _as) -> failwith "TODO"
  | GroupAll (_, _as) -> String.concat "" (List.map to_string _as)
