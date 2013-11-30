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

(** A first naive approach. *)
let rec to_string (a : t) : string =
  match a with
  | String s -> s
  | Space -> " "
  | NewLine -> "\n"
  | GroupOne (_, _as) | GroupAll (_, _as) -> String.concat "" (List.map to_string _as)
