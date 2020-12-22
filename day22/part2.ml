open StdLabels
open MoreLabels

module S = Set.Make (struct
  type t = int * int

  let compare ((p1_1, p2_1) : t) ((p1_2, p2_2) : t) : int =
    let p1_compare = Int.compare p1_1 p1_2 in
    if p1_compare <> 0 then p1_compare else Int.compare p2_1 p2_2
end)

(* representing the players as a list isn't great in terms of performance, but
 * I don't want to implement a doubly-linked list *)
let parse_player input =
  let rows = String.split_on_char ~sep:'\n' input in
  rows |> List.tl |> List.map ~f:int_of_string

let read_input () =
  let rec read_input acc =
    try
      let line = read_line () in
      read_input (line :: acc)
    with End_of_file -> (
      let nn = Str.regexp "\n\n" in
      let players =
        acc
        |> List.rev
        |> String.concat ~sep:"\n"
        |> Str.split nn
        |> List.map ~f:parse_player
      in
      match players with
      | [ p1; p2 ] -> (p1, p2)
      | _ -> failwith "invalid input")
  in
  read_input []

type winner = Left of int | Right of int

let hash player =
  let rec hash' acc multiplier = function
    | [] -> acc
    | hd :: tl -> hash' (acc + (hd * multiplier)) (multiplier - 1) tl
  in
  hash' 0 (List.length player) player

let rec play_game players =
  let rec play_round rounds (p1, p2) =
    let h1 = hash p1 in
    let h2 = hash p2 in
    if S.mem (h1, h2) rounds then Left h1
    else
      match (p1, p2) with
      | [], p2 -> Right h2
      | p1, [] -> Left h1
      | c1 :: tl1, c2 :: tl2 ->
          if c1 <= List.length tl1 && c2 <= List.length tl2 then
            match play_game (tl1, tl2) with
            | Left _ ->
                play_round (S.add (h1, h2) rounds) (tl1 @ [ c1; c2 ], tl2)
            | Right _ ->
                play_round (S.add (h1, h2) rounds) (tl1, tl2 @ [ c2; c1 ])
          else if c1 > c2 then
            play_round (S.add (h1, h2) rounds) (tl1 @ [ c1; c2 ], tl2)
          else play_round (S.add (h1, h2) rounds) (tl1, tl2 @ [ c2; c1 ])
  in
  play_round S.empty players

let score winner =
  match winner with
  | Left p -> p
  | Right p -> p

let () = read_input () |> play_game |> score |> Printf.printf "%d\n"
