open StdLabels
open MoreLabels

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

type winner = Left of int list | Right of int list

let rec play_game players =
  let rounds = Hashtbl.create 1 in
  let rec play_round (p1, p2) =
    if Hashtbl.mem rounds (p1, p2) then Left p1
    else (
      Hashtbl.replace ~key:(p1, p2) ~data:true rounds;
      match (p1, p2) with
      | [], p2 -> Right p2
      | p1, [] -> Left p1
      | c1 :: tl1, c2 :: tl2 ->
          if c1 <= List.length tl1 && c2 <= List.length tl2 then
            match play_game (tl1, tl2) with
            | Left _ -> play_round (tl1 @ [ c1; c2 ], tl2)
            | Right _ -> play_round (tl1, tl2 @ [ c2; c1 ])
          else if c1 > c2 then play_round (tl1 @ [ c1; c2 ], tl2)
          else play_round (tl1, tl2 @ [ c2; c1 ]))
  in
  play_round players

let score winner =
  let player =
    match winner with
    | Left p -> p
    | Right p -> p
  in
  let rec score' acc multiplier = function
    | [] -> acc
    | hd :: tl -> score' (acc + (hd * multiplier)) (multiplier - 1) tl
  in
  score' 0 (List.length player) player

let () = read_input () |> play_game |> score |> Printf.printf "%d\n"
