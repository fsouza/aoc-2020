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

let rec play = function
  | [], p2 -> p2
  | p1, [] -> p1
  | c1 :: tl1, c2 :: tl2 ->
      if c1 > c2 then play (tl1 @ [ c1; c2 ], tl2)
      else play (tl1, tl2 @ [ c2; c1 ])

let score player =
  let rec score' acc multiplier = function
    | [] -> acc
    | hd :: tl -> score' (acc + (hd * multiplier)) (multiplier - 1) tl
  in
  score' 0 (List.length player) player

let () = read_input () |> play |> score |> Printf.printf "%d\n"
