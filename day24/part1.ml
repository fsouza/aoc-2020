open StdLabels
open MoreLabels

type instruction = East | Southeast | Southwest | West | Northwest | Northeast

let offset = function
  | East -> (0, 2)
  | Southeast -> (-1, 1)
  | Southwest -> (-1, -1)
  | West -> (0, -2)
  | Northwest -> (1, -1)
  | Northeast -> (1, 1)

module S = Set.Make (struct
  type t = int * int

  let compare ((i1, j1) : t) ((i2, j2) : t) : int =
    let cmp_i = Int.compare i1 i2 in
    if cmp_i <> 0 then cmp_i else Int.compare j1 j2
end)

let parse_instruction = function
  | "se" -> Southeast
  | "sw" -> Southwest
  | "nw" -> Northwest
  | "ne" -> Northeast
  | _ -> failwith "invalid instruction"

let parse_line line =
  let length = String.length line in
  let rec parse_line' acc i =
    if i = length then acc |> List.rev
    else
      match line.[i] with
      | 'e' -> parse_line' (East :: acc) (i + 1)
      | 'w' -> parse_line' (West :: acc) (i + 1)
      | ch when ch = 's' || ch = 'n' ->
          parse_line'
            ((line |> String.sub ~pos:i ~len:2 |> parse_instruction) :: acc)
            (i + 2)
      | _ -> failwith "invalid input"
  in
  parse_line' [] 0

let read_input () =
  let rec read_input acc =
    try
      let line = read_line () in
      read_input (line :: acc)
    with End_of_file -> acc |> List.rev |> List.map ~f:parse_line
  in
  read_input []

let rec navigate coordinates =
  List.fold_left ~init:coordinates ~f:(fun (i, j) instruction ->
      let offset_i, offset_j = offset instruction in
      (i + offset_i, j + offset_j))

let process_line black_tiles instructions =
  let coordinates = navigate (0, 0) instructions in
  if S.mem coordinates black_tiles then S.remove coordinates black_tiles
  else S.add coordinates black_tiles

let () =
  read_input ()
  |> List.fold_left ~init:S.empty ~f:process_line
  |> S.cardinal
  |> Printf.printf "%d\n"
