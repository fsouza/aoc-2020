open StdLabels

type direction = North | East | South | West

let int_of_direction = function
  | North -> 0
  | East -> 1
  | South -> 2
  | West -> 3

let rec direction_of_int = function
  | 0 -> North
  | 1 -> East
  | 2 -> South
  | 3 -> West
  | n when n < 0 -> 4 + n |> direction_of_int
  | n -> Printf.sprintf "invalid int for direction %d" n |> invalid_arg

let rotate direction degrees =
  let d = degrees / 90 in
  direction
  |> int_of_direction
  |> ( + ) d
  |> Fun.flip ( mod ) 4
  |> direction_of_int

type position = { x : int; y : int; bearing : direction }

let move_forward position offset =
  match position.bearing with
  | North -> { position with y = position.y + offset }
  | East -> { position with x = position.x + offset }
  | South -> { position with y = position.y - offset }
  | West -> { position with x = position.x - offset }

let initial_position = { x = 0; y = 0; bearing = East }

let parse_instruction input =
  let action = input.[0] in
  let arg = String.sub input 1 (String.length input - 1) |> int_of_string in
  (action, arg)

let read_input () =
  let rec read_input acc =
    try
      let line = read_line () in
      read_input (line :: acc)
    with End_of_file -> acc |> List.rev |> List.map ~f:parse_instruction
  in
  read_input []

let execute_instruction position = function
  | 'L', deg -> { position with bearing = rotate position.bearing (-deg) }
  | 'R', deg -> { position with bearing = rotate position.bearing deg }
  | 'E', offset -> { position with x = position.x + offset }
  | 'W', offset -> { position with x = position.x - offset }
  | 'N', offset -> { position with y = position.y + offset }
  | 'S', offset -> { position with y = position.y - offset }
  | 'F', offset -> move_forward position offset
  | _ -> invalid_arg "invalid instruction"

let manhattan_distance position = abs position.x + abs position.y

let () =
  read_input ()
  |> List.fold_left ~init:initial_position ~f:execute_instruction
  |> manhattan_distance
  |> string_of_int
  |> print_endline
