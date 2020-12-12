open StdLabels

type waypoint = { x : int; y : int }

type position = { x : int; y : int; waypoint : waypoint }

let rotate (waypoint : waypoint) (degrees : int) =
  let d = degrees / 90 in
  let d = if d < 0 then 4 + d else d in
  match d with
  | 1 -> { x = waypoint.y; y = -waypoint.x }
  | 2 -> { x = -waypoint.x; y = -waypoint.y }
  | 3 -> { x = -waypoint.y; y = waypoint.x }
  | _ -> waypoint

let move position offset =
  {
    position with
    x = position.x + (offset * position.waypoint.x);
    y = position.y + (offset * position.waypoint.y);
  }

let initial_position = { x = 0; y = 0; waypoint = { x = 10; y = 1 } }

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
  | 'L', deg -> { position with waypoint = rotate position.waypoint (-deg) }
  | 'R', deg -> { position with waypoint = rotate position.waypoint deg }
  | 'N', offset ->
      {
        position with
        waypoint = { position.waypoint with y = position.waypoint.y + offset };
      }
  | 'E', offset ->
      {
        position with
        waypoint = { position.waypoint with x = position.waypoint.x + offset };
      }
  | 'S', offset ->
      {
        position with
        waypoint = { position.waypoint with y = position.waypoint.y - offset };
      }
  | 'W', offset ->
      {
        position with
        waypoint = { position.waypoint with x = position.waypoint.x - offset };
      }
  | 'F', offset -> move position offset
  | _ -> invalid_arg "invalid instruction"

let manhattan_distance position = abs position.x + abs position.y

let () =
  read_input ()
  |> List.fold_left ~init:initial_position ~f:execute_instruction
  |> manhattan_distance
  |> string_of_int
  |> print_endline
