open StdLabels

let read_input () =
  let rec read_input acc =
    try
      let line = read_line () in
      read_input (line :: acc)
    with End_of_file -> acc |> List.rev
  in
  read_input []

let parse_input = function
  | [ first_line; second_line ] ->
      ( int_of_string first_line,
        String.split_on_char ~sep:',' second_line
        |> List.mapi ~f:(fun i bus -> (i, bus))
        |> List.filter_map ~f:(fun (i, bus) ->
               int_of_string_opt bus |> Option.map (fun bus_id -> (i, bus_id)))
      )
  | _ -> failwith "invalid input"

let wait_time n m =
  let diff = n mod m in
  if diff = 0 then diff else m - diff

let rec look_for_answer offset_buses =
  let delta =
    match offset_buses with
    | (_, bus) :: _ -> bus
    | _ -> 1
  in
  let rec find_answer t = function
    | [] -> t
    | (offset, bus) :: tl when (t + offset) mod bus <> 0 ->
        find_answer (t + delta) offset_buses
    | _ :: tl -> find_answer t tl
  in
  find_answer delta offset_buses

let () =
  let input = read_input () in
  let _, offset_buses = parse_input input in
  look_for_answer offset_buses |> string_of_int |> print_endline
