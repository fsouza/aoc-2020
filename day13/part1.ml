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
        |> List.filter_map ~f:int_of_string_opt )
  | _ -> failwith "invalid input"

let wait_time n m =
  let diff = n mod m in
  if diff = 0 then diff else m - diff

let () =
  let input = read_input () in
  let ts, buses = parse_input input in
  buses
  |> List.map ~f:(fun bus -> (bus, wait_time ts bus))
  |> List.fold_left ~init:(0, Int.max_int)
       ~f:(fun (curr_bus, curr_wait_time) (bus, wait_time) ->
         if wait_time < curr_wait_time then (bus, wait_time)
         else (curr_bus, curr_wait_time))
  |> fun (bus, wait_time) -> bus * wait_time |> string_of_int |> print_endline
