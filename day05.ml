open StdLabels

let read_input () =
  let rec read_input acc =
    try
      let line = read_line () in
      read_input (line :: acc)
    with End_of_file -> acc |> List.rev
  in
  read_input []

let find ~lower_char ~upper_char lower upper str =
  let length = String.length str in
  let rec loop lower upper idx =
    if idx = length then lower
    else
      let h = lower + ((upper - lower) / 2) in
      match str.[idx] with
      | c when c = lower_char -> loop lower h (idx + 1)
      | c when c = upper_char -> loop (h + 1) upper (idx + 1)
      | _ -> invalid_arg "invalid input"
  in
  loop lower upper 0

let parse_row = find ~lower_char:'F' ~upper_char:'B' 0 127

let parse_col = find ~lower_char:'L' ~upper_char:'R' 0 7

let get_seat_id repr =
  let row_part = String.sub ~pos:0 ~len:7 repr in
  let col_part = String.sub ~pos:7 ~len:3 repr in
  (parse_row row_part * 8) + parse_col col_part

let get_seats () = read_input () |> List.map ~f:get_seat_id

let part1 () = get_seats () |> List.fold_left ~f:max ~init:0

let part2 () =
  let seats = get_seats () |> Array.of_list in
  let n_seats = Array.length seats in
  seats |> Array.sort ~cmp:( - );
  seats
  |> Array.iteri ~f:(fun i seat ->
         if i < n_seats - 1 && seats.(i + 1) - seat = 2 then print_int (seat + 1))

let () =
  part2 ();
  print_newline ()
