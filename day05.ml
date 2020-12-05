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

let part1 () =
  read_input () |> List.map ~f:get_seat_id |> List.fold_left ~f:max ~init:0

let () = part1 () |> string_of_int |> print_endline
