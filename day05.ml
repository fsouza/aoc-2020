open StdLabels

let read_input () =
  let rec read_input acc =
    try
      let line = read_line () in
      read_input (line :: acc)
    with End_of_file -> acc |> List.rev
  in
  read_input []

let pow x y = float_of_int x ** float_of_int y |> int_of_float

let find ~bit_char str =
  let length = String.length str in
  let rec loop acc idx =
    if idx = length then acc
    else
      let v = if str.[idx] = bit_char then 1 else 0 in
      loop ((acc lsl 1) lor v) (idx + 1)
  in
  loop 0 0

let parse_row = find ~bit_char:'B'

let parse_col = find ~bit_char:'R'

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
