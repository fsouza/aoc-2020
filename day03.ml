open StdLabels

let read_input () =
  let rec read_input acc =
    try
      let line = read_line () in
      read_input (line :: acc)
    with End_of_file -> acc |> List.rev |> Array.of_list
  in
  read_input []

let count_trees ~right ~down grid =
  let cols = String.length grid.(0) in
  let is_tree = Char.equal '#' in
  let rec loop row col acc =
    if row >= Array.length grid then acc
    else
      let curr_row = grid.(row) in
      let chr = curr_row.[col] in
      let new_acc = if is_tree chr then acc + 1 else acc in
      loop (row + down) ((col + right) mod cols) new_acc
  in
  loop 0 0 0

let part01 input =
  input |> count_trees ~right:3 ~down:1 |> string_of_int |> print_endline

let part02 input =
  let answer_p1 = 153 in
  [ (1, 1); (5, 1); (7, 1); (1, 2) ]
  |> List.map ~f:(fun (right, down) -> input |> count_trees ~right ~down)
  |> List.fold_left ~init:answer_p1 ~f:(fun acc v -> acc * v)
  |> string_of_int |> print_endline

let () = read_input () |> part02
