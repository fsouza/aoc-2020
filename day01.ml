open StdLabels

let read_input () =
  let rec read_input acc =
    try
      let line = read_line () in
      read_input (line :: acc)
    with End_of_file ->
      acc
      |> List.map ~f:int_of_string
      |> List.sort ~cmp:compare
      |> Array.of_list
  in
  read_input []

let find_pair ?(left = 0) input target =
  let rec loop left right =
    if left > right then None
    else
      let left_val = input.(left) in
      let right_val = input.(right) in
      let sum = left_val + right_val in
      if sum == target then Some (left_val, right_val)
      else if sum > target then loop left (right - 1)
      else loop (left + 1) right
  in
  loop left (Array.length input - 1)

let part01 input =
  find_pair input 2020
  |> Option.map (fun (x, y) -> x * y)
  |> Option.value ~default:0
  |> string_of_int
  |> print_endline

let part02 input =
  input
  |> Array.iteri ~f:(fun i elm ->
         match find_pair ~left:(i + 1) input (2020 - elm) with
         | Some (x, y) -> elm * x * y |> string_of_int |> print_endline
         | None -> ())

let () = read_input () |> part02
