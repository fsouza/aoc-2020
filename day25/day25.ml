open StdLabels

let read_input () =
  let rec read_input acc =
    try
      let line = read_line () in
      read_input (line :: acc)
    with End_of_file -> (
      acc
      |> List.rev
      |> List.map ~f:int_of_string
      |> function
      | [ first; second ] -> (first, second)
      | _ -> failwith "invalid input")
  in
  read_input []

let rec find_loop_size target value subject_number n =
  if value = target then n
  else
    find_loop_size target
      (value * subject_number mod 20201227)
      subject_number (n + 1)

let rec run_loop value subject_number n =
  if n = 0 then value
  else run_loop (value * subject_number mod 20201227) subject_number (n - 1)

let subject_number = 7

let () =
  read_input ()
  |> (fun (first, second) ->
       let first_loop_size = find_loop_size first 1 subject_number 0 in
       (second, first_loop_size))
  |> fun (public_key, loop_size) ->
  run_loop 1 public_key loop_size |> Printf.printf "%d\n"
