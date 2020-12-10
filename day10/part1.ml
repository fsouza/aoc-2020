open StdLabels

let read_input () =
  let rec read_input acc =
    try
      let line = read_line () in
      read_input (line :: acc)
    with End_of_file ->
      acc |> List.map ~f:int_of_string |> List.sort ~cmp:compare
  in
  read_input []

let find_differences =
  let rec find_answer' (diff1, diff3) prev = function
    | [] -> (diff1, diff3 + 1)
    | hd :: tl ->
        let diff1, diff3 =
          match hd - prev with
          | 1 -> (diff1 + 1, diff3)
          | 2 -> (diff1, diff3)
          | 3 -> (diff1, diff3 + 1)
          | _ -> invalid_arg "invalid input"
        in
        find_answer' (diff1, diff3) hd tl
  in
  find_answer' (0, 0) 0

let () =
  let diff1, diff3 = read_input () |> find_differences in
  diff1 * diff3 |> string_of_int |> print_endline
