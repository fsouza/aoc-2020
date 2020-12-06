open StdLabels
module CharSet = Set.Make (Char)

let parse_group lines =
  lines |> String.concat ~sep:"" |> String.to_seq |> CharSet.of_seq

let read_input () =
  let rec read_input groups curr_lines =
    let add_group () = read_input (parse_group curr_lines :: groups) [] in
    try
      let line = read_line () in
      if line = "" then add_group () else read_input groups (line :: curr_lines)
    with End_of_file -> (
      match curr_lines with [] -> groups | _ -> add_group ())
  in
  read_input [] []

let part1 () =
  read_input ()
  |> List.fold_left ~f:(fun acc set -> acc + CharSet.cardinal set) ~init:0

let () = part1 () |> string_of_int |> print_endline
