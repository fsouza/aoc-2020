open StdLabels
module S = Set.Make (Int)

type instruction = Nop | Acc of int | Jmp of int

let parse_instruction = function
  | "acc", n -> Acc (int_of_string n)
  | "jmp", n -> Jmp (int_of_string n)
  | _ -> Nop

let read_input () =
  let rec read_input acc =
    try
      let line = read_line () in
      read_input (line :: acc)
    with End_of_file ->
      acc
      |> List.rev
      |> List.map ~f:(fun v ->
             match String.split_on_char ~sep:' ' v with
             | [ inst; n ] -> parse_instruction (inst, n)
             | _ -> invalid_arg "invalid input")
      |> Array.of_list
  in
  read_input []

let () =
  let instructions = read_input () in
  let rec exec visited p acc =
    if S.mem p visited then acc
    else
      let visited = S.add p visited in
      match instructions.(p) with
      | Nop -> exec visited (p + 1) acc
      | Acc delta -> exec visited (p + 1) (acc + delta)
      | Jmp jmp -> exec visited (p + jmp) acc
  in
  exec S.empty 0 0 |> string_of_int |> print_endline
