open StdLabels
module S = Set.Make (Int)

type instruction = Nop of int | Acc of int | Jmp of int

let parse_instruction = function
  | "acc", n -> Acc (int_of_string n)
  | "jmp", n -> Jmp (int_of_string n)
  | "nop", n -> Nop (int_of_string n)
  | _ -> invalid_arg "invalid input"

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

(* alternatively, we could have a function that tries to execute once,
 * returning either the value of acc if it can finish or the set of visited
 * instructions if it loops,
 * Then we consume such set and try to excute again with jmps changed to nops
 * and vice-versa (for each jmp or nop in the set) *)
let rec brute_force instructions idx =
  let length = Array.length instructions in
  let rec exec visited p acc =
    if S.mem p visited then None
    else if p >= length then Some acc
    else
      let visited = S.add p visited in
      match instructions.(p) with
      | Nop _ -> exec visited (p + 1) acc
      | Acc delta -> exec visited (p + 1) (acc + delta)
      | Jmp jmp -> exec visited (p + jmp) acc
  in
  match instructions.(idx) with
  | Acc _ -> brute_force instructions (idx + 1)
  | Jmp n -> (
      instructions.(idx) <- Nop n;
      match exec S.empty 0 0 with
      | None ->
          instructions.(idx) <- Jmp n;
          brute_force instructions (idx + 1)
      | Some v -> v)
  | Nop n -> (
      instructions.(idx) <- Jmp n;
      match exec S.empty 0 0 with
      | None ->
          instructions.(idx) <- Nop n;
          brute_force instructions (idx + 1)
      | Some v -> v)

let () =
  let instructions = read_input () in
  brute_force instructions 0 |> string_of_int |> print_endline
