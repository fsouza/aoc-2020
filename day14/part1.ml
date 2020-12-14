open StdLabels

let parse_instruction line =
  match String.split_on_char ~sep:'=' line with
  | [ lhs; rhs ] -> Some (String.trim lhs, String.trim rhs)
  | _ -> None

let read_input () =
  let rec read_input n acc =
    try
      let line = read_line () in
      read_input (n + 1) (line :: acc)
    with End_of_file ->
      (n, acc |> List.rev |> List.filter_map ~f:parse_instruction)
  in
  read_input 0 []

let parse_mask mask =
  let rec parse_mask' (mask_0, mask_1) = function
    | [] -> (mask_0, mask_1)
    | '0' :: tl -> parse_mask' (mask_0 lsl 1, mask_1 lsl 1) tl
    | '1' :: tl -> parse_mask' ((mask_0 lsl 1) lor 1, (mask_1 lsl 1) lor 1) tl
    | _ :: tl -> parse_mask' ((mask_0 lsl 1) lor 1, mask_1 lsl 1) tl
  in
  parse_mask' (0, 0) (String.to_seq mask |> List.of_seq)

let parse_pos pos =
  match String.split_on_char ~sep:'[' pos with
  | [ _; position ] -> (
      match String.split_on_char ~sep:']' position with
      | [ position; _ ] -> int_of_string position
      | _ -> invalid_arg "invalid position")
  | _ -> invalid_arg "invalid position"

let apply_mask (mask_0, mask_1) v = v lor mask_1 land mask_0

let rec execute mem mask = function
  | [] -> ()
  | ("mask", m) :: tl -> execute mem (parse_mask m) tl
  | (pos, v) :: tl ->
      let pos = parse_pos pos in
      let v = int_of_string v in
      Hashtbl.replace mem pos (apply_mask mask v);
      execute mem mask tl

let print_mem = Hashtbl.iter (Printf.printf "mem[%d] = %d\n")

let () =
  let n, instructions = read_input () in
  let mem = Hashtbl.create n in
  execute mem (0, 0) instructions;
  Hashtbl.fold (fun _ v acc -> acc + v) mem 0 |> string_of_int |> print_endline
