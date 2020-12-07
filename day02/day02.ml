open StdLabels

type line = { min : int; max : int; letter : char; password : string }

let parse_line line =
  match String.split_on_char ~sep:' ' line with
  | [ min_max; letter_with_colon; password ] -> (
      match String.split_on_char ~sep:'-' min_max with
      | [ min; max ] ->
          {
            min = int_of_string min;
            max = int_of_string max;
            letter = letter_with_colon.[0];
            password;
          }
      | _ -> invalid_arg "invalid input")
  | _ -> invalid_arg "invalid input"

let read_input () =
  let rec read_input acc =
    try
      let line = read_line () in
      read_input (line :: acc)
    with End_of_file -> List.rev acc |> List.map ~f:parse_line
  in
  read_input []

let seq_length s = Seq.fold_left (fun acc _ -> acc + 1) 0 s

let is_line_valid_policy1 { min; max; letter; password } =
  let count =
    String.to_seq password |> Seq.filter (Char.equal letter) |> seq_length
  in
  count >= min && count <= max

let is_line_valid_policy2 { min = pos1; max = pos2; letter; password } =
  let check = Char.equal letter in
  let pos1_is_char = check password.[pos1 - 1] in
  let pos2_is_char = check password.[pos2 - 1] in
  pos1_is_char <> pos2_is_char

let count_valid_passwords predicate () =
  read_input () |> List.to_seq |> Seq.filter predicate |> seq_length

let part01 = count_valid_passwords is_line_valid_policy1

let part02 = count_valid_passwords is_line_valid_policy2

let () = part02 () |> string_of_int |> print_endline
