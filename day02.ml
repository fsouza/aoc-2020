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

let is_line_valid { min; max; letter; password } =
  let length =
    String.to_seq password |> Seq.filter (Char.equal letter) |> seq_length
  in
  length >= min && length <= max

let part01 () =
  read_input () |> List.to_seq |> Seq.filter is_line_valid |> seq_length

let () = part01 () |> string_of_int |> print_endline
