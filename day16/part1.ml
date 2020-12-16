open StdLabels
module M = Map.Make (String)

type rule = { min : int; max : int }

type ticket = int list

type input = { rules : (rule * rule) M.t; mine : ticket; nearby : ticket list }

let parse_rule_pair rule_def =
  let parse_rule r =
    match String.split_on_char ~sep:'-' r with
    | [ min; max ] -> { min = int_of_string min; max = int_of_string max }
    | _ -> r |> Printf.sprintf "invalid rule %s" |> failwith
  in
  match String.split_on_char ~sep:' ' rule_def with
  | [ rule1; "or"; rule2 ] -> (parse_rule rule1, parse_rule rule2)
  | _ -> rule_def |> Printf.sprintf "invalid rule_def %s" |> failwith

let parse_field row =
  match String.split_on_char ~sep:':' row with
  | [ key; rule_val ] -> (key, String.trim rule_val |> parse_rule_pair)
  | _ -> row |> Printf.sprintf "invalid rule %s" |> failwith

let parse_rules =
  List.fold_left ~init:M.empty ~f:(fun m item ->
      let k, v = parse_field item in
      M.add k v m)

let parse_ticket s =
  s |> String.split_on_char ~sep:',' |> List.map ~f:int_of_string

let parse_input = function
  | [ p1; p2; p3 ] ->
      {
        rules = p1 |> String.split_on_char ~sep:'\n' |> parse_rules;
        mine =
          p2
          |> String.split_on_char ~sep:'\n'
          |> List.tl
          |> List.hd
          |> parse_ticket;
        nearby =
          p3
          |> String.split_on_char ~sep:'\n'
          |> List.tl
          |> List.map ~f:parse_ticket;
      }
  | _ -> failwith "invalid input"

let read_input () =
  let rec read_input acc =
    try
      let line = read_line () in
      read_input (line :: acc)
    with End_of_file ->
      let nn = Str.regexp "\n\n" in
      acc |> List.rev |> String.concat ~sep:"\n" |> Str.split nn |> parse_input
  in
  read_input []

let collect_invalid numbers rules =
  let is_valid n (rule1, rule2) =
    let is_valid' ticket rule = ticket >= rule.min && ticket <= rule.max in
    is_valid' n rule1 || is_valid' n rule2
  in
  numbers
  |> List.filter ~f:(fun n ->
         rules |> List.for_all ~f:(Fun.negate (is_valid n)))

let () =
  let input = read_input () in
  let numbers = input.nearby |> List.flatten in
  let rules = M.to_seq input.rules |> Seq.map snd |> List.of_seq in
  collect_invalid numbers rules
  |> List.fold_left ~init:0 ~f:( + )
  |> string_of_int
  |> print_endline
