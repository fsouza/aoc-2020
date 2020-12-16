open StdLabels
module M = Map.Make (String)
module S = Set.Make (String)

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

let is_valid n (rule1, rule2) =
  let is_valid' ticket rule = ticket >= rule.min && ticket <= rule.max in
  is_valid' n rule1 || is_valid' n rule2

let is_ticket_valid rules ticket =
  ticket |> List.for_all ~f:(fun n -> rules |> List.exists ~f:(is_valid n))

let string_of_s s =
  S.fold (fun elt acc -> Printf.sprintf "%s '%s'" acc elt) s ""

let classify tickets rule_map =
  let field_names = rule_map |> M.bindings |> List.map ~f:fst in
  let fields = Array.make (List.length field_names) (S.of_list field_names) in
  tickets
  |> List.iter
       ~f:
         (List.iteri ~f:(fun i n ->
              M.iter
                (fun field_name rule_pair ->
                  match S.find_opt field_name fields.(i) with
                  | None -> ()
                  | Some v ->
                      if not (is_valid n rule_pair) then
                        fields.(i) <- S.remove field_name fields.(i))
                rule_map));
  fields
  |> Array.to_list
  |> List.mapi ~f:(fun i s -> (s, i))
  |> List.sort ~cmp:(fun (s1, _) (s2, _) -> S.cardinal s1 - S.cardinal s2)

let () =
  let input = read_input () in
  let rules = M.to_seq input.rules |> Seq.map snd |> List.of_seq in
  let tickets = input.nearby |> List.filter ~f:(is_ticket_valid rules) in
  let field_map, _ =
    classify tickets input.rules
    |> List.fold_left ~init:([], S.empty) ~f:(fun (items, removed) (s, i) ->
           let s = S.diff s removed in
           ((s, i) :: items, S.union s removed))
  in
  let prefix = "departure " in
  let prefix_len = String.length prefix in
  let my_ticket = input.mine |> Array.of_list in
  field_map
  |> List.map ~f:(fun (s, i) -> (S.find_first (Fun.const true) s, i))
  |> List.filter ~f:(fun (field_name, _) ->
         let len = String.length field_name in
         len > prefix_len
         && String.sub ~pos:0 ~len:prefix_len field_name = prefix)
  |> List.map ~f:(fun (_, i) -> my_ticket.(i))
  |> List.fold_left ~init:1 ~f:( * )
  |> string_of_int
  |> print_endline
