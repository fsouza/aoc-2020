open StdLabels
open MoreLabels

type rule =
  | Char of char
  | Seq of string list
  | AltSeq of string list * string list

let parse_rule r =
  let parse_seq refs = refs |> String.trim |> String.split_on_char ~sep:' ' in
  match r with
  | "\"a\"" -> Char 'a'
  | "\"b\"" -> Char 'b'
  | r -> (
      match String.split_on_char ~sep:'|' r with
      | [ seq ] -> Seq (parse_seq seq)
      | [ seq1; seq2 ] -> AltSeq (parse_seq seq1, parse_seq seq2)
      | _ -> r |> Printf.sprintf "invalid rule: %s" |> failwith)

let parse_input = function
  | [ rules; messages ] ->
      let rules = String.split_on_char ~sep:'\n' rules in
      let rules_tbl = Hashtbl.create (List.length rules) in
      rules
      |> List.iteri ~f:(fun i r ->
             match String.split_on_char ~sep:':' r with
             | [ key; r ] ->
                 let key = String.trim key in
                 Hashtbl.replace ~key
                   ~data:(r |> String.trim |> parse_rule)
                   rules_tbl
             | _ -> failwith "invalid input");
      (rules_tbl, String.split_on_char ~sep:'\n' messages)
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

let string_of_rule rule =
  let string_of_seq = String.concat ~sep:" " in
  match rule with
  | Char c -> String.make 1 c
  | Seq s -> s |> string_of_seq
  | AltSeq (s1, s2) ->
      Printf.sprintf "%s | %s" (s1 |> string_of_seq) (s2 |> string_of_seq)

let highlighted msg idx =
  msg
  |> String.to_seqi
  |> Seq.map (fun (i, ch) ->
         let fmt_fn =
           if i = idx then Printf.sprintf "[%c]" else Printf.sprintf "%c"
         in
         fmt_fn ch)
  |> List.of_seq
  |> String.concat ~sep:""

let is_valid msg rules_tbl rule_key =
  let length = String.length msg in
  let rec is_valid' idx rule =
    if idx = length then (true, idx)
    else
      match rule with
      | Char c -> if msg.[idx] = c then (true, idx + 1) else (false, -1)
      | Seq s -> all_valid idx s
      | AltSeq (s1, s2) ->
          let s1_valid, s1_idx = all_valid idx s1 in
          let s2_valid, s2_idx = all_valid idx s2 in
          if s1_valid && s2_valid then
            if s1_idx > s2_idx then (s1_valid, s1_idx) else (s2_valid, s2_idx)
          else if s1_valid then (s1_valid, s1_idx)
          else if s2_valid then (s2_valid, s2_idx)
          else (false, -1)
  and all_valid idx = function
    | [] -> (true, idx)
    | hd :: tl ->
        let is_valid, new_idx = is_valid' idx (Hashtbl.find rules_tbl hd) in
        if not is_valid then (false, -1) else all_valid new_idx tl
  in
  let rules =
    match Hashtbl.find rules_tbl rule_key with
    | Seq s -> s |> List.map ~f:(Hashtbl.find rules_tbl)
    | r -> [ r ]
  in
  rules
  |> List.fold_left ~init:(true, 0) ~f:(fun (valid, idx) rule ->
         if not valid then (valid, idx)
         else if idx = length then (false, idx)
         else is_valid' idx rule)
  |> fun (is_valid, idx) -> is_valid && idx = length

(* aaaabbaaaabbaaa is here because when I run the sample, I get 13 results
 * instead of 12, and that's the incorrect one. The hope is that if I can fix
 * that, I'll fix the code for good. *)
let cases =
  [
    ("aa", "18", true);
    ("bb", "18", true);
    ("ba", "18", true);
    ("ab", "18", true);
    ("aba", "27", false);
    ("abb", "27", true);
    ("aab", "27", true);
    ("baa", "27", true);
    ("bab", "27", true);
    ("bba", "27", true);
    ("bbb", "27", true);
    ("aaaabbaaaabbaaa", "0", false);
  ]

let run_test_cases rules =
  cases
  |> List.iter ~f:(fun (msg, rule_key, expected) ->
         let obtained = is_valid msg rules rule_key in
         if obtained <> expected then
           Printf.printf
             "incorrect result for msg '%s' on key '%s': expected %b, obtained \
              %b\n"
             msg rule_key expected obtained)

let () =
  let run_tests = ref false in
  Arg.parse_argv Sys.argv
    [ ("-test", Arg.Set run_tests, "run tests instead of input messages") ]
    (Fun.const ()) "";
  let rule_key = "0" in
  let rules, messages = read_input () in
  Hashtbl.replace ~key:"8" ~data:(AltSeq ([ "42" ], [ "42"; "8" ])) rules;
  Hashtbl.replace ~key:"11"
    ~data:(AltSeq ([ "42"; "31" ], [ "42"; "11"; "31" ]))
    rules;
  if !run_tests then run_test_cases rules
  else
    messages
    |> List.filter ~f:(fun msg -> is_valid msg rules rule_key)
    |> List.length
    |> Printf.printf "%d\n"
