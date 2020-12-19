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

let is_valid msg rules rule_key =
  let length = String.length msg in
  let rec is_valid' idx rule is_main =
    if idx >= length then (not is_main, idx)
    else
      match rule with
      | Char c -> (msg.[idx] = c, idx + 1)
      | Seq s -> all_valid idx is_main s
      | AltSeq (s1, s2) ->
          let s1_valid, s1_idx = is_valid' idx (Seq s1) false in
          if s1_valid then (s1_valid, s1_idx) else is_valid' idx (Seq s2) false
  and all_valid idx is_main = function
    | [] -> (true, idx)
    | hd :: tl ->
        if is_main && idx >= length then (false, idx)
        else
          let is_valid, idx = is_valid' idx (Hashtbl.find rules hd) false in
          if not is_valid then (is_valid, idx) else all_valid idx is_main tl
  in
  let rule = Hashtbl.find rules rule_key in
  let r, idx = is_valid' 0 rule true in
  r && idx = length

(* so fun fact: "a" passes with 8, and "a" passes with 11, but "aa" doesn't
 * pass with 0 (which is '8 11'). Hopefully, if I fix that, it should pass the
 * regular input.
 *
 * aaaabbaaaabbaaa is here because when I run the sample, I get 13 results
 * instead of 12, and that's the incorrect one. *)
let cases =
  [
    ("a", "8", true);
    ("a", "11", true);
    ("aa", "0", true);
    ("aaaabbaaaabbaaa", "0", false);
  ]

let test_cases rules =
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
  if !run_tests then test_cases rules
  else
    messages
    |> List.filter ~f:(fun msg -> is_valid msg rules rule_key)
    |> List.length
    |> Printf.printf "%d\n"
