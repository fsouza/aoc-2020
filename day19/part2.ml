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
  let rec is_valid' idx rule =
    if idx >= length then (true, idx)
    else
      match rule with
      | Char c -> (msg.[idx] = c, idx + 1)
      | Seq s -> all_valid idx s
      | AltSeq (s1, s2) ->
          let s1_valid, s1_idx = is_valid' idx (Seq s1) in
          if s1_valid then (s1_valid, s1_idx) else is_valid' idx (Seq s2)
  and all_valid idx = function
    | [] -> (true, idx)
    | hd :: tl ->
        let is_valid, idx = is_valid' idx (Hashtbl.find rules hd) in
        if not is_valid then (is_valid, idx) else all_valid idx tl
  in
  let r, idx = is_valid' 0 (Hashtbl.find rules rule_key) in
  r && idx = length

let rec print_rule rules = function
  | Char c -> print_char c
  | Seq s ->
      s
      |> List.map ~f:(fun key -> Hashtbl.find rules key)
      |> List.iter ~f:(print_rule rules)
  | AltSeq (s1, s2) ->
      print_rule rules (Seq s1);
      print_rule rules (Seq s2)

(* Notes: when looking at the sample, "aaaabbaaaabbaaa" matches, but it should
 * not. Therefore this implementation isn't correct. Will pick it up later
 * today. *)

let () =
  let rules, messages = read_input () in
  messages
  |> List.filter ~f:(fun msg -> is_valid msg rules "0")
  |> List.iter ~f:print_endline
