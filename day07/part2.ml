open StdLabels
module M = Map.Make (String)

let parse_bag =
  let bag = Str.regexp " bags?\\.?" in
  Str.replace_first bag ""

let parse_content content =
  let length = String.length content in
  let n =
    String.sub ~pos:0 ~len:1 content
    |> int_of_string_opt
    |> Option.value ~default:0
  in
  (n, content |> String.sub ~pos:2 ~len:(length - 2) |> parse_bag)

let parse_items (bag, contents) =
  ( parse_bag bag,
    contents
    |> String.split_on_char ~sep:','
    |> List.map ~f:String.trim
    |> List.map ~f:parse_content )

let parse_line l =
  let contain_re = Str.regexp " contain " in
  match Str.split contain_re l with
  | [ bag; contents ] -> parse_items (bag, contents)
  | _ -> invalid_arg "invalid input"

let rec add_bag_to_map m bag = function
  | [] -> m
  | (hd, _) :: tl when hd = 0 -> add_bag_to_map m bag tl
  | (n, inner_bag) :: tl -> (
      match M.find_opt inner_bag m with
      | None -> add_bag_to_map (M.add inner_bag [ bag ] m) bag tl
      | Some v -> add_bag_to_map (M.add inner_bag (bag :: v) m) bag tl)

let read_input () =
  let rec read_input acc =
    try
      let line = read_line () in
      read_input (line :: acc)
    with End_of_file ->
      acc |> List.map ~f:parse_line |> List.to_seq |> M.of_seq
  in
  read_input []

let () =
  let m = read_input () in
  let rec pair_value = function
    | 0, _ -> 0
    | n, inner_bag -> n * bag_value inner_bag
  and bag_value bag =
    match M.find_opt bag m with
    | None -> 0
    | Some v ->
        v
        |> List.map ~f:(fun (n, inner_bag) -> n + pair_value (n, inner_bag))
        |> List.fold_left ~f:( + ) ~init:0
  in
  bag_value "shiny gold" |> string_of_int |> print_endline
