open StdLabels
module M = Map.Make (String)
module S = Set.Make (String)

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
  | (_, inner_bag) :: tl -> (
      match M.find_opt inner_bag m with
      | None -> add_bag_to_map (M.add inner_bag [ bag ] m) bag tl
      | Some v -> add_bag_to_map (M.add inner_bag (bag :: v) m) bag tl)

let build_map =
  let rec build_map' m = function
    | [] -> m
    | (bag, contents) :: tl -> build_map' (add_bag_to_map m bag contents) tl
  in
  build_map' M.empty

let read_input () =
  let rec read_input acc =
    try
      let line = read_line () in
      read_input (line :: acc)
    with End_of_file -> acc |> List.map ~f:parse_line |> build_map
  in
  read_input []

let () =
  let m = read_input () in
  let rec loop visited = function
    | [] ->
        (* subtract 1 because the initial bag shouldn't be counted *)
        S.cardinal visited - 1
    | hd :: tl -> (
        match M.find_opt hd m with
        | None -> loop (S.add hd visited) tl
        | Some v ->
            let visited = S.add hd visited in
            let filtered_list =
              List.filter ~f:(Fun.negate @@ Fun.flip S.mem visited) v
            in
            loop visited (tl @ filtered_list))
  in
  loop S.empty [ "shiny gold" ] |> string_of_int |> print_endline
