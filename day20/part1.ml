open StdLabels
open MoreLabels
module M = Map.Make (String)
module S = Set.Make (Int)

let parse_tile_title title = String.sub ~pos:5 ~len:4 title |> int_of_string

let rec last = function
  | [] -> invalid_arg "empty list"
  | [ hd ] -> hd
  | _ :: tl -> last tl

let make_string l = l |> List.to_seq |> String.of_seq

let grab_borders rows =
  let left, right =
    rows
    |> List.fold_left ~init:([], []) ~f:(fun (left, right) row ->
           let length = String.length row in
           (row.[0] :: left, row.[length - 1] :: right))
  in
  (List.hd rows, make_string right, last rows, make_string left)

let parse_tile tile =
  match String.split_on_char ~sep:'\n' tile with
  | hd :: tl -> (parse_tile_title hd, grab_borders tl)
  | _ -> failwith "invalid input"

let read_input () =
  let rec read_input acc =
    try
      let line = read_line () in
      read_input (line :: acc)
    with End_of_file ->
      let nn = Str.regexp "\n\n" in
      acc
      |> List.rev
      |> String.concat ~sep:"\n"
      |> Str.split nn
      |> List.map ~f:parse_tile
      |> List.fold_left ~init:M.empty
           ~f:(fun m (tile_id, (top, right, bottom, left)) ->
             [ top; right; bottom; left ]
             |> List.map ~f:(fun row ->
                    let rev =
                      row
                      |> String.to_seq
                      |> List.of_seq
                      |> List.rev
                      |> List.to_seq
                      |> String.of_seq
                    in
                    [ row; rev ])
             |> List.flatten
             |> List.fold_left ~init:m ~f:(fun m row ->
                    match M.find_opt row m with
                    | None -> M.add ~key:row ~data:(S.singleton tile_id) m
                    | Some tiles -> M.add ~key:row ~data:(S.add tile_id tiles) m))
  in
  read_input []

let arrange_tiles m = m

let () =
  read_input ()
  |> M.iter ~f:(fun ~key ~data ->
         Printf.printf "%s: [%s]\n" key
           (data
           |> S.to_seq
           |> Seq.map string_of_int
           |> List.of_seq
           |> String.concat ~sep:", "))
