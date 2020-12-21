open StdLabels
open MoreLabels
module M = Map.Make (String)
module S = Set.Make (Int)
module IntMap = Map.Make (Int)

let parse_tile_title title = String.sub ~pos:5 ~len:4 title |> int_of_string

let parse_tile_contents rows =
  rows
  |> List.map ~f:(fun row -> row |> String.to_seq |> Array.of_seq)
  |> Array.of_list

let grab_borders rows =
  let left, right =
    rows
    |> Array.fold_left ~init:([], []) ~f:(fun (left, right) row ->
           let length = Array.length row in
           (row.(0) :: left, row.(length - 1) :: right))
  in
  [
    rows.(0) |> Array.to_seq |> String.of_seq;
    right |> List.to_seq |> String.of_seq;
    rows.(Array.length rows - 1) |> Array.to_seq |> String.of_seq;
    left |> List.to_seq |> String.of_seq;
  ]

type tile = {
  id : int;
  body : char array array;
  top : char array;
  right : char array;
  bottom : char array;
  left : char array;
}

let make_tile id rows =
  let length = Array.length rows in
  let body =
    rows
    |> Array.sub ~pos:1 ~len:(length - 2)
    |> Array.map ~f:(fun row ->
           let row_length = Array.length row in
           row |> Array.sub ~pos:1 ~len:(row_length - 2))
  in
  let left, right =
    rows
    |> Array.fold_left ~init:([], []) ~f:(fun (left, right) row ->
           let length = Array.length row in
           (row.(0) :: left, row.(length - 1) :: right))
  in
  {
    id;
    body;
    top = rows.(0);
    bottom = rows.(length - 1);
    left = left |> Array.of_list;
    right = right |> Array.of_list;
  }

let parse_tile tile =
  match String.split_on_char ~sep:'\n' tile with
  | hd :: tl ->
      let tile_contents = parse_tile_contents tl in
      let id = parse_tile_title hd in
      (id, grab_borders tile_contents, make_tile id tile_contents)
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
  in
  read_input []

let get_pairings tiles =
  tiles
  |> List.fold_left ~init:M.empty ~f:(fun m (tile_id, borders, _) ->
         borders
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
  |> M.fold ~init:[] ~f:(fun ~key:_ ~data acc ->
         match data |> S.to_seq |> List.of_seq with
         | [ tile1; tile2 ] -> (tile1, tile2) :: acc
         | [ _ ] -> acc
         | _ -> failwith "invalid input")
  |> List.fold_left ~init:IntMap.empty ~f:(fun m (tile1, tile2) ->
         match IntMap.find_opt tile1 m with
         | None -> IntMap.add ~key:tile1 ~data:[ tile2 ] m
         | Some tiles -> IntMap.add ~key:tile1 ~data:(tile2 :: tiles) m)

let get_tiles_map tiles =
  tiles
  |> List.fold_left ~init:IntMap.empty ~f:(fun m (tile_id, _, tile) ->
         IntMap.add ~key:tile_id ~data:tile m)

let get_corner_tiles pairings =
  IntMap.fold ~init:[] ~f:(fun ~key ~data acc ->
      if List.length data = 2 then key :: acc else acc)

let () =
  let tiles = read_input () in
  let pairings = get_pairings tiles in
  let tiles_map = get_tiles_map tiles in
  let corners = get_corner_tiles pairings in
  let tiles_per_row =
    tiles_map |> IntMap.cardinal |> float_of_int |> sqrt |> int_of_float
  in
  (* I have the corners, I have the connections, but I don't have the tiles in
     * the proper position - I don't know if I have to rotate and/or flip the tiles.
     * This is gonna be fun. *)
  failwith "TODO: build the actual image"
