open StdLabels
open MoreLabels
module M = Map.Make (String)
module S = Set.Make (Int)
module IntMap = Map.Make (Int)

module RevMap = Map.Make (struct
  type t = int * int

  let compare ((t1_1, t2_1) : t) ((t1_2, t2_2) : t) : int =
    let cmp_1 = Int.compare t1_1 t1_2 in
    let cmp_2 = Int.compare t2_1 t2_2 in
    if cmp_1 <> 0 then cmp_1 else cmp_2
end)

let parse_tile_title title = String.sub ~pos:5 ~len:4 title |> int_of_string

let make_string l = l |> List.to_seq |> String.of_seq

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

let remove_borders rows =
  let length = Array.length rows in
  rows
  |> Array.sub ~pos:1 ~len:(length - 2)
  |> Array.map ~f:(fun row ->
         let row_length = Array.length row in
         row |> Array.sub ~pos:1 ~len:(row_length - 2))

let parse_tile tile =
  match String.split_on_char ~sep:'\n' tile with
  | hd :: tl ->
      let tile_contents = parse_tile_contents tl in
      ( parse_tile_title hd,
        grab_borders tile_contents,
        tile_contents |> remove_borders )
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

let () =
  read_input ()
  |> M.filter ~f:(fun _ tiles -> S.cardinal tiles = 2)
  |> M.map ~f:(fun tile_ids ->
         match tile_ids |> S.to_seq |> List.of_seq with
         | [ id1; id2 ] -> (id1, id2)
         | _ -> assert false)
  |> M.fold ~init:RevMap.empty ~f:(fun ~key ~data ->
         RevMap.add ~key:data ~data:key)
  |> RevMap.fold ~init:[] ~f:(fun ~key:(t1, t2) ~data:_ acc -> t1 :: t2 :: acc)
  |> List.fold_left ~init:IntMap.empty ~f:(fun m tile_id ->
         let curr = IntMap.find_opt tile_id m |> Option.value ~default:0 in
         IntMap.add ~key:tile_id ~data:(curr + 1) m)
  |> IntMap.filter ~f:(fun _ value -> value = 2)
  |> IntMap.fold ~init:1 ~f:(fun ~key ~data:_ acc -> acc * key)
  |> Printf.printf "%d\n"
