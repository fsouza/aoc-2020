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
  [ List.hd rows; make_string right; last rows; make_string left ]

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
      |> List.fold_left ~init:M.empty ~f:(fun m (tile_id, borders) ->
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
