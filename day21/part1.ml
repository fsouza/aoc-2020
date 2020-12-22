open StdLabels
open MoreLabels
module M = Map.Make (String)
module S = Set.Make (String)

type entry = { ingredients : string list; allergens : string list }

let parse_entry row =
  let ingredients, allergens =
    match String.index_opt row '(' with
    | Some idx ->
        let len_prefix_to_remove = String.length "contains " in
        let len_allergens = String.length row - idx in
        ( String.sub ~pos:0 ~len:idx row,
          String.sub
            ~pos:(idx + 1 + len_prefix_to_remove)
            ~len:(len_allergens - 2 - len_prefix_to_remove)
            row )
    | None -> (row, "")
  in
  {
    ingredients = ingredients |> String.trim |> String.split_on_char ~sep:' ';
    allergens =
      allergens |> String.split_on_char ~sep:',' |> List.map ~f:String.trim;
  }

let read_input () =
  let rec read_input acc =
    try
      let line = read_line () in
      read_input (line :: acc)
    with End_of_file -> acc |> List.rev |> List.map ~f:parse_entry
  in
  read_input []

let collect_ingredients =
  List.fold_left ~init:M.empty ~f:(fun m entry ->
      entry.ingredients
      |> List.fold_left ~init:m ~f:(fun m ingredient ->
             let curr_val =
               M.find_opt ingredient m |> Option.value ~default:0
             in
             M.add ~key:ingredient ~data:(curr_val + 1) m))

let diff all = S.fold ~init:all ~f:M.remove

let () =
  let entries = read_input () in
  let all_ingredients = collect_ingredients entries in
  entries
  |> List.fold_left ~init:M.empty ~f:(fun m entry ->
         let ingredients = entry.ingredients |> List.to_seq |> S.of_seq in
         entry.allergens
         |> List.fold_left ~init:m ~f:(fun m allergen ->
                match M.find_opt allergen m with
                | None -> M.add ~key:allergen ~data:ingredients m
                | Some s -> M.add ~key:allergen ~data:(S.inter s ingredients) m))
  |> M.fold ~init:S.empty ~f:(fun ~key:_ ~data s -> S.union s data)
  |> diff all_ingredients
  |> M.fold ~init:0 ~f:(fun ~key:_ ~data acc -> acc + data)
  |> Printf.printf "%d\n"
