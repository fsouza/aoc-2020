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

let rec process_items i items =
  let length = Array.length items in
  if i = length then items
  else (
    Array.stable_sort
      ~cmp:(fun (_, s1) (_, s2) -> S.cardinal s1 - S.cardinal s2)
      items;
    let _, i_allergens = items.(i) in
    for j = i + 1 to length - 1 do
      let ing, allergens = items.(j) in
      items.(j) <- (ing, S.diff allergens i_allergens)
    done;
    process_items (i + 1) items)

let build_dangerous_ingredient_list arr =
  Array.stable_sort ~cmp:(fun (k1, _) (k2, _) -> String.compare k1 k2) arr;
  arr
  |> Array.fold_left ~init:[] ~f:(fun acc (_, s) ->
         assert (S.cardinal s = 1);
         S.find_first ~f:(Fun.const true) s :: acc)
  |> List.rev
  |> String.concat ~sep:","

let () =
  let entries = read_input () in
  entries
  |> List.fold_left ~init:M.empty ~f:(fun m entry ->
         let ingredients = entry.ingredients |> List.to_seq |> S.of_seq in
         entry.allergens
         |> List.fold_left ~init:m ~f:(fun m allergen ->
                match M.find_opt allergen m with
                | None -> M.add ~key:allergen ~data:ingredients m
                | Some s -> M.add ~key:allergen ~data:(S.inter s ingredients) m))
  |> M.to_seq
  |> Array.of_seq
  |> process_items 0
  |> build_dangerous_ingredient_list
  |> print_endline
