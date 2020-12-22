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

let print_entry entry =
  Printf.printf "entry:\n ingredients: [%s]\n allergens: [%s]\n\n"
    (entry.ingredients |> String.concat ~sep:", ")
    (entry.allergens |> String.concat ~sep:", ")

let print_map =
  M.iter ~f:(fun ~key ~data ->
      Printf.printf "%s: [%s]\n" key
        (data |> S.to_seq |> List.of_seq |> String.concat ~sep:", "))

let () =
  read_input ()
  |> List.fold_left ~init:M.empty ~f:(fun m entry ->
         let allergens = entry.allergens |> List.to_seq |> S.of_seq in
         entry.ingredients
         |> List.fold_left ~init:m ~f:(fun m ingredient ->
                match M.find_opt ingredient m with
                | None -> M.add ~key:ingredient ~data:allergens m
                | Some s -> M.add ~key:ingredient ~data:(S.inter s allergens) m))
  |> print_map
