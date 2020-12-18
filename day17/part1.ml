open StdLabels
open MoreLabels

module S = Set.Make (struct
  type t = int * int * int

  let compare ((i1, j1, k1) : int * int * int) ((i2, j2, k2) : int * int * int)
      : int =
    let comp_i = compare i1 i2 in
    let comp_j = compare j1 j2 in
    let comp_k = compare k1 k2 in
    if comp_i <> 0 then comp_i else if comp_j <> 0 then comp_j else comp_k
end)

let parse_input rows =
  rows
  |> List.mapi ~f:(fun i row -> (i, row))
  |> List.fold_left ~init:S.empty ~f:(fun s (i, row) ->
         row
         |> String.to_seqi
         |> Seq.fold_left
              (fun s (j, ch) -> if ch = '#' then S.add (i, j, 0) s else s)
              s)

let read_input () =
  let rec read_input acc =
    try
      let line = read_line () in
      read_input (line :: acc)
    with End_of_file -> acc |> List.rev |> parse_input
  in
  read_input []

let get_adj_indices (i, j, k) =
  let adj = ref [] in
  for i_offset = -1 to 1 do
    for j_offset = -1 to 1 do
      for k_offset = -1 to 1 do
        if not (i_offset = 0 && j_offset = 0 && k_offset = 0) then
          adj := (i + i_offset, j + j_offset, k + k_offset) :: !adj
      done
    done
  done;
  !adj

let count_active_adjacent set (i, j, k) =
  let all_adj_indices = get_adj_indices (i, j, k) in
  all_adj_indices |> List.filter ~f:(Fun.flip S.mem set) |> List.length

let handle_active active_set shadow (i, j, k) =
  let n_active_adj = count_active_adjacent shadow (i, j, k) in
  if n_active_adj = 2 || n_active_adj = 3 then S.add (i, j, k) active_set
  else S.remove (i, j, k) active_set

let handle_inactive active_set shadow (i, j, k) =
  let n_active_adj = count_active_adjacent shadow (i, j, k) in
  if n_active_adj = 3 then S.add (i, j, k) active_set
  else S.remove (i, j, k) active_set

let run_iteration active_set inactive_set =
  S.union active_set inactive_set
  |> S.fold ~init:S.empty ~f:(fun triple s ->
         let fn =
           if S.mem triple active_set then handle_active else handle_inactive
         in
         fn s active_set triple)

let build_inactive_set active_set =
  active_set
  |> S.fold ~init:S.empty ~f:(fun triple s ->
         S.union s (S.diff (get_adj_indices triple |> S.of_list) active_set))

let rec run active_set = function
  | 0 -> active_set
  | n when n < 0 -> failwith "invalid target"
  | n -> run (run_iteration active_set (build_inactive_set active_set)) (n - 1)

let () =
  let s = read_input () in
  s |> Fun.flip run 6 |> S.cardinal |> string_of_int |> print_endline
