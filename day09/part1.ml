open StdLabels
module S = Set.Make (Int64)

let read_input () =
  let rec read_input acc =
    try
      let line = read_line () in
      read_input (line :: acc)
    with End_of_file ->
      acc |> List.rev |> List.map ~f:Int64.of_string |> Array.of_list
  in
  read_input []

let find_pair ~left ~right ~target arr =
  let rec loop s i =
    if i = right then None
    else
      let n = arr.(i) in
      let other = Int64.sub target n in
      if S.mem other s then Some (other, n) else loop (S.add n s) (i + 1)
  in
  loop S.empty left

let () =
  let preamble = Sys.argv.(1) |> int_of_string in
  let arr = read_input () in
  let length = Array.length arr in
  let rec loop left =
    let right = left + preamble in
    if right + 1 = length then None
    else
      let target = arr.(right) in
      match find_pair ~left ~right ~target arr with
      | None -> Some target
      | Some _ -> loop (left + 1)
  in
  loop 0 |> Option.value ~default:(-1L) |> Int64.to_string |> print_endline
