open StdLabels
module S = Set.Make (Int)

let read_input () =
  let rec read_input acc =
    try
      let line = read_line () in
      read_input (line :: acc)
    with End_of_file ->
      acc |> List.rev |> List.map ~f:int_of_string |> Array.of_list
  in
  read_input []

let find_pair ~left ~right ~target arr =
  let rec loop s i =
    if i = right then None
    else
      let n = arr.(i) in
      let other = target - n in
      if S.mem other s then Some (other, n) else loop (S.add n s) (i + 1)
  in
  loop S.empty left

let find_number arr preamble =
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
  loop 0

let arr_sum arr left right =
  let sum = ref 0 in
  for i = left to right do
    sum := !sum + arr.(i)
  done;
  !sum

let largest_smallest arr left right =
  let largest = ref arr.(left) in
  let smallest = ref arr.(left) in
  for i = left + 1 to right do
    if arr.(i) > !largest then largest := arr.(i);
    if arr.(i) < !smallest then smallest := arr.(i)
  done;
  (!largest, !smallest)

let find_weakness arr target =
  let length = Array.length arr in
  let rec loop left right =
    if left = length then invalid_arg "cant find it"
    else if right = length then loop (left + 1) (left + 2)
    else
      let sum = arr_sum arr left right in
      if sum = target then
        let largest, smallest = largest_smallest arr left right in
        largest + smallest
      else if sum > target then loop (left + 1) (left + 2)
      else loop left (right + 1)
  in
  loop 0 1

let () =
  let arr = read_input () in
  let preamble = Sys.argv.(1) |> int_of_string in
  let number = find_number arr preamble in
  match number with
  | None -> invalid_arg "you gave me a valid sequence"
  | Some v -> find_weakness arr v |> string_of_int |> print_endline
