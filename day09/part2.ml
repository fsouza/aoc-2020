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

let largest_smallest_in_range arr left right =
  let largest = ref arr.(left) in
  let smallest = ref arr.(left) in
  for i = left + 1 to right do
    if arr.(i) > !largest then largest := arr.(i);
    if arr.(i) < !smallest then smallest := arr.(i)
  done;
  (!largest, !smallest)

let rec get_under_target arr left sum target =
  if sum <= target then (left, sum)
  else get_under_target arr (left + 1) (sum - arr.(left)) target

let find_weakness arr target =
  let length = Array.length arr in
  let rec loop left right sum =
    if left = length || right = length then invalid_arg "cant find it"
    else
      let sum = sum + arr.(right) in
      if sum = target then
        let largest, smallest = largest_smallest_in_range arr left right in
        largest + smallest
      else if sum > target then
        let left, sum = get_under_target arr left sum target in
        loop left right (sum - arr.(right))
      else loop left (right + 1) sum
  in
  loop 0 1 arr.(0)

let () =
  let arr = read_input () in
  let preamble = Sys.argv.(1) |> int_of_string in
  let number = find_number arr preamble in
  match number with
  | None -> invalid_arg "you gave me a valid sequence"
  | Some v -> find_weakness arr v |> string_of_int |> print_endline
