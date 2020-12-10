open StdLabels

let read_input () =
  let rec read_input acc =
    try
      let line = read_line () in
      read_input (line :: acc)
    with End_of_file ->
      acc
      |> List.map ~f:int_of_string
      |> List.sort ~cmp:compare
      |> Array.of_list
  in
  read_input []

let number_of_ways arr =
  let length = Array.length arr in
  let dp = Array.make (length + 1) 0 in
  dp.(0) <- 1;
  arr
  |> Array.iteri ~f:(fun i elm ->
         let dp_idx = i + 1 in
         let dp_start = max 0 (dp_idx - 3) in
         let sum =
           Array.sub dp dp_start 3
           |> Array.to_list
           |> List.filteri ~f:(fun idx item ->
                  let idx = idx + dp_start - 1 in
                  let v = if idx < 0 then 0 else arr.(idx) in
                  v + 3 >= elm)
           |> List.fold_left ~f:( + ) ~init:0
         in
         dp.(dp_idx) <- sum);
  dp.(length)

let () =
  let input = read_input () in
  number_of_ways input |> string_of_int |> print_endline
