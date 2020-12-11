open StdLabels

let read_input () =
  let rec read_input acc =
    try
      let line = read_line () in
      read_input (line :: acc)
    with End_of_file ->
      acc
      |> List.rev
      |> List.map ~f:(fun line -> line |> String.to_seq |> Array.of_seq)
      |> Array.of_list
  in
  read_input []

let or_else f g v = f v || g v

let is_occupied = Char.equal '#'

let count_occupied seat_layout =
  Array.fold_left ~init:0
    ~f:(fun acc ->
      Array.fold_left ~init:acc ~f:(fun acc ch ->
          if is_occupied ch then acc + 1 else acc))
    seat_layout

let get_adjacent seat_layout i j =
  let rows = Array.length seat_layout in
  assert (rows > 0);
  let cols = Array.length seat_layout.(0) in
  let arr_get_opt arr (i, j) =
    if i < 0 then None
    else if j < 0 then None
    else if i >= rows then None
    else if j >= cols then None
    else Some seat_layout.(i).(j)
  in
  [
    (i - 1, j - 1);
    (i - 1, j);
    (i - 1, j + 1);
    (i, j - 1);
    (i, j + 1);
    (i + 1, j - 1);
    (i + 1, j);
    (i + 1, j + 1);
  ]
  |> List.filter_map ~f:(arr_get_opt seat_layout)

let handle_empty seat_layout shadow i j =
  let adjacent = get_adjacent shadow i j in
  if List.for_all ~f:(Fun.negate is_occupied) adjacent then (
    seat_layout.(i).(j) <- '#';
    true)
  else false

let handle_occupied seat_layout shadow i j =
  let adjacent = get_adjacent shadow i j in
  let occupied_adjacent = adjacent |> List.filter ~f:is_occupied in
  if List.length occupied_adjacent > 3 then (
    seat_layout.(i).(j) <- 'L';
    true)
  else false

let print_layout seat_layout =
  let rows =
    Array.map ~f:(fun arr -> Array.to_seq arr |> String.of_seq) seat_layout
  in
  Array.iter ~f:(Printf.printf "%s\n") rows;
  print_endline ""

let copy_layout = Array.map ~f:Array.copy

let run_transformations seat_layout =
  let rows = Array.length seat_layout in
  let cols = Array.length seat_layout.(0) in
  let shadow = ref @@ copy_layout seat_layout in
  let iter () =
    let layout_changed = ref false in
    for i = 0 to rows - 1 do
      for j = 0 to cols - 1 do
        let seat_changed =
          match !shadow.(i).(j) with
          | 'L' -> handle_empty seat_layout !shadow i j
          | '#' -> handle_occupied seat_layout !shadow i j
          | _ -> false
        in
        layout_changed := !layout_changed || seat_changed
      done
    done;
    !layout_changed
  in
  while iter () do
    shadow := copy_layout seat_layout
  done

let () =
  let input = read_input () in
  run_transformations input;
  count_occupied input |> string_of_int |> print_endline
