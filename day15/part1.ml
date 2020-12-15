open StdLabels

let read_input () =
  let rec read_input acc =
    try
      let line = read_line () in
      read_input (line :: acc)
    with End_of_file -> acc |> List.hd |> String.split_on_char ~sep:','
  in
  read_input []

let run until (input : string list) : string =
  let turn_index = Hashtbl.create 2020 in
  let rec process_input turn = function
    | [] -> failwith "invalid input"
    | [ hd ] ->
        Hashtbl.replace turn_index hd turn;
        (turn + 1, "0")
    | hd :: tl ->
        Hashtbl.replace turn_index hd turn;
        process_input (turn + 1) tl
  in
  let rec run' turn next =
    if turn = until then next
    else
      let v = next in
      let next =
        match Hashtbl.find_opt turn_index v with
        | None -> "0"
        | Some t -> turn - t |> string_of_int
      in
      Hashtbl.replace turn_index v turn;
      run' (turn + 1) next
  in
  let turn, next = process_input 1 input in
  run' turn next

let () = read_input () |> run 2020 |> print_endline
