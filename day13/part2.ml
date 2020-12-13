open StdLabels

let read_input () =
  let rec read_input acc =
    try
      let line = read_line () in
      read_input (line :: acc)
    with End_of_file -> acc |> List.rev
  in
  read_input []

let parse_input = function
  | [ first_line; second_line ] ->
      ( int_of_string first_line,
        String.split_on_char ~sep:',' second_line
        |> List.mapi ~f:(fun i bus -> (i, bus))
        |> List.filter_map ~f:(fun (i, bus) ->
               int_of_string_opt bus |> Option.map (fun bus_id -> (i, bus_id)))
      )
  | _ -> failwith "invalid input"

let wait_time n m =
  let diff = n mod m in
  if diff = 0 then diff else m - diff

(* this was the original brute force implementation *)
let rec look_for_answer offset_buses =
  let delta =
    match offset_buses with
    | (_, bus) :: _ -> bus
    | _ -> 1
  in
  let rec find_answer t = function
    | [] -> t
    | (offset, bus) :: tl when (t + offset) mod bus <> 0 ->
        find_answer (t + delta) offset_buses
    | _ :: tl -> find_answer t tl
  in
  find_answer delta offset_buses

(* this is a better solution, "inspired" by @lizthegrey's solution :D
 *
 * This is somewhat similar to the brute-force solution, but instead of always
 * adding the initial offset to the candidate ts, it keeps track of a minimal
 * value that keeps the timestamp divisible by all previous timestamps and the
 * current one (taking the offset into consideration).
 *
 * *)
let rec find_answer min_val running_product = function
  | [] -> min_val
  | (offset, bus) :: tl when (min_val + offset) mod bus = 0 ->
      find_answer min_val (running_product * bus) tl
  | l -> find_answer (min_val + running_product) running_product l

let () =
  let input = read_input () in
  let _, offset_buses = parse_input input in
  find_answer 0 1 offset_buses |> string_of_int |> print_endline
