open StdLabels

let parse_instruction line =
  match String.split_on_char ~sep:'=' line with
  | [ lhs; rhs ] -> Some (String.trim lhs, String.trim rhs)
  | _ -> None

let read_input () =
  let rec read_input n acc =
    try
      let line = read_line () in
      read_input (n + 1) (line :: acc)
    with End_of_file ->
      (n, acc |> List.rev |> List.filter_map ~f:parse_instruction)
  in
  read_input 0 []

let print_arr = Array.iter ~f:(Printf.printf "%d\n")

let make_floating_masks length floating =
  let floating = floating |> List.map ~f:(( - ) length) |> Array.of_list in
  let vs = Array.make (Array.length floating) 0 in
  let length = Array.length vs in
  let incr_vs () =
    (* increments the array and returns a boolean indicating whether its done *)
    let carry = ref 1 in
    for i = length - 1 downto 0 do
      let v = vs.(i) + !carry in
      vs.(i) <- v mod 2;
      carry := v / 2
    done;
    !carry = 1
  in
  let make_mask_from_vs () =
    (* this isn't quite right as it produces the mask from 000 twice, but for
     * this problem, it's not an issue, since it's just gonna write to the
     * memory position twice *)
    let mask = ref 0 in
    Array.iter2
      ~f:(fun pos v ->
        let v = if v = 0 then 1 else 0 in
        mask := (!mask lor v) lsl (pos - 1))
      floating vs;
    lnot !mask
  in
  let rec make_masks acc stop =
    if stop then acc else make_masks (make_mask_from_vs () :: acc) (incr_vs ())
  in
  make_masks [] false

let parse_mask mask =
  let length = String.length mask in
  let rec parse_mask' mask floating = function
    | [] -> (mask, floating |> List.rev |> make_floating_masks length)
    | (_, '0') :: tl -> parse_mask' (mask lsl 1) floating tl
    | (_, '1') :: tl -> parse_mask' ((mask lsl 1) lor 1) floating tl
    | (i, 'X') :: tl -> parse_mask' (mask lsl 1) (i :: floating) tl
    | _ -> invalid_arg "invalid mask"
  in
  parse_mask' 0 []
    (String.to_seq mask |> List.of_seq |> List.mapi ~f:(fun i c -> (i, c)))

let parse_pos pos =
  match String.split_on_char ~sep:'[' pos with
  | [ _; position ] -> (
      match String.split_on_char ~sep:']' position with
      | [ position; _ ] -> int_of_string position
      | _ -> invalid_arg "invalid position")
  | _ -> invalid_arg "invalid position"

let apply_mask (mask, floating_masks) pos =
  let masked = pos lor mask in
  floating_masks |> List.map ~f:(fun floating_mask -> masked land floating_mask)

let rec execute mem mask = function
  | [] -> ()
  | ("mask", m) :: tl -> execute mem (parse_mask m) tl
  | (pos, v) :: tl ->
      let pos = parse_pos pos in
      let v = int_of_string v in
      apply_mask mask pos
      |> List.iter ~f:(fun pos ->
             Printf.printf "%d\n" pos;
             Hashtbl.replace mem pos v);
      execute mem mask tl

let print_mem = Hashtbl.iter (Printf.printf "mem[%d] = %d\n")

let () =
  let n, instructions = read_input () in
  let mem = Hashtbl.create n in
  execute mem (0, []) instructions;
  print_mem mem;
  Hashtbl.fold (fun _ v acc -> acc + v) mem 0 |> string_of_int |> print_endline
