open StdLabels
open MoreLabels

module Cycle : sig
  type 'a t

  val of_list : 'a list -> 'a t

  val remove_n : 'a t -> int -> 'a list

  val value : 'a t -> 'a

  val advance : 'a t -> unit

  val insert_in_cycle : 'a t -> ('a -> bool) -> 'a list -> unit

  val take_after : 'a t -> ('a -> bool) -> int -> 'a list
end = struct
  type 'a element = {
    value : 'a;
    mutable next : 'a element option;
    mutable prev : 'a element option;
  }

  type 'a dlist = {
    mutable hd : 'a element option;
    mutable tl : 'a element option;
  }

  type 'a t = { mutable current : 'a element }

  let make_element v = { value = v; next = None; prev = None }

  let insert dl v =
    let e = make_element v in
    match (dl.hd, dl.tl) with
    | None, None ->
        dl.hd <- Some e;
        dl.tl <- Some e
    | Some hd, Some tl ->
        tl.next <- Some e;
        e.prev <- Some tl;
        dl.tl <- Some e
    | _ -> failwith "inconsistent list"

  let connect dl =
    match (dl.hd, dl.tl) with
    | Some hd, Some tl ->
        hd.prev <- Some tl;
        tl.next <- Some hd;
        { current = hd }
    | _ -> failwith "invalid list, cannot connect"

  let of_list l =
    let dl = { hd = None; tl = None } in
    let rec of_list' = function
      | [] -> dl
      | hd :: tl ->
          insert dl hd;
          of_list' tl
    in
    of_list' l |> connect

  let remove_n c n =
    let rec remove_n' cursor acc n =
      if n = 0 then (cursor, acc |> List.rev)
      else remove_n' (Option.get cursor.next) (cursor.value :: acc) (n - 1)
    in
    let new_next, values = remove_n' (Option.get c.current.next) [] n in
    new_next.prev <- Some c.current;
    c.current.next <- Some new_next;
    values

  let value c = c.current.value

  (* this could loop forever if p never returns true *)
  let find_v p =
    let rec find_v' cursor =
      if p cursor.value then cursor else find_v' (Option.get cursor.next)
    in
    find_v'

  let insert_in_cycle c p l =
    let rec insert_after' cursor = function
      | [] -> ()
      | hd :: tl ->
          let e = make_element hd in
          let next = Option.get cursor.next in
          e.next <- Some next;
          next.prev <- Some e;
          cursor.next <- Some e;
          e.prev <- Some cursor;
          insert_after' e tl
    in
    insert_after' (find_v p c.current) l

  let advance c = c.current <- Option.get c.current.next

  let take_after c p n =
    let rec take' acc cursor n =
      if n = 0 then acc |> List.rev
      else take' (cursor.value :: acc) (Option.get cursor.next) (n - 1)
    in
    let v = find_v p c.current in
    take' [] (Option.get v.next) n
end

let int_of_digit ch = Char.code ch - Char.code '0'

let read_input () =
  let rec read_input acc =
    try
      let line = read_line () in
      read_input (line :: acc)
    with End_of_file ->
      acc
      |> List.hd
      |> String.to_seq
      |> Seq.map int_of_digit
      |> List.of_seq
      |> Cycle.of_list
  in
  read_input []

let index_of arr value =
  let length = Array.length arr in
  let rec index_of' i =
    if i = length then failwith "not found"
    else if arr.(i) = value then i
    else index_of' (i + 1)
  in
  index_of' 0

let move c =
  let vs = Cycle.remove_n c 3 in
  let rec find_destination destination =
    if destination = 0 then find_destination 9
    else if List.mem ~set:vs destination then find_destination (destination - 1)
    else destination
  in
  let destination = find_destination @@ (Cycle.value c - 1) in
  Cycle.insert_in_cycle c (Int.equal destination) vs;
  Cycle.advance c

let rec run_iterations n cycle =
  if n = 0 then ()
  else (
    move cycle;
    run_iterations (n - 1) cycle)

let () =
  let cycle = read_input () in
  run_iterations 100 cycle;
  Cycle.take_after cycle (Int.equal 1) 8
  |> List.map ~f:string_of_int
  |> String.concat ~sep:""
  |> print_endline
