open StdLabels
open MoreLabels

module Ring : sig
  type 'a t

  val of_list : 'a list -> 'a t

  val remove_n : 'a t -> int -> 'a list

  val value : 'a t -> 'a

  val advance : 'a t -> unit

  val insert_in_cycle : 'a t -> 'a -> 'a list -> unit

  val take_after : 'a t -> 'a -> int -> 'a list

  val add_seq : 'a t -> 'a Seq.t -> unit
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

  type 'a t = {
    mutable current : 'a element;
    rev_index : ('a, 'a element) Hashtbl.t;
  }

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

  let make_rev_index_from_dlist cursor =
    let rev_index = Hashtbl.create 1 in
    let rec collect' = function
      | None -> ()
      | Some v ->
          Hashtbl.replace ~key:v.value ~data:v rev_index;
          collect' v.next
    in
    collect' cursor;
    rev_index

  let connect dl =
    match (dl.hd, dl.tl) with
    | Some hd, Some tl ->
        let rev_index = make_rev_index_from_dlist (Some hd) in
        hd.prev <- Some tl;
        tl.next <- Some hd;
        { current = hd; rev_index }
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

  let create_element_after cursor v =
    let e = make_element v in
    let next = Option.get cursor.next in
    e.next <- Some next;
    next.prev <- Some e;
    cursor.next <- Some e;
    e.prev <- Some cursor;
    e

  let insert_in_cycle c v l =
    let rec insert_after' cursor = function
      | [] -> ()
      | hd :: tl ->
          let e = create_element_after cursor hd in
          Hashtbl.replace ~key:hd ~data:e c.rev_index;
          insert_after' e tl
    in
    insert_after' (Hashtbl.find c.rev_index v) l

  let advance c = c.current <- Option.get c.current.next

  let take_after c v n =
    let rec take' acc cursor n =
      if n = 0 then acc |> List.rev
      else take' (cursor.value :: acc) (Option.get cursor.next) (n - 1)
    in
    let e = Hashtbl.find c.rev_index v in
    take' [] (Option.get e.next) n

  let add_seq c seq =
    let last = Option.get c.current.prev in
    let _ =
      seq
      |> Seq.fold_left
           (fun cursor v ->
             let e = create_element_after cursor v in
             Hashtbl.replace ~key:v ~data:e c.rev_index;
             e)
           last
    in
    ()
end

let int_seq last =
  Seq.unfold (fun n -> if n > last then None else Some (n, n + 1))

let int_of_digit ch = Char.code ch - Char.code '0'

let read_input () =
  let rec read_input acc =
    try
      let line = read_line () in
      read_input (line :: acc)
    with End_of_file ->
      acc |> List.hd |> String.to_seq |> Seq.map int_of_digit |> List.of_seq
  in
  read_input []

let largest = 1_000_000

let move c =
  let vs = Ring.remove_n c 3 in
  let rec find_destination destination =
    if destination = 0 then find_destination largest
    else if List.mem ~set:vs destination then find_destination (destination - 1)
    else destination
  in
  let destination = find_destination @@ (Ring.value c - 1) in
  Ring.insert_in_cycle c destination vs;
  Ring.advance c

let rec run_iterations n cycle =
  if n = 0 then ()
  else (
    move cycle;
    run_iterations (n - 1) cycle)

let () =
  let seq = int_seq largest in
  let input = read_input () in
  let cycle = Ring.of_list input in
  Ring.add_seq cycle (seq 10);
  run_iterations 10_000_000 cycle;
  Ring.take_after cycle 1 2
  |> List.fold_left ~init:1 ~f:( * )
  |> Printf.printf "%d\n"
