open StdLabels

type expr = Num of int | Sum of expr * expr | Prod of expr * expr

let rec eval = function
  | Num n -> n
  | Sum (x, y) -> eval x + eval y
  | Prod (x, y) -> eval x * eval y

type stack_elm = Expr of expr | Op_sum | Op_prod | Op_p

let is_digit ch = ch >= '0' && ch <= '9'

let to_int ch =
  let zero = Char.code '0' in
  Char.code ch - zero

let parse line =
  let length = String.length line in
  let rec parse' stack i =
    if i = length then
      match stack with
      | [ Expr e ] -> e
      | _ -> failwith "invalid expression"
    else
      match line.[i] with
      | '+' -> parse' (Op_sum :: stack) (i + 1)
      | '*' -> parse' (Op_prod :: stack) (i + 1)
      | '(' -> parse' (Op_p :: stack) (i + 1)
      | ')' -> handle_p stack i
      | ch when is_digit ch -> (
          match stack with
          | [] | Op_p :: _ -> parse' (Expr (Num (to_int ch)) :: stack) (i + 1)
          | Op_sum :: Expr e :: tl ->
              parse' (Expr (Sum (e, Num (to_int ch))) :: tl) (i + 1)
          | Op_prod :: Expr e :: tl ->
              parse' (Expr (Prod (e, Num (to_int ch))) :: tl) (i + 1)
          | _ -> failwith "malformed expression")
      | _ -> parse' stack (i + 1)
  and handle_p stack i =
    match stack with
    | Expr e2 :: Op_p :: Op_sum :: Expr e1 :: tl ->
        parse' (Expr (Sum (e1, e2)) :: tl) (i + 1)
    | Expr e2 :: Op_p :: Op_prod :: Expr e1 :: tl ->
        parse' (Expr (Prod (e1, e2)) :: tl) (i + 1)
    | Expr e :: Op_p :: tl -> parse' (Expr e :: tl) (i + 1)
    | _ -> failwith "malformed expression"
  in
  parse' [] 0

let read_input () =
  let rec read_input acc =
    try
      let line = read_line () in
      read_input (line :: acc)
    with End_of_file -> acc |> List.map ~f:parse
  in
  read_input []

let () =
  read_input ()
  |> List.map ~f:eval
  |> List.fold_left ~init:0 ~f:( + )
  |> string_of_int
  |> print_endline
