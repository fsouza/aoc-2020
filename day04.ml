open StdLabels

type height = Cm of int | In of int

let is_digit ch = ch >= '0' && ch <= '9'

let parse_height v =
  let input_len = String.length v in
  let digits = String.sub ~pos:0 ~len:(input_len - 2) v |> int_of_string_opt in
  let suffix = String.sub ~pos:(input_len - 2) ~len:2 v in
  digits
  |> Fun.flip Option.bind (fun digits ->
         match suffix with
         | "cm" -> Some (Cm digits)
         | "in" -> Some (In digits)
         | _ -> None)

let validate_height = function
  | Some (Cm x) -> x > 149 && x < 194
  | Some (In x) -> x > 58 && x < 77
  | _ -> false

let validate_hair_color c =
  if String.length c <> 7 then false
  else
    c.[0] == '#'
    && c |> String.to_seq |> List.of_seq |> List.tl
       |> List.for_all ~f:(fun ch -> is_digit ch || (ch >= 'a' && ch <= 'f'))

let validate_color color = false

type eye_color = Amb | Blu | Brn | Gry | Grn | Hzl | Oth

let parse_eye_color = function
  | "amb" -> Some Amb
  | "blu" -> Some Blu
  | "brn" -> Some Brn
  | "gry" -> Some Gry
  | "grn" -> Some Grn
  | "hzl" -> Some Hzl
  | "oth" -> Some Oth
  | _ -> None

type passport = {
  byr : int option;
  iyr : int option;
  eyr : int option;
  hgt : height option;
  hcl : string;
  ecl : eye_color option;
  pid : string;
  cid : string;
}

let empty_passport =
  {
    byr = None;
    iyr = None;
    eyr = None;
    hgt = None;
    hcl = "";
    ecl = None;
    pid = "";
    cid = "";
  }

let int_in_range ~min ~max = function
  | Some v -> v >= min && v <= max
  | None -> false

let validate_byr = int_in_range ~min:1920 ~max:2002

let validate_iyr = int_in_range ~min:2010 ~max:2020

let validate_eyr = int_in_range ~min:2020 ~max:2030

let validate_pid pid =
  String.length pid == 9
  && pid |> String.to_seq |> List.of_seq |> List.for_all ~f:is_digit

let is_valid { byr; iyr; eyr; hgt; hcl; ecl; pid; cid } =
  validate_byr byr && validate_iyr iyr && validate_eyr eyr
  && validate_height hgt && validate_hair_color hcl && Option.is_some ecl
  && validate_pid pid

let passport_with_field passport field_decl =
  match String.split_on_char ~sep:':' field_decl with
  | [ field_name; field_value ] -> (
      match field_name with
      | "byr" -> { passport with byr = int_of_string_opt field_value }
      | "iyr" -> { passport with iyr = int_of_string_opt field_value }
      | "eyr" -> { passport with eyr = int_of_string_opt field_value }
      | "hgt" -> { passport with hgt = parse_height field_value }
      | "hcl" -> { passport with hcl = field_value }
      | "ecl" -> { passport with ecl = parse_eye_color field_value }
      | "pid" -> { passport with pid = field_value }
      | "cid" -> { passport with cid = field_value }
      | _ -> passport)
  | _ -> passport

let parse_passport lines =
  let passport = ref empty_passport in
  lines
  |> List.map ~f:(String.split_on_char ~sep:' ')
  |> List.flatten
  |> List.iter ~f:(fun field -> passport := passport_with_field !passport field);
  if is_valid !passport then Some !passport else None

let read_input () =
  let rec read_input passports curr_lines =
    let add_passport () =
      match parse_passport curr_lines with
      | Some p -> read_input (p :: passports) []
      | None -> read_input passports []
    in
    try
      let line = read_line () in
      if line = "" then add_passport ()
      else read_input passports (line :: curr_lines)
    with End_of_file -> (
      match curr_lines with [] -> passports | _ -> add_passport ())
  in
  read_input [] []

let part2 () = read_input () |> List.length

let () = part2 () |> string_of_int |> print_endline
