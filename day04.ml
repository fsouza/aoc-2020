open StdLabels

type passport = {
  byr : string;
  iyr : string;
  eyr : string;
  hgt : string;
  hcl : string;
  ecl : string;
  pid : string;
  cid : string;
}

let empty_passport =
  {
    byr = "";
    iyr = "";
    eyr = "";
    hgt = "";
    hcl = "";
    ecl = "";
    pid = "";
    cid = "";
  }

let is_valid { byr; iyr; eyr; hgt; hcl; ecl; pid; cid } =
  byr <> "" && iyr <> "" && eyr <> "" && hgt <> "" && hcl <> "" && ecl <> ""
  && pid <> ""

let passport_with_field passport field_decl =
  match String.split_on_char ~sep:':' field_decl with
  | [ field_name; field_value ] -> (
      match field_name with
      | "byr" -> { passport with byr = field_value }
      | "iyr" -> { passport with iyr = field_value }
      | "eyr" -> { passport with eyr = field_value }
      | "hgt" -> { passport with hgt = field_value }
      | "hcl" -> { passport with hcl = field_value }
      | "ecl" -> { passport with ecl = field_value }
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

let part1 () = read_input () |> List.length

let () = part1 () |> string_of_int |> print_endline
