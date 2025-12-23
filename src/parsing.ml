open Interp_sig

let normalize_separators (s : string) : string =
  String.map
    (fun c ->
      match c with
      | ',' | ';' | '\t' -> ' '
      | _ -> c)
    s

let parse_point (line : string) : point option =
  let line = String.trim line in
  if line = "" then None
  else
    let line = normalize_separators line in
    let parts =
      line
      |> String.split_on_char ' '
      |> List.filter (fun s -> s <> "")
    in
    match parts with
    | [ xs; ys ] -> (
        try Some { x = float_of_string xs; y = float_of_string ys }
        with Failure _ -> None)
    | _ -> None

let parse_points (lines : string list) : point list =
  lines |> List.filter_map parse_point
