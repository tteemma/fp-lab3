open Interp_sig

let parse_point line =
  let open String in
  let line = trim line in
  if line = "" then None
  else
    let buf = Bytes.of_string line in
    for i = 0 to Bytes.length buf - 1 do
      match Bytes.get buf i with
      | ',' | ';' | '\t' -> Bytes.set buf i ' '
      | _ -> ()
    done;
    let parts =
      Bytes.to_string buf
      |> split_on_char ' '
      |> List.filter (fun s -> s <> "")
    in
    match parts with
    | [ xs; ys ] -> (
        try
          Some { x = float_of_string xs; y = float_of_string ys }
        with Failure _ ->
          None
      )
    | _ -> None

let parse_points lines =
  lines |> List.filter_map parse_point
