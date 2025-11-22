open Interp_sig

let parse_point line =
  let open String in
  let line = trim line in
  if line = "" then None
  else
    let buf = Bytes.of_string line in
    for i = 0 to Bytes.length buf - 1 do
      match Bytes.get buf i with
      | ';' | ',' | '\t' -> Bytes.set buf i ' '
      | _ -> ()
    done;
    let s = Bytes.to_string buf in
    let parts =
      s |> split_on_char ' ' |> List.filter (fun s -> s <> "")
    in
    match parts with
    | [sx; sy] ->
        (try
           let x = float_of_string sx in
           let y = float_of_string sy in
           Some { x; y }
         with _ -> None)
    | _ -> None
