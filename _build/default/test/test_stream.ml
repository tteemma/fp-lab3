let run_stream cmd input =
  let (ic, oc, ec) =
    Unix.open_process_full cmd (Unix.environment ())
  in

  output_string oc input;
  flush oc;
  close_out oc;

  let rec read acc =
    try read (input_line ic :: acc)
    with End_of_file -> List.rev acc
  in
  let result = read [] in

  ignore (Unix.close_process_full (ic, oc, ec));
  result

let () =
  let cmd = "dune exec src/main.exe -- --linear --step 1.0" in
  let output = run_stream cmd "0 0\n2 2\n" in

  let expected = [
    "linear: 0 0";
    "linear: 1 1";
    "linear: 2 2";
  ] in

  match output with
  | lst when lst = expected -> ()
  | _ ->
      Printf.printf "FAIL stream: got [%s]\n"
        (String.concat "; " output)
