[@@@warning "-32-33-34-37-69"]

open Printf
open Interpolation
open Interpolation.Interp_sig
open Interpolation.Runner
open Printf

type algo =
  | Use_linear
  | Use_newton of int

type config = {
  step : float;
  algos : algo list;
}

let default_step = 1.0

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

let parse_args () =
  let step = ref default_step in
  let use_lin = ref false in
  let use_newt = ref None in

  let specs = [
    ("--step", Arg.Float (fun v -> step := v), "STEP size");
    ("--linear", Arg.Unit (fun () -> use_lin := true), "Use linear interpolation");
    ("--newton", Arg.Int (fun n -> use_newt := Some n), "N points for Newton");
  ] in

  Arg.parse specs (fun _ -> ()) "lab3 args";

  let algos =
    (if !use_lin then [Use_linear] else [])
    @
    (match !use_newt with
     | None -> []
     | Some n -> [Use_newton n])
  in

  if algos = [] then (eprintf "No algorithms!\n"; exit 1);
  { step = !step; algos }

let build_algos cfg =
  List.map
    (function
      | Use_linear ->
          A ((module Linear), (), ref Linear.empty, "linear")

      | Use_newton n ->
          A ((module Newton), n, ref Newton.empty, "newton"))
    cfg.algos

let () =
  let cfg = parse_args () in
  let algos = build_algos cfg in
  let last_point = ref None in

  try
    while true do
      let line = input_line stdin in
      match Parsing.parse_point line with
      | None -> ()
      | Some p ->
          last_point := Some p;
          List.iter (process_point cfg.step p) algos;
          flush stdout
    done
  with End_of_file ->
    begin match !last_point with
    | None -> ()
    | Some p ->
        List.iter
          (flush_algo cfg.step p.x)
          algos
    end;
    flush stdout
