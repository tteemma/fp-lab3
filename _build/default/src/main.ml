[@@@warning "-32-33-34-37-69"]

open Interp_sig

type algo =
  | Linear
  | Newton of int

type config = {
  step : float;
  algos : algo list;
}

let default_step = 1.0

let parse_args () =
  let step = ref default_step in
  let algos = ref [] in
  let specs =
    [
      ("--step", Arg.Set_float step, "Grid step (float)");
      ("--linear", Arg.Unit (fun () -> algos := Linear :: !algos), "Enable linear");
      ( "--newton",
        Arg.Int (fun n -> algos := Newton n :: !algos),
        "Enable Newton interpolation with window size N" );
    ]
  in
  let usage =
    "Usage: main [--step S] [--linear] [--newton N]\n\
     Reads points from stdin: \"x y\" per line.\n\
     Prints generated points immediately."
  in
  Arg.parse specs (fun _ -> ()) usage;

  let algos =
    match List.rev !algos with
    | [] -> [ Linear ]
    | xs -> xs
  in
  { step = !step; algos }

let build_algos (cfg : config) : Runner.algo_def list =
  List.map
    (function
      | Linear ->
          Runner.AlgoDef
            { name = "linear"; module_ = (module Linear); params = () }
      | Newton n ->
          Runner.AlgoDef
            { name = "newton"; module_ = (module Newton); params = n })
    cfg.algos

let () =
  let cfg = parse_args () in
  let alg_defs = build_algos cfg in

  let packed = ref (Runner.pack ~step:cfg.step alg_defs) in

  let last_x = ref None in

  (try
     while true do
       let line = read_line () in
       match Parsing.parse_point line with
       | None -> ()
       | Some p ->
           last_x := Some p.x;
           packed := List.map (Runner.process_point p) !packed
     done
   with End_of_file -> ());

  match !last_x with
  | None -> ()
  | Some lx -> List.iter (Runner.flush_algo ~last_x:lx) !packed
