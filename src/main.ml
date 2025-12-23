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

let rec parse_cli (args : string list) (step : float) (algos : algo list) :
    config =
  match args with
  | [] ->
      let algos =
        match List.rev algos with
        | [] -> [ Linear ]
        | xs -> xs
      in
      { step; algos }
  | "--step" :: s :: rest ->
      let step' =
        match float_of_string_opt s with
        | Some v -> v
        | None -> step
      in
      parse_cli rest step' algos
  | "--linear" :: rest ->
      parse_cli rest step (Linear :: algos)
  | "--newton" :: n :: rest ->
      let algos' =
        match int_of_string_opt n with
        | Some k -> Newton k :: algos
        | None -> algos
      in
      parse_cli rest step algos'
  | _unknown :: rest ->
      parse_cli rest step algos

let parse_args () : config =
  match Array.to_list Sys.argv with
  | [] -> { step = default_step; algos = [ Linear ] }
  | _prog :: args -> parse_cli args default_step []

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


let rec read_loop (packed : Runner.packed_algo list) (max_x : float option) :
    Runner.packed_algo list * float option =
  try
    let line = read_line () in
    match Parsing.parse_point line with
    | None -> read_loop packed max_x
    | Some p ->
        let packed' = List.map (Runner.process_point p) packed in
        let max_x' =
          match max_x with
          | None -> Some p.x
          | Some mx -> Some (Float.max mx p.x)
        in
        read_loop packed' max_x'
  with End_of_file ->
    (packed, max_x)

let () =
  let cfg = parse_args () in
  let alg_defs = build_algos cfg in
  let packed0 = Runner.pack ~step:cfg.step alg_defs in
  let packedN, max_x = read_loop packed0 None in
  match max_x with
  | None -> ()
  | Some mx ->
      List.iter (Runner.flush_algo ~last_x:mx) packedN
