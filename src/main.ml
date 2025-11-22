open Printf

open Interpolation

type algo =
  | use_linear
  | use_newton of int

type config = {
  step : float;
  algos : algo list;
}

let default_step = 1.0

let parse_args () =
  let step = ref default_step in
  let use_lin = ref false in
  let use_newt = ref None in
  let specs = [
    ("--step",
     Arg.Float (fun v -> step := v),
     "STEP  step size for output grid (default 1.0)");
    ("--linear",
     Arg.Unit (fun () -> use_lin := true),
     " enable piecewise-linear interpolation");
    ("--newton",
     Arg.Int (fun n -> use_newt := Some n),
     "N  enable Newton interpolation on N points");
  ] in
  let usage =
    "Usage: lab3 [--linear] [--newton N] [--step h]\n     Reads x y pairs from stdin and prints interpolated points."
  in
  Arg.parse specs (fun _ -> ()) usage;
  let algos =
    (if !use_lin then [ use_linear ] else []) @
    (match !use_newt with
     | None -> []
     | Some n -> [ use_newton n ])
  in
  if algos = [] then (
    eprintf "No algorithms selected. Use --linear and/or --newton N.\n";
    exit 1
  );
  if !step <= 0.0 then (
    eprintf "Step must be positive.\n";
    exit 1
  );
  { step = !step; algos }

let parse_point line =
  let open String in
  let line = trim line in
  if line = "" then
    None
  else
    let buf = Bytes.of_string line in
    for i = 0 to Bytes.length buf - 1 do
      match Bytes.get buf i with
      | ';' | ',' | '\t' -> Bytes.set buf i ' '
      | _ -> ()
    done;
    let s = Bytes.to_string buf in
    let parts =
      s
      |> String.split_on_char ' '
      |> List.filter (fun s -> s <> "")
    in
    match parts with
    | [sx; sy] ->
        (try
           let x = float_of_string sx in
           let y = float_of_string sy in
           Some { x; y }
         with Failure _ -> None)
    | _ -> None

let print_point prefix p =
  printf "%s: %.10g %.10g\n" prefix p.x p.y

let () =
  let cfg = parse_args () in
  let lin_state = ref Linear.empty in
  let newt_state = ref Newton.empty in
  let last_point = ref None in
  let use_lin =
    List.exists (function use_linear -> true | _ -> false) cfg.algos
  in
  let newton_n =
    List.fold_left
      (fun acc -> function
         | use_newton n -> Some n
         | _ -> acc)
      None cfg.algos
  in
  let use_newt = match newton_n with Some _ -> true | None -> false in
  try
    while true do
      let line = input_line stdin in
      match parse_point line with
      | None -> ()
      | Some p ->
          last_point := Some p;
          if use_lin then (
            let st, outs = Linear.step ~step:cfg.step !lin_state p in
            lin_state := st;
            List.iter (print_point "linear") outs
          );
          (match newton_n with
           | None -> ()
           | Some n ->
               let st, outs =
                 Newton.step ~step:cfg.step ~n !newt_state p
               in
               newt_state := st;
               List.iter (print_point "newton") outs);
          flush stdout
    done
  with End_of_file ->
    (match (!last_point, newton_n) with
     | Some p_last, Some n when use_newt ->
         let outs = Newton.flush ~step:cfg.step ~n !newt_state p_last.x in
         List.iter (print_point "newton") outs
     | _ -> ());
    flush stdout
