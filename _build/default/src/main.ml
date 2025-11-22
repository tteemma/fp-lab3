open Printf

open Interpolation

type algo =
  | Use_linear
  | Use_newton of int

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

let () =
  let cfg = parse_args () in

  let lin_state = ref Linear.empty in
  let newt_state = ref Newton.empty in
  let last_point = ref None in

  let use_lin =
    List.exists (function Use_linear -> true | _ -> false) cfg.algos
  in

  let newton_n =
    List.fold_left
      (fun acc -> function Use_newton n -> Some n | _ -> acc)
      None cfg.algos
  in

  try
    while true do
      let line = input_line stdin in
      match parse_point line with
      | None -> ()
      | Some p ->
          last_point := Some p;

          if use_lin then
            let st, outs =
              Linear.step ~step:cfg.step !lin_state p
            in
            lin_state := st;
            List.iter (fun p -> printf "linear: %.10g %.10g\n" p.x p.y) outs;

          begin match newton_n with
          | None -> ()
          | Some n ->
              let st, outs =
                Newton.step ~step:cfg.step ~n !newt_state p
              in
              newt_state := st;
              List.iter (fun p -> printf "newton: %.10g %.10g\n" p.x p.y) outs
          end;

          flush stdout
    done
  with End_of_file ->
    begin match !last_point, newton_n with
    | Some p_last, Some n ->
        let outs = Newton.flush ~step:cfg.step ~n !newt_state p_last.x in
        List.iter (fun p -> printf "newton: %.10g %.10g\n" p.x p.y) outs
    | _ -> ()
    end;
    flush stdout
