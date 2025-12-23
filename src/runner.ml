open Interp_sig

type algo_def =
  | AlgoDef : {
      name : string;
      module_ : (module INTERP with type params = 'p);
      params : 'p;
    } -> algo_def

type packed_algo =
  | Algo : {
      name : string;
      step : float;
      params : 'p;
      module_ : (module INTERP with type state = 's and type params = 'p);
      state : 's;
    } -> packed_algo

let output_points (name : string) (pts : point list) : unit =
  List.iter
    (fun p -> Printf.printf "%s %.12g %.12g\n%!" name p.x p.y)
    pts

let process_point (p : point) (algo : packed_algo) : packed_algo =
  match algo with
  | Algo a ->
      let (module M) = a.module_ in
      let state', pts = M.step ~step:a.step ~params:a.params a.state p in
      output_points a.name pts;
      Algo { a with state = state' }

let flush_algo ~last_x (algo : packed_algo) : unit =
  match algo with
  | Algo a ->
      let (module M) = a.module_ in
      let pts = M.flush ~step:a.step ~params:a.params a.state ~last_x in
      output_points a.name pts

let pack ~step (defs : algo_def list) : packed_algo list =
  List.map
    (fun (AlgoDef d) ->
      let (module M) = d.module_ in
      Algo
        {
          name = d.name;
          step;
          params = d.params;
          module_ = (module M);
          state = M.empty;
        })
    defs

let run ~step ~(algs : algo_def list) ~(points : point list) =
  let packed0 = pack ~step algs in
  let packedN =
    List.fold_left
      (fun acc p -> List.map (process_point p) acc)
      packed0
      points
  in
  match List.rev points with
  | [] -> ()
  | last :: _ -> List.iter (flush_algo ~last_x:last.x) packedN
