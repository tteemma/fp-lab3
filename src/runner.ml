open Interp_sig

type packed_algo =
  | A :
      (module INTERP with type state = 's and type params = 'p)
      * 'p
      * 's ref
      * string
      -> packed_algo

let process_point step p (A ((module M), params, st_ref, name)) =
  let st', outs =
    M.step ~step ~params !st_ref p
  in
  st_ref := st';
  List.iter
    (fun {x; y} ->
      Printf.printf "%s: %.10g %.10g\n" name x y
    )
    outs

let flush_algo step last_x (A ((module M), params, st_ref, name)) =
  let outs =
    M.flush ~step ~params !st_ref ~last_x
  in
  List.iter
    (fun {x; y} ->
      Printf.printf "%s: %.10g %.10g\n" name x y
    )
    outs
