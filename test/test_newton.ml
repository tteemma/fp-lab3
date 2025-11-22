open Interpolation

let approx a b =
  abs_float (a -. b) < 1e-6

let assert_point p (x, y) =
  if not (approx p.x x && approx p.y y) then
    Printf.printf "FAIL: got %.4f %.4f expected %.4f %.4f\n"
      p.x p.y x y

let () =
  let st = ref Newton.empty in

  let feed p =
    let st', out = Newton.step ~step:0.5 ~n:4 !st p in
    st := st';
    out
  in

  let _ = feed {x=0.; y=0.} in
  let _ = feed {x=1.; y=1.} in
  let _ = feed {x=2.; y=2.} in
  let out = feed {x=3.; y=3.} in

  match List.map (fun p -> (p.x, p.y)) out with
  | [(0.,0.); (0.5,0.5); (1.,1.); (1.5,1.5);
     (2.,2.); (2.5,2.5); (3.,3.)] -> ()
  | lst ->
      Printf.printf "FAIL: unexpected Newton output: ";
      List.iter (fun (x,y) -> Printf.printf "(%.1f %.1f) " x y) lst;
      print_newline ()
