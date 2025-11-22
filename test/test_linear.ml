open Interpolation

let assert_point p (x, y) =
  if abs_float (p.x -. x) > 1e-6 || abs_float (p.y -. y) > 1e-6 then
    Printf.printf "FAIL: got %.4f %.4f expected %.4f %.4f\n"
      p.x p.y x y;
  ()

let () =
  let st = ref Linear.empty in

  let st1, out1 = Linear.step ~step:0.7 !st {x=0.; y=0.} in
  st := st1;
  assert (out1 = []);

  let st2, out2 = Linear.step ~step:0.7 !st {x=1.; y=1.} in
  st := st2;

  match out2 with
  | [p0; p1] ->
      assert_point p0 (0., 0.);
      assert_point p1 (0.7, 0.7)
  | _ ->
      Printf.printf "FAIL: unexpected output length=%d\n" (List.length out2)
