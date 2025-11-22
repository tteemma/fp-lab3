open Interp_sig

let approx ?(eps = 1e-6) a b = Float.abs (a -. b) <= eps

let assert_true msg b =
  if not b then failwith msg

let exists_point ?(eps=1e-6) ~x ~y (xs : point list) =
  List.exists (fun (p:point) -> approx ~eps p.x x && approx ~eps p.y y) xs

let collect_linear ~step (pts : point list) : point list =
  let rec go st acc = function
    | [] -> List.rev acc
    | p :: ps ->
        let st', out = Linear.step ~step ~params:() st p in
        go st' (List.rev_append out acc) ps
  in
  go Linear.empty [] pts

let test_one_point_no_output () =
  let out = collect_linear ~step:1.0 [ {x=0.; y=0.} ] in
  assert_true "one point should produce no output" (out = [])

let test_basic () =
  let out = collect_linear ~step:1.0 [ {x=0.;y=0.}; {x=2.;y=2.} ] in
  assert_true "should include (1,1)" (exists_point ~x:1. ~y:1. out)

let test_reverse_order () =
  let out = collect_linear ~step:1.0 [ {x=2.;y=2.}; {x=0.;y=0.} ] in
  assert_true "reverse should include (1,1)" (exists_point ~x:1. ~y:1. out)

let test_step_half () =
  let out = collect_linear ~step:0.5 [ {x=0.;y=0.}; {x=2.;y=2.} ] in
  assert_true "should include (0.5,0.5)" (exists_point ~x:0.5 ~y:0.5 out);
  assert_true "should include (1.0,1.0)" (exists_point ~x:1.0 ~y:1.0 out);
  assert_true "should include (1.5,1.5)" (exists_point ~x:1.5 ~y:1.5 out)

let test_non_integer_bounds () =
  let out = collect_linear ~step:1.0 [ {x=0.2;y=0.2}; {x=3.7;y=3.7} ] in
  (* start_grid_x gives 1.0; then 2.0; then 3.0 *)
  assert_true "should include x=1.0" (exists_point ~x:1.0 ~y:1.0 out);
  assert_true "should include x=2.0" (exists_point ~x:2.0 ~y:2.0 out);
  assert_true "should include x=3.0" (exists_point ~x:3.0 ~y:3.0 out);
  assert_true "should not include x=4.0" (not (List.exists (fun p -> approx p.x 4.0) out))

let test_multiple_segments () =
  let out =
    collect_linear ~step:1.0
      [ {x=0.;y=0.}; {x=2.;y=2.}; {x=4.;y=0.} ]
  in
  assert_true "segment1 includes (1,1)" (exists_point ~x:1.0 ~y:1.0 out);
  (* second segment between (2,2) and (4,0) includes x=3 => y=1 *)
  assert_true "segment2 includes (3,1)" (exists_point ~x:3.0 ~y:1.0 out)

let test_duplicate_x_ignored () =
  let out =
    collect_linear ~step:1.0
      [ {x=0.;y=0.}; {x=0.;y=10.}; {x=2.;y=2.} ]
  in
  (* duplicate x=0 is ignored; still should interpolate (1,1) between (0,?) and (2,2).
     But note: logic keeps prev updated to last point even on duplicate.
     Here prev becomes {0,10} after duplicate, so interpolation uses (0,10)->(2,2), at x=1 y=6. *)
  assert_true "should include (1,6) based on last duplicate point"
    (exists_point ~x:1.0 ~y:6.0 out)

let test_no_outside_points () =
  let out = collect_linear ~step:1.0 [ {x=0.;y=0.}; {x=1.2;y=1.2} ] in
  assert_true "should not generate x>=1.2" (List.for_all (fun p -> p.x < 1.2) out)

let () =
  test_one_point_no_output ();
  test_basic ();
  test_reverse_order ();
  test_step_half ();
  test_non_integer_bounds ();
  test_multiple_segments ();
  test_duplicate_x_ignored ();
  test_no_outside_points ();
  print_endline "Linear tests OK"
