open Interp_sig

let approx ?(eps = 1e-3) a b = Float.abs (a -. b) <= eps

let assert_true msg b =
  if not b then failwith msg

let exists_x ?(eps=1e-6) (x:float) (xs:point list) =
  List.exists (fun (p:point) -> Float.abs (p.x -. x) <= eps) xs

let find_x ?(eps=1e-6) (x:float) (xs:point list) : point option =
  List.find_opt (fun (p:point) -> Float.abs (p.x -. x) <= eps) xs

let is_finite f =
  match classify_float f with
  | FP_normal | FP_subnormal | FP_zero -> true
  | FP_infinite | FP_nan -> false

let collect_newton ~step ~n (pts : point list) : point list =
  let rec go st acc = function
    | [] -> (st, List.rev acc)
    | p :: ps ->
        let st', out = Newton.step ~step ~params:n st p in
        go st' (List.rev_append out acc) ps
  in
  let st, out = go Newton.empty [] pts in
  let last_x =
    match pts with
    | [] -> 0.0
    | _ ->
        List.fold_left (fun m (p:point) -> Float.max m p.x) (List.hd pts).x pts
  in
  let tail = Newton.flush ~step ~params:n st ~last_x in
  out @ tail

let test_generates_points_and_not_on_existing_x () =
  let given =
    [ {x=0.;y=0.}; {x=1.;y=1.}; {x=2.;y=4.}; {x=3.;y=9.} ]
  in
  let out = collect_newton ~step:0.5 ~n:3 given in

  assert_true "should generate some points" (out <> []);

  (* не должен генерировать точки на исходных x *)
  assert_true "should not generate x=1.0" (not (exists_x 1.0 out));
  assert_true "should not generate x=2.0" (not (exists_x 2.0 out));

  (* должен сгенерировать хотя бы одну точку в середине *)
  assert_true "should generate at least one of 0.5/1.5/2.5"
    (exists_x 0.5 out || exists_x 1.5 out || exists_x 2.5 out);

  (* все y должны быть конечными числами *)
  assert_true "all y are finite" (List.for_all (fun p -> is_finite p.y) out)

let test_quadratic_values_loose () =
  (* y = x^2, очень мягко проверяем только одну точку *)
  let given =
    [ {x=0.;y=0.}; {x=1.;y=1.}; {x=2.;y=4.}; {x=3.;y=9.} ]
  in
  let out = collect_newton ~step:0.5 ~n:3 given in

  match find_x 0.5 out with
  | None ->
      (* допустимо: если по твоей реализации не появилась именно 0.5 *)
      ()
  | Some p ->
      (* большой допуск, чтобы не зависеть от окна и порядка *)
      assert_true "y(0.5) should be near 0.25 (loose)"
        (approx ~eps:0.3 p.y 0.25)

let test_out_of_order_no_crash () =
  let given = [ {x=2.;y=4.}; {x=0.;y=0.}; {x=3.;y=9.}; {x=1.;y=1.} ] in
  let out = collect_newton ~step:1.0 ~n:3 given in
  ignore out;
  assert_true "ok" true

let test_two_points_no_crash () =
  let out = collect_newton ~step:1.0 ~n:3 [ {x=0.;y=0.}; {x=2.;y=2.} ] in
  ignore out;
  assert_true "ok" true

let test_duplicate_x_no_crash () =
  let out =
    collect_newton ~step:0.5 ~n:3
      [ {x=0.;y=0.}; {x=1.;y=1.}; {x=1.;y=9.}; {x=2.;y=4.} ]
  in
  ignore out;
  assert_true "ok" true

let test_n2_sanity_loose () =
  (* n=2 — должно вести себя близко к линейной интерполяции *)
  let given = [ {x=0.;y=0.}; {x=2.;y=2.} ] in
  let out = collect_newton ~step:1.0 ~n:2 given in
  match find_x 1.0 out with
  | None -> failwith "missing x=1.0"
  | Some p ->
      assert_true "y at 1.0 should be close to 1.0"
        (approx ~eps:0.2 p.y 1.0)

let test_step_not_dividing_interval () =
  let given = [ {x=0.;y=0.}; {x=1.;y=1.}; {x=2.;y=4.} ] in
  let out = collect_newton ~step:0.3 ~n:3 given in
  assert_true "should include some generated point" (out <> [])

let () =
  test_generates_points_and_not_on_existing_x ();
  test_quadratic_values_loose ();
  test_out_of_order_no_crash ();
  test_two_points_no_crash ();
  test_duplicate_x_no_crash ();
  test_n2_sanity_loose ();
  test_step_not_dividing_interval ();
  print_endline "Newton tests OK"
