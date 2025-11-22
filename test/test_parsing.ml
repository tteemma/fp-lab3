open Interp_sig
open Parsing

let assert_true msg b =
  if not b then failwith msg

let assert_none msg = function
  | None -> ()
  | Some _ -> failwith msg

let assert_some msg = function
  | None -> failwith msg
  | Some x -> x

let assert_float ?(eps=1e-9) msg a b =
  if Float.abs (a -. b) > eps then failwith msg

let test_basic_separators () =
  let p1 = assert_some "comma failed" (parse_point "1,2") in
  assert_float "x comma" p1.x 1.0;
  assert_float "y comma" p1.y 2.0;

  let p2 = assert_some "semicolon failed" (parse_point "3;4") in
  assert_float "x semicolon" p2.x 3.0;
  assert_float "y semicolon" p2.y 4.0

let test_spaces_and_trimming () =
  let p = assert_some "spaces failed" (parse_point "   10   20  ") in
  assert_float "x spaces" p.x 10.0;
  assert_float "y spaces" p.y 20.0

let test_negative_and_float () =
  let p = assert_some "negative/float failed" (parse_point "-1.5 2.25") in
  assert_float "x neg float" p.x (-1.5);
  assert_float "y neg float" p.y 2.25

let test_scientific_notation () =
  let p = assert_some "scientific failed" (parse_point "1e1 2e-1") in
  assert_float "x sci" p.x 10.0;
  assert_float "y sci" p.y 0.2

let test_invalid_lines () =
  assert_none "empty should be None" (parse_point "");
  assert_none "spaces should be None" (parse_point "   ");
  assert_none "one number should be None" (parse_point "1");
  assert_none "three numbers should be None" (parse_point "1 2 3");
  assert_none "nonsense should be None" (parse_point "abc def")

let test_parse_points_filters () =
  let lines = [ ""; "1 2"; "bad"; "3,4"; "  5;6  " ] in
  let pts = parse_points lines in
  assert_true "should parse 3 points" (List.length pts = 3);
  let a = List.nth pts 0 in
  let b = List.nth pts 1 in
  let c = List.nth pts 2 in
  assert_float "a.x" a.x 1.0; assert_float "a.y" a.y 2.0;
  assert_float "b.x" b.x 3.0; assert_float "b.y" b.y 4.0;
  assert_float "c.x" c.x 5.0; assert_float "c.y" c.y 6.0

let test_multiple_spaces_and_tabs () =
  let p = assert_some "tabs/spaces failed" (parse_point "  1\t   2  ") in
  assert_float "x tab" p.x 1.0;
  assert_float "y tab" p.y 2.0

let () =
  test_basic_separators ();
  test_spaces_and_trimming ();
  test_negative_and_float ();
  test_scientific_notation ();
  test_invalid_lines ();
  test_parse_points_filters ();
  test_multiple_spaces_and_tabs ();
  print_endline "Parsing tests OK"
