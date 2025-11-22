open Interpolation

let test str expected =
  match Interpolation.parse_point str with
  | None -> Printf.printf "FAIL parse '%s'\n" str
  | Some p ->
      if abs_float (p.x -. fst expected) > 1e-6 ||
         abs_float (p.y -. snd expected) > 1e-6 then
        Printf.printf "FAIL parse '%s' -> %.2f %.2f\n"
          str p.x p.y

let () =
  test "1 2" (1., 2.);
  test "1,2" (1., 2.);
  test "1; 2" (1., 2.);
  test "1\t2" (1., 2.);
  test "   5.3   9.1   " (5.3, 9.1)
