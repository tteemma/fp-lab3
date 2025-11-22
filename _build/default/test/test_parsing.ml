open Interpolation
open Interp_sig
open Parsing

let () =
  let p1 = parse_point "1 2" in
  let p2 = parse_point "3,4" in
  let p3 = parse_point "5;6" in
  let p4 = parse_point "7\t8" in

  match p1, p2, p3, p4 with
  | Some a, Some b, Some c, Some d ->
      assert (a.x = 1.0 && a.y = 2.0);
      assert (b.x = 3.0 && b.y = 4.0);
      assert (c.x = 5.0 && c.y = 6.0);
      assert (d.x = 7.0 && d.y = 8.0)
  | _ ->
      failwith "Parsing test failed"

let () = print_endline "Parsing test OK"
