open Interp_sig

type params = int

type state = {
  points : point list;
  next_x : float option;
}

let empty = { points = []; next_x = None }

let rec insert_sorted_unique (p : point) (xs : point list) : point list =
  match xs with
  | [] -> [ p ]
  | h :: t ->
      if Float_util.eq p.x h.x then
        p :: t
      else if Float_util.lt p.x h.x then
        p :: xs
      else
        h :: insert_sorted_unique p t

let has_x x xs =
  List.exists (fun (p : point) -> Float_util.eq p.x x) xs

let take n xs =
  let rec go i acc = function
    | [] -> List.rev acc
    | _ when i = 0 -> List.rev acc
    | h :: t -> go (i - 1) (h :: acc) t
  in
  go n [] xs

let pick_window ~(n:int) ~(x:float) (xs:point list) : point list =
  let with_d =
    List.map (fun p -> (Float.abs (p.x -. x), p)) xs
    |> List.sort (fun (d1, _) (d2, _) -> Float.compare d1 d2)
  in
  let chosen = take n with_d |> List.map snd in
  List.sort (fun a b -> Float.compare a.x b.x) chosen

let drop n xs =
  let rec go i ys =
    if i <= 0 then ys
    else
      match ys with
      | [] -> []
      | _ :: t -> go (i - 1) t
  in
  go n xs

let next_level_k (k:int) (xs:float list) (ys:float list) : float list =
  let rec go xs0 xsk ys0 =
    match xs0, xsk, ys0 with
    | x0 :: xs0', xk :: xsk', y0 :: y1 :: ys_tail ->
        ((y1 -. y0) /. (xk -. x0)) :: go xs0' xsk' (y1 :: ys_tail)
    | _ -> []
  in
  go xs (drop k xs) ys

let newton_coeffs (pts : point list) : float list =
  let xs = List.map (fun p -> p.x) pts in
  let ys0 = List.map (fun p -> p.y) pts in
  let rec go k ys acc =
    match ys with
    | [] -> List.rev acc
    | a :: _ ->
        let acc' = a :: acc in
        let ys' = next_level_k (k + 1) xs ys in
        go (k + 1) ys' acc'
  in
  go 0 ys0 []

let eval_newton (pts : point list) (coeffs : float list) (x : float) : float =
  let xs = List.map (fun p -> p.x) pts in
  let rev_xs = List.rev xs in
  let rev_cs = List.rev coeffs in
  let rec fold xs cs acc =
    match xs, cs with
    | x_i :: xs', a_i :: cs' -> fold xs' cs' (a_i +. (x -. x_i) *. acc)
    | _ -> acc
  in
  match rev_cs, rev_xs with
  | [], _ -> nan
  | _, [] -> nan
  | a_last :: rest, _x_last :: xs_tail_rev ->
      fold xs_tail_rev rest a_last

let gen_points ~step ~n ~(state:state) : point list =
  let pts = state.points in
  match pts with
  | [] | [ _ ] -> []
  | _ ->
      let min_x = (List.hd pts).x in
      let max_x = (List.hd (List.rev pts)).x in
      let start_x =
        match state.next_x with
        | Some nx -> nx
        | None -> Grid_util.start_grid_x ~step min_x
      in
      let rec loop x acc =
        if Float_util.lt x max_x then
          if has_x x pts then
            loop (x +. step) acc
          else
            let window = pick_window ~n ~x pts in
            if List.length window < 2 then
              loop (x +. step) acc
            else
              let coeffs = newton_coeffs window in
              let y = eval_newton window coeffs x in
              loop (x +. step) ({ x; y } :: acc)
        else
          List.rev acc
      in
      loop start_x []

let step ~step ~params:n st p =
  let points = insert_sorted_unique p st.points in
  let st' = { st with points } in
  let out = gen_points ~step ~n ~state:st' in
  let next_x =
    if out = [] then st'.next_x
    else Some ((List.hd (List.rev out)).x +. step)
  in
  ({ st' with next_x }, out)

let flush ~step ~params:n st ~last_x =
  let pts = st.points in
  match pts with
  | [] | [ _ ] -> []
  | _ ->
      let min_x = (List.hd pts).x in
      let start_x =
        match st.next_x with
        | Some nx -> nx
        | None -> Grid_util.start_grid_x ~step min_x
      in
      let rec loop x acc =
        if Float_util.le x last_x then
          if has_x x pts then
            loop (x +. step) acc
          else
            let window = pick_window ~n ~x pts in
            if List.length window < 2 then
              loop (x +. step) acc
            else
              let coeffs = newton_coeffs window in
              let y = eval_newton window coeffs x in
              loop (x +. step) ({ x; y } :: acc)
        else
          List.rev acc
      in
      loop start_x []
