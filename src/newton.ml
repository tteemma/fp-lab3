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

let divided_differences (pts : point list) : float array =
  let m = List.length pts in
  let xs = Array.of_list (List.map (fun p -> p.x) pts) in
  let a = Array.of_list (List.map (fun p -> p.y) pts) in
  for j = 1 to m - 1 do
    for i = m - 1 downto j do
      let denom = xs.(i) -. xs.(i - j) in
      a.(i) <- (a.(i) -. a.(i - 1)) /. denom
    done
  done;
  a

let eval_newton (pts : point list) (coeffs : float array) (x : float) : float =
  let xs = Array.of_list (List.map (fun p -> p.x) pts) in
  let m = Array.length coeffs in
  let acc = ref coeffs.(m - 1) in
  for i = m - 2 downto 0 do
    acc := coeffs.(i) +. (x -. xs.(i)) *. !acc
  done;
  !acc

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
              let coeffs = divided_differences window in
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
              let coeffs = divided_differences window in
              let y = eval_newton window coeffs x in
              loop (x +. step) ({ x; y } :: acc)
        else List.rev acc
      in
      loop start_x []
