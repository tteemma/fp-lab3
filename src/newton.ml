open Interp_sig

type params = int

type state = {
  points : point list;
  next_x : float option;
}

let empty = { points = []; next_x = None }

let choose_window n points x =
  let arr = Array.of_list points in
  let k = Array.length arr in
  if k < n then None
  else
    let best_i = ref None in
    let best_d = ref infinity in
    for i = 0 to k - n do
      let p0 = arr.(i) in
      let p1 = arr.(i + n - 1) in
      if x >= p0.x && x <= p1.x then
        let mid = (p0.x +. p1.x) /. 2.0 in
        let d = abs_float (x -. mid) in
        if d < !best_d then (
          best_d := d;
          best_i := Some i
        )
    done;
    match !best_i with
    | None -> None
    | Some i ->
        Some (Array.init n (fun j -> arr.(i + j)))

let newton_value pts x =
  let n = Array.length pts in
  let c = Array.make n 0.0 in
  for i = 0 to n - 1 do
    c.(i) <- pts.(i).y
  done;
  for j = 1 to n - 1 do
    for i = n - 1 downto j do
      c.(i) <-
        (c.(i) -. c.(i - 1))
        /. (pts.(i).x -. pts.(i - j).x)
    done
  done;
  let acc = ref c.(n - 1) in
  for i = n - 2 downto 0 do
    acc := !acc *. (x -. pts.(i).x) +. c.(i)
  done;
  !acc

let step ~step ~params:n state p_new =
  let points' = state.points @ [p_new] in

  let next_x =
    match state.next_x with
    | Some x -> Some x
    | None ->
        if List.length points' >= n
        then Some (List.hd points').x
        else None
  in

  let rec loop acc x =
    match x with
    | None -> (None, List.rev acc)
    | Some xx ->
        if xx > p_new.x then (Some xx, List.rev acc)
        else
          match choose_window n points' xx with
          | None -> (Some xx, List.rev acc)
          | Some win ->
              let y = newton_value win xx in
              loop ({ x = xx; y } :: acc) (Some (xx +. step))
  in

  let next_x', outs =
    match next_x with
    | None -> (None, [])
    | Some x0 -> loop [] (Some x0)
  in

  ({ points = points'; next_x = next_x' }, outs)

let flush ~step ~params:n state ~last_x =
  match state.next_x with
  | None -> []
  | Some x0 ->
      let rec loop acc x =
        if x > last_x then List.rev acc
        else
          match choose_window n state.points x with
          | None -> List.rev acc
          | Some win ->
              let y = newton_value win x in
              loop ({ x; y } :: acc) (x +. step)
      in
      loop [] x0
