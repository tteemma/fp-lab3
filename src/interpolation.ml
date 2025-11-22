type point = { x : float; y : float }

module Linear = struct
  type state = {
    prev : point option;
    next_x : float option;
  }

  let empty = { prev = None; next_x = None }

  let step ~step state p_new =
    match state.prev with
    | None ->
        (* first point, just remember it *)
        ({ prev = Some p_new; next_x = None }, [])
    | Some p_prev ->
        let state =
          match state.next_x with
          | None -> { state with next_x = Some p_prev.x }
          | Some _ -> state
        in
        let rec loop acc st =
          match st.next_x with
          | None -> (st, List.rev acc)
          | Some x ->
              if x > p_new.x then
                (st, List.rev acc)
              else
                let dx = p_new.x -. p_prev.x in
                let t =
                  if dx = 0.0 then 0.0
                  else (x -. p_prev.x) /. dx
                in
                let y =
                  p_prev.y +. t *. (p_new.y -. p_prev.y)
                in
                let acc' = { x; y } :: acc in
                let st' = { st with next_x = Some (x +. step) } in
                loop acc' st'
        in
        let st', outs = loop [] state in
        ({ st' with prev = Some p_new }, outs)

  let flush _state _last_x = (_state, [])
end

module Newton = struct
  type state = {
    points : point list;
    next_x : float option;
  }

  let empty = { points = []; next_x = None }

  let choose_window n points x =
    let arr = Array.of_list points in
    let k = Array.length arr in
    if k < n then
      None
    else
      let best_idx = ref None in
      let best_dist = ref infinity in
      for start = 0 to k - n do
        let p0 = arr.(start) in
        let plast = arr.(start + n - 1) in
        if x >= p0.x && x <= plast.x then (
          let mid_x = (p0.x +. plast.x) /. 2.0 in
          let dist = abs_float (x -. mid_x) in
          match !best_idx with
          | None ->
              best_idx := Some start;
              best_dist := dist
          | Some _ when dist < !best_dist ->
              best_idx := Some start;
              best_dist := dist
          | _ -> ()
        )
      done;
      match !best_idx with
      | None -> None
      | Some start ->
          let win = Array.init n (fun i -> arr.(start + i)) in
          Some win

  let newton_value pts x =
    let n = Array.length pts in
    if n = 0 then invalid_arg "newton_value: empty" ;
    let coeffs = Array.make n 0.0 in
    for i = 0 to n - 1 do
      coeffs.(i) <- pts.(i).y
    done;
    for j = 1 to n - 1 do
      for i = n - 1 downto j do
        let num = coeffs.(i) -. coeffs.(i - 1) in
        let den = pts.(i).x -. pts.(i - j).x in
        coeffs.(i) <- num /. den
      done
    done;
    let res = ref coeffs.(n - 1) in
    for i = n - 2 downto 0 do
      res := !res *. (x -. pts.(i).x) +. coeffs.(i)
    done;
    !res

  let step ~step ~n state p_new =
    let points' = state.points @ [ p_new ] in
    let next_x =
      match state.next_x with
      | Some x -> Some x
      | None ->
          if List.length points' >= n then
            let first = List.hd points' in
            Some first.x
          else
            None
    in
    let rec loop acc = function
      | None -> (None, List.rev acc)
      | Some x ->
          if x > p_new.x then
            (Some x, List.rev acc)
          else
            (match choose_window n points' x with
            | None ->
                (* cannot yet approximate this x, wait for more points *)
                (Some x, List.rev acc)
            | Some win ->
                let y = newton_value win x in
                let acc' = { x; y } :: acc in
                loop acc' (Some (x +. step)))
    in
    let next_x', outs =
      match next_x with
      | None -> (None, [])
      | Some x0 -> loop [] (Some x0)
    in
    ({ points = points'; next_x = next_x' }, outs)

  let flush ~step ~n state last_x =
    let rec loop acc = function
      | None -> List.rev acc
      | Some x ->
          if x > last_x then
            List.rev acc
          else
            (match choose_window n state.points x with
            | None -> List.rev acc
            | Some win ->
                let y = newton_value win x in
                let acc' = { x; y } :: acc in
                loop acc' (Some (x +. step)))
    in
    match state.next_x with
    | None -> []
    | Some x0 -> loop [] (Some x0)
end
