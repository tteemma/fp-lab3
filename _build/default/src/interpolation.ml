type point = { x : float; y : float }

(* ---------------- Linear interpolation ---------------- *)

module Linear = struct
  type state = {
    prev : point option;
    next_x : float option;
  }

  let empty = { prev = None; next_x = None }

  let step ~step state p_new =
    match state.prev with
    | None ->
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

  let flush _ _ = []
end

(* ---------------- Newton interpolation ---------------- *)

module Newton = struct
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
      let best_idx = ref None in
      let best_dist = ref infinity in
      for start = 0 to k - n do
        let p0 = arr.(start) in
        let p1 = arr.(start + n - 1) in
        if x >= p0.x && x <= p1.x then
          let mid = (p0.x +. p1.x) /. 2.0 in
          let d = abs_float (x -. mid) in
          match !best_idx with
          | None -> best_idx := Some start; best_dist := d
          | Some _ when d < !best_dist -> best_idx := Some start; best_dist := d
          | _ -> ()
      done;
      match !best_idx with
      | None -> None
      | Some i ->
          Some (Array.init n (fun j -> arr.(i + j)))

  let newton_value pts x =
    let n = Array.length pts in
    let coeffs = Array.make n 0.0 in
    for i = 0 to n - 1 do
      coeffs.(i) <- pts.(i).y
    done;
    for j = 1 to n - 1 do
      for i = n - 1 downto j do
        coeffs.(i) <-
          (coeffs.(i) -. coeffs.(i - 1))
          /. (pts.(i).x -. pts.(i - j).x)
      done
    done;
    let acc = ref coeffs.(n - 1) in
    for i = n - 2 downto 0 do
      acc := !acc *. (x -. pts.(i).x) +. coeffs.(i)
    done;
    !acc

  let step ~step ~n state p_new =
    let points' = state.points @ [ p_new ] in
    let next_x =
      match state.next_x with
      | Some x -> Some x
      | None ->
          if List.length points' >= n then
            Some (List.hd points').x
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
                loop ( { x = xx; y } :: acc ) (Some (xx +. step))
    in
    let next_x', outs =
      match next_x with
      | None -> (None, [])
      | Some x0 -> loop [] (Some x0)
    in
    ({ points = points'; next_x = next_x' }, outs)

  let flush ~step ~n state last_x =
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
end

(* ---------------- parse_point moved from main.ml ---------------- *)

let parse_point line =
  let open String in
  let line = trim line in
  if line = "" then None
  else
    let buf = Bytes.of_string line in
    for i = 0 to Bytes.length buf - 1 do
      match Bytes.get buf i with
      | ';' | ',' | '\t' -> Bytes.set buf i ' '
      | _ -> ()
    done;
    let s = Bytes.to_string buf in
    let parts =
      s |> split_on_char ' ' |> List.filter (fun s -> s <> "")
    in
    match parts with
    | [sx; sy] ->
        (try
           let x = float_of_string sx in
           let y = float_of_string sy in
           Some { x; y }
         with _ -> None)
    | _ -> None
