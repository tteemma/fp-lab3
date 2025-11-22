open Interp_sig

type params = unit

type state = {
  prev : point option;
  next_x : float option;
}

let empty = { prev = None; next_x = None }

let step ~step ~params:() state p_new =
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
            if x > p_new.x then (st, List.rev acc)
            else
              let dx = p_new.x -. p_prev.x in
              let t =
                if dx = 0.0 then 0.0
                else (x -. p_prev.x) /. dx
              in
              let y =
                p_prev.y +. t *. (p_new.y -. p_prev.y)
              in
              let st' =
                { st with next_x = Some (x +. step) }
              in
              loop ({ x; y } :: acc) st'
      in

      let st', outs = loop [] state in
      ({ st' with prev = Some p_new }, outs)

let flush ~step:_ ~params:() _ ~last_x:_ = []
