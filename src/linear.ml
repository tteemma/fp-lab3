open Interp_sig

type params = unit

type state = {
  prev : point option;
  next_x : float option;
}

let empty = { prev = None; next_x = None }

let lerp (a : point) (b : point) (x : float) : point =
  let t = (x -. a.x) /. (b.x -. a.x) in
  { x; y = a.y +. t *. (b.y -. a.y) }

let step ~step ~params:() st p =
  match st.prev with
  | None -> ({ st with prev = Some p }, [])
  | Some prev_p ->
      let lo, hi =
        if Float_util.lt prev_p.x p.x then (prev_p, p) else (p, prev_p)
      in
      if Float_util.eq lo.x hi.x then
        ({ prev = Some p; next_x = st.next_x }, [])
      else
        let start_x =
          match st.next_x with
          | Some nx -> nx
          | None -> Grid_util.start_grid_x ~step lo.x
        in
        let rec gen x acc =
          if x < hi.x -. Float_util.eps then
            gen (x +. step) (lerp lo hi x :: acc)
          else
            List.rev acc
        in
        let pts = gen start_x [] in
        let next_x =
          Some (start_x +. (Float.of_int (List.length pts)) *. step)
        in
        ({ prev = Some p; next_x }, pts)

let flush ~step:_ ~params:() _st ~last_x:_ =
  []
