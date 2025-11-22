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
  | None -> ({ prev = Some p; next_x = None }, [])
  | Some a ->
      if Float_util.eq p.x a.x then
        ({ prev = Some p; next_x = None }, [])
      else
        let lo = if a.x < p.x then a else p in
        let hi = if a.x < p.x then p else a in
        let start_x =
          match st.next_x with
          | Some nx when nx > lo.x +. Float_util.eps -> nx
          | _ -> Grid_util.start_grid_x ~step lo.x
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
