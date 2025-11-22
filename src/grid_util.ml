let start_grid_x ~(step : float) (min_x : float) : float =
  let k = Float.floor ((min_x /. step) +. 1.0) in
  k *. step
