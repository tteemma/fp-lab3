let eps = 1e-12

let eq (a : float) (b : float) : bool =
  Float.abs (a -. b) <= eps

let lt (a : float) (b : float) : bool =
  a < b -. eps

let le (a : float) (b : float) : bool =
  a <= b +. eps
