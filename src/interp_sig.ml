type point = { x : float; y : float }

module type INTERP = sig
  type state
  type params

  val empty : state

  val step :
    step:float ->
    params:params ->
    state ->
    point ->
    state * point list

  val flush :
    step:float ->
    params:params ->
    state ->
    last_x:float ->
    point list
end
