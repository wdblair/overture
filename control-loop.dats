staload "rat.sats"
staload "flow.sats"

(** 
  Take a look at page 7 of Jane Liu's Real-Time Systems
  for a more complete example.
*)
extern
fun roll (): strict_flow (int, 10, Rational(0))

extern
fun elevator_control (strict_flow (int, 100, Rational(0))): void

fun 
balance_plane (): void = let
  val r = roll ()
in
  elevator_control (flow_divide_clock (r, 10))
end
