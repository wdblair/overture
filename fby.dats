staload "rat.sats"
staload "flow.sats"

#define :: flow_cons

extern
castfn to_rat {n:int} (n: int n): rational(Rational(n))

(**
  A possible implementation of fby
*)
fun fby {a:t@ype} {n:pos} {p:rat | is_nat(Rational(n)*p)} (
  x: a, i: strict_flow (a, n, p)
): strict_flow (a, n, p) = let
  val i' = flow_shift_phase (i, to_rat (1))
in
  x :: i'
end