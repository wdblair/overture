staload "rat.sats"
staload "flow.sats"

(**
  Trying out a more general approach to see what I need to infer.
*)

extern fun
swap {n:pos} {p:rat | is_nat (Rational(n) * p)} (
  strict_flow (int, n, p), strict_flow (int, n, p)
): (strict_flow (int, n, p), strict_flow (int, n, p))

extern fun
id {n:pos} {p:rat | is_nat (Rational(n) * p)} (
  strict_flow (int, n, p)
): (strict_flow (int, n, p))

(** 
  So in the absence of explicit rates for imported nodes, the only things
  I really need to infer are the rates of the local variables.
  
  One thing that's missing from all this is some way to extract all the
  computed rates during type checking so the tasks can be 
  constructed from the program.
*)

fun 
sampling (
  i: strict_flow (int, 500, Rational(0))
): [n:pos] [p:rat] (
  strict_flow (int, n, p)
) = let
  var vf : StrictFlow (int)
  var vs : StrictFlow (int)
  //
  prval pfvf = set_clock (vf, 500, rational_pf (0, 1))
  prval pfvs = set_clock (vs, 1500, rational_pf (0,1))
  //
  val (o, vf') = swap (i, flow_multiply_clock (flow_fby (5, vs), 3))
  val (vs') = id (flow_divide_clock (vf, 3))
  //
  prval () = flow_future_elim (pfvf, vf, vf')
  prval () = flow_future_elim (pfvs, vs, vs')
in
  (o)
end
