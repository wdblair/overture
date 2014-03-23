staload "rat.sats"
staload "flow.sats"

(**

This demonstrates the use of conditional operators with
boolean clocks. The semantics of Prelude allow us to use
boolean transformations on strictly periodic flows, but
never the converse. Strictly periodic operators like 
divide and multiply are undefined for flows subject to 
boolean transformations.

Forget's thesis specifies that the advised approach is 
to use the periodic operators to first make flows 
synchronous, and then apply boolean transformations to
to satisfy conditonal constraints in the system.

node condperiodic (c: rate(5,0); i: rate (10,0)) 
returns (o)
let
  o=(i*^2) when c;
tel

*)
fun condperiodic {a:t@ype} {n:pos | divides(2, n)} (
  c: strict_flow (bool, n/2, Rational(0)), i: strict_flow (a, n, Rational(0))
): flow (a, n/2, Rational (0)) = let
  val i2 = flow_multiply_clock (i, 2)
  val o = flow_when (c, i2)
in
  o
end