staload "rat.sats"
staload "flow.sats"

#define :: flow_cons

(**
  Here's a simple test of the tail operator. 

  node tail_twice(i: int rate(10,0))
  returns (o1: int rate(10,1); o2: int rate(10,2))
  let
    o1=tail(i);
    o2=tail(o1);
  tel
*)

fun tail_twice (
  i: strict_flow (int, 10, Rational(0))
): (strict_flow (int, 10, Rational(1)), strict_flow (int, 10, Rational(2))) = let
  val j = flow_tail (i)
  val k = flow_tail (j)
in
  (j, k)
end

