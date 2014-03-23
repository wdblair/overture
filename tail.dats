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

fun tail_twice {n:pos} (
  i: strict_flow (int, n, Rational(0))
): (
  strict_flow (int, n, Rational(1)), strict_flow (int, n, Rational(2))
) = let
  val j = flow_tail (i)
  val k = flow_tail (j)
in
  (j, k)
end

(**

And an example of taking apart a flow and
putting it back together

  node init(i: int rate(10,0))
  returns (o1: int rate(10,0); o2: int rate(10,0))
  
  var v1,v2;
  let
  
  (v1,v2)=tail_twice(i);
  o1=0::v1;
  o2=0::0::v2;
  
  tel

We can make these nodes a little bit more general if 
we put a quantifier over the rate of the flows involved 
since we only deal with concatenation. Maybe this support 
could be added to the prelude language itself, where you 
can describe the type signature of nodes in terms of quantified
variables as opposed to constants...
*)

fun init {n:pos} (
  i: strict_flow (int, n, Rational(0))
): (
  strict_flow (int, n, Rational(0)), strict_flow(int, n, Rational(0))
) = let
  val (v1, v2) = tail_twice (i)
  val o1 = 0 :: v1
  val o2 = 0 :: 0 :: v2
in
  (o1, o2)
end