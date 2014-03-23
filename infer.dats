staload "rat.sats"
staload "flow.sats"

(** 
  Test composing nodes
  
  Prelude offers inference in its clock calculus, and
  this example explores translating a simple program
  to a corresponding ATS program.
  
  node poly(i: int rate (10, 0); j: int rate (5, 0))
    returns (o, p: int)
  let
    o=under_sample(i);
    p=under_sample(j);
  tel
  
  node under_sample (i: int) returns (o: int)
  let
    o=i/^2;
  tel
  
  A naive way to go about inference is to just recursively
  traverse a Prelude program and derive the corresponding
  ATS types. Let's start with the example given above.
  
  node poly
  
  input:
  i: (int, 10, 0), j (int, 5, 0)
  
  output:
  o: int, p: int
  
  Right now this gives me a type
  
  poly (i:(int,10,0),j:(int,10,0)): 
    \exists n,m \in \mathbb{Z} p,q \mathbb{Q} (o: (int, n, p), o: (int, m,q))
    
  I previously assumed that the absence of rate information 
  for a flow made it synchronous, but I see now that is not
  necessarily the case. Indeed, there really isn't a global
  clock as we have in Esterel or Lustre.
  
  For each flow returned, go through the node and collect the
  operations performed on it
  
  o=under_sample(i);
  
  We don't know the type of under_sample, recurse on it
  
  node under_sample
  
  input:
  i: (int, n, p)
  
  output
  \exists m \in \mathbb{Z} ,q \in \mathbb{Q} s.t. o: (int, m, q)
  
  o = i /^ 2

  The operator /^ is defined exactly, so I can express o in terms of 
  i.
  
    o: (int, n * 2, p / 2)

  and I know the type of under_sample
  
  fun 
  under_sample {n:nat} {p:rat} (
    strict_flow (int, n, p)
  ): strict_flow (int, n *2, p / 2)

  Now, back to o=under_sample (i)
  
  o : (int, 10 * 2, 0)
  o : (int, 20, 0)
  
  and
  
  p = under_sample (j)
  
  p : (int, 5 * 2, 0)
  p : (int, 20, 0)
  
  And the type of this instance of  poly is determined to be
  
  fun 
  poly (i: strict_flow (10, 0), j: strict_flow (5, 0)):
    (strict_flow (20, 0), strict_flow (10, 0))
*)

fun
under_sample {n:pos} {p:rat | is_nat(Rational(n)*p)} (
  i: strict_flow (int, n, p)
): strict_flow (int, n * 2, p / Rational(2)) = let
  val o = flow_divide_clock (i, 2)
in
  o
end

fun 
poly (
  i: strict_flow (int, 10, Rational(0)), j: strict_flow (int, 5, Rational(0))
): (
  strict_flow (int, 10*2, Rational(0)), strict_flow (int, 5*2, Rational(0))
) = let
  val o = under_sample (i)
  val p = under_sample (j)
in
  (o, p)
end