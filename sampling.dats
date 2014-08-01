staload "rat.sats"
staload "flow.sats"

(** 
  An example of using undersampling to allow a fast process 
  F to communicate with a slower process S.
 
  I'm interested in how I can capture the notion of concurrency 
  between flows.
  
  suppose I have the following system
  
  -> F -> S
  <- F <- S
  
  Where F and S are periodic tasks. Obviously, we have a causality
  loop if we do the naive approach
  
    node sample (i: rate (10, 0)): returns o 
    let
      var vf, vs
      (o, vf) = F (i, vs /^ 3)
      vs = S (vf *^ 3)
    tel
  
  The issue of causality doesn't really need to be addressed by ATS
  since I am mostly using this to check constraints produced  by the
  clock calculus.
  
  At the bare minimum, I want some mechanism to express the concurrent
  dependency between these tasks. I think linear types can capture this.
*)

extern fun
F {n:pos}{p:rat}(strict_flow (int, n, p), strict_flow (int, n, p)):
  (strict_flow (int, n, p), strict_flow (int, n, p))
  
extern fun
S {n:pos}{p:rat}(strict_flow (int, n, p)): strict_flow (int, n, p)

fun
sample (i: strict_flow (int, 10, Rational(0))):
  [n:nat] [p:rat] strict_flow (int, n, p) = let
  var vs : strict_flow (int, 30, Rational(0))
  var vf : strict_flow (int, 10, Rational(0))
  //
  prval pfvs = flow_future_make (vs)
  prval pfvf = flow_future_make (vf)
  //
  val (o, vf') = F (i, flow_multiply_clock (vs, 3))
  
  val vs' = S (flow_divide_clock (vf, 3))
  prval () = flow_future_elim (pfvs, vs, vs')
  prval () = flow_future_elim (pfvf, vf, vf')
in
  o
end