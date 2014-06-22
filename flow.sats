(**
  A flow is the primary data type  used in Prelude. Here, we define it
  as an  abstract type indexed  by the flat  type that appears  in the
  flow, the period with which a  value of that type becomes available,
  and the flow's phase offset.
  
  We limit  all task  periods as  natural numbers  since a  discrete time
  model  is required  by  the  scheduling theory  and  most real  time
  operating systems. Task phases are allowed to be rational, under the
  constraint that  the release time for  the task is a  natural number
  (i.e. it is  a valid date). The  addition of a rational  type in the
  statics is made  possible by using an  SMT solver such as  Z3 within 
  our constraint solver during type checking.
*)

staload "rat.sats"

abst@ype rational (r: rat)

fun rational_make {p,q:int} (int p, int q): rational (RationalDiv(p,q))

praxi rational_pf {p,q:int} (int p, int q): rational (RationalDiv(p,q))


(**
  A simple flow type
  
  Values of type a only occur at times t_0, t_1,... where for all 
  i >= 0 t_{i+1} - t_i = n and t_0 = n*p.
*)
abst@ype flow (a:t@ype, n: int, p: rat)

(**
  A strictly periodic flow type
  
  Values of type a _always_ occur at times t_0, t_1, ... where for all i >=0
  t_{i+1} - t_i = n and t_0 = n*p.
*)
abst@ype strict_flow (a:t@ype, n: int, p: rat)

typedef StrictFlow (a:t@ype) = [n:pos] [p:rat] strict_flow (a, n, p)

(**
  Create a flow
*)
fun
flow_make {a:t@ype} {n:nat} {p:rat} (): strict_flow (a, n, p)

(** 
  The following axioms don't capture much, but I think they 
  could be an interesting way to work linear types into the design
  of the system.
  
  What's needed is an index that expresses the flow for which 
  we are providing a future value. This requires refining the flow type 
  with  possibly a typekind to capture the node that generated 
  any given flow.
*)
absview FlowFuture

praxi set_clock {a:t@ype} {m:pos} {q:rat}   (
  &StrictFlow (int)? >> strict_flow (a, m, q), int m, rational (q)
): FlowFuture

praxi
flow_future_make {a:t@ype} {n:nat} {p:rat} (
  &strict_flow (a, n, p)? >> strict_flow (a, n, p)
): FlowFuture

praxi
flow_future_elim {a:t@ype} {n:nat} {p:rat} (
  FlowFuture, &strict_flow (a, n, p)?, strict_flow (a, n, p)
): void


(**
  The strictly periodic flow transformation operators specified by prelude
*)

(**
  Dividing a periodic clock by k is equivalent to sampling 
  its every kth occurence of the clock (undersampling).
*)
fun
flow_divide_clock {a:t@ype} {n,k:pos} {p:rat | is_nat(Rational(n)*p)} (
  strict_flow (a, n, p), int k
): strict_flow (a, n * k, p / Rational(k))

(**
  Multiplying a periodic clock by k is equivalent to sampling
  every k times more of the clock (oversampling).
*)
fun
flow_multiply_clock {a:t@ype} {n,k:pos | divides(k, n)} {p:rat | is_nat(Rational(n)*p)} (
  strict_flow (a, n, p), int k
): strict_flow (a, n / k, p * Rational(k))

(**
  A phase shift moves the release time of a task by some fraction of its period.
*)
fun
flow_shift_phase {a:t@ype} {n:pos} {p,k:rat | is_nat((p + k)*Rational(n))} (
  strict_flow (a, n, p), rational k
): strict_flow (a, n, p + k)

(**
  Drop the first value of a flow. This is equivalent to a clock with phase shift 1
*)
fun
flow_tail {a:t@ype} {n:pos} {p:rat | is_nat(Rational(n)*p)} (
  strict_flow (a, n, p)
): strict_flow (a, n, p + Rational(1))

(**
  Concat a value to the head of the flow. I'm calling it cons since
  it seems closer to cons for list then concatenation.
*)
fun
flow_cons {a:t@ype} {n:pos} {p:rat | is_nat(Rational(n)*p)} (
  a, strict_flow (a, n, p)
): strict_flow (a, n, p - Rational(1))

(**
  Introduce a delay of a flow
*)
fun
flow_fby {a:t@ype} {n:pos} {p:rat | is_nat(Rational(n)*p)} (
  a, strict_flow (a, n, p)
): strict_flow (a, n, p)

(**
  Using when on two strictly periodic clocks yields a new clock that
  is x_i if at time t_i activate_i evaluates to true and undefined
  otherwise.
*)
fun
flow_when {a:t@ype} {n:pos} {p:rat | is_nat(Rational(n)*p)} (
   x: strict_flow (a, n, p),  activate: strict_flow (bool, n, p)
): flow (a, n, p)

fun
flow_whennot {a:t@ype} {n:pos} {p:rat | is_nat(Rational(n)*p)} (
   x: strict_flow (a, n, p),  activate: strict_flow (bool, n, p)
): flow (a, n, p)


(**
  Combining two flows  under complementary boolean clocks. The  new flow's value
  at date t_i is defined as on_i if c_i is true and off_i otherwise.
*)
fun
flow_merge {a:t@ype} {n:pos} {p:rat | is_nat(Rational(n)*p)} (
  c: strict_flow (bool, n, p), on: strict_flow (a, n, p), off: strict_flow (a,n,p)
): strict_flow (a, n, p)

(**
  Transform a flow of single values into a flow of arrays.
  
  This could be useful if a system would rather not drop values
  during undersampling.
  
  Forget has an operator like this in his thesis, but I don't think the
  compiler supports it yet.
*)
fun
flow_divide_clock_queue {a:t@ype} {n,k:pos}  {p:rat | is_nat(Rational(n)*p)} (
  strict_flow (a, n, p), int k
): strict_flow (@[a][k], n*k, p)