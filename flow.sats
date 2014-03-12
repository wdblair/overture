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

(**
  A simple flow type
  
  Values of type a only occur at times t_0, t_1,... where for all 
  i >= 0 t_{i+1} - t_i = n and t_0 = n*p.
*)
absvt@ype flow (a:t@ype, n: int, p: rat)

(**
  A strictly periodic flow type
  
  Values of type a _always_ occur at times t_0, t_1, ... where for all i >=0
  t_{i+1} - t_i = n and t_0 = n*p.
*)
absvt@ype strict_flow (a:t@ype, n: int, p: rat)

(**
  The strictly periodc flow transformation operators as specified by prelude
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
  Concat a value to the head of the flow.
*)
fun
flow_concat {a:t@ype} {n:pos} {p:rat | is_nat(Rational(n)*p)} (
  a, strict_flow (a, n, p)
): strict_flow (a, n, p - Rational(1))

(**
  Using when on two strictly periodic clocks yields a new clock that
  is x_i if at time t_i activate_i evaluates to true and undefined
  otherwise.
*)
fun
flow_when {a:t@ype} {n:pos} {p:rat | is_nat(Rational(n)*p)} (
  activate: strict_flow (bool, n, p), x: strict_flow (a, n, p)
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
  
  I haven't decided how tricky this might be to work into how Prelude
  does compilation...
*)
fun
flow_divide_clock_keep {a:t@ype} {n,k:pos}  {p:rat | is_nat(Rational(n)*p)} (
  strict_flow (a, n, p), int k
): strict_flow (@[a][k], n*k, p)