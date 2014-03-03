(**
  A flow is the primary data type used in Prelude. Here, we define it as an abstract
  type indexed by the flat type that appears in the flow, the period with which a value of
  that type becomes available, and the flow's phase offset.
  
  We limit all task periods as integers as that fits the constraints of schedulability 
  analysis. Phase offsets are allowed to be rational. The addition of a rational type in
  the statics is made possible by using an SMT solver such as Z3 as our constraint solver
  during type checking.
*)

(**
  Declare a rational static type and its constructors.
*)
datasort rat =
  | Rational of (int)
  | RationalDiv of (int, int)

(**
  Provide static functions on rational numbers.
*)
stacst mul_rat_rat : rat -> rat -> rat
stadef * = mul_rat_rat

stacst add_rat_rat : rat -> rat -> rat
stadef + = add_rat_rat

(**
  A built-in function provided by Z3
  
  Example:
  (assert (is_int 0.0))
*)
stacst is_int : rat -> bool

stadef is_nat (n) = is_int (n) && n >= 0

(**
  The flow data type
*)
absvt@ype flow (a:t@ype, rate: int, phase: rat)

(**
  The flow transformations as specified by prelude
*)
stacst divides_int_int : int -> int -> bool
stadef divides = divides_int_int

stadef divides (a:int, b:int) = (b mod a == 0)

fun
flow_divide_clock {a:t@ype} {period,k:pos} {phase:rat} (
  flow (a, period, phase), int k
): flow (a, period * k, phase / k)

fun
flow_multiply_clock {a:t@ype} {period,k:pos | divides(d, period)} {phase:rat} (
  flow (a, period, phase), int k
): flow (a, period / k, phase * k)

fun
flow_shift_phase {a:t@ype} {period:pos} {phase,k:rat | is_nat((phase + k)*Rational(period))} (
  flow (a, period, phase), int k
): flow (a, period, phase + k)