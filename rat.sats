(**
  An interface to static rational numbers supported by Z3
*)

(**
  Declare a rational static type and its constructors
*)
datasort rat =
  | Rational of (int)
  | RationalDiv of (int, int)
  
(**
  Provide static functions on rational numbers
*)
stacst mul_rat_rat : (rat, rat) -> rat
stadef * = mul_rat_rat

stacst div_rat_rat : (rat, rat) -> rat
stadef / = div_rat_rat

stacst add_rat_rat : (rat, rat) -> rat
stadef + = add_rat_rat

stacst sub_rat_rat : (rat, rat) -> rat
stadef - = sub_rat_rat

stacst gte_rat_rat : (rat, rat) -> bool
stadef >= = gte_rat_rat

(**
  A built-in function provided by Z3
  
  Example:
  (assert (is_int 0.0))
*)
stacst is_int_int : rat -> bool

stadef is_nat (n:rat) = is_int_int(n) && (n >= Rational(0))

(**
  Check whether a | b
*)
stadef divides (a:int, b:int) = (b mod a == 0)