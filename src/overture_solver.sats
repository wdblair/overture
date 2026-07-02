(*
** Overture: the constraint solver
**
** A monomorphic port of the pats_lintprgm approach. Every clock is
** lowered to a pair of integer unknowns (period n, activation date
** d = n * p), which makes the paper's validity condition n * p being
** a natural number structural and keeps every clock operator linear:
**
**   clk(n, p)     |->  (n, n * p)         (p a rational literal)
**   c */ k        |->  (n / k, d)         (dates are invariant)
**   c ^/ k        |->  (n * k, d)
**   shift(c, a/b) |->  (n, d + n * a / b)
**
** Proof obligations are checked by validity: refute the hypotheses
** conjoined with the negated goal, via Gaussian elimination on
** equalities and integer-tightened Fourier-Motzkin elimination on
** inequalities. Divisibility and integrality goals are proved by
** substituting unit-coefficient equality hypotheses and checking
** that the divisor divides all residual coefficients.
**
** Unsolved obligations are reported through [errmsg], printing the
** original surface-syntax goal and hypotheses.
*)

staload "libats/ML/SATS/basis.sats"
staload "libats/ML/SATS/list0.sats"

staload "./overture_constraint.sats"

(* ****** ****** *)

fun solver_solve (cs: list0(c3nstr)): void

(* ****** ****** *)

(* end of [overture_solver.sats] *)
