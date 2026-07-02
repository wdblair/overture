(*
** Overture: code generation to an ATS harness
**
** [ccomp_program] monomorphizes a typechecked program from its
** top-level equations inward (every reachable node instance gets
** ground statics, harvested by the same matching discipline the
** typechecker used) and emits two ATS2 source files:
**
**   <base>_gen.dats   the harness: value cells, ring buffers, a
**                     step function firing each action when its
**                     ground clock (n, d) says so, and a main loop
**                     over logical base ticks
**   <base>_stubs.dats deterministic default implementations of the
**                     sensors, actuators, and extern nodes; users
**                     replace this file for real IO (and later, the
**                     thin OS for hardware)
**
** Clock operators compile to their value-level residue:
**   */ ^/        latest-value copy (dates align by construction)
**   shift, tail  ring-buffered delay lines of depth s/n + 1
**   fby          one-slot delayed state with an initial value
**   cons         same-instant copy with an initial value
**   merge        conditional copy
*)

staload "libats/ML/SATS/basis.sats"

staload "./overture_trans12.sats"

(* ****** ****** *)

(*
** lang: 0 for ATS2 (harness compiled by patscc), 1 for C
*)
fun ccomp_program
  (base: string(*output file basename*), prog: p2rog, lang: int): void
// end of [ccomp_program]

(* ****** ****** *)

(* end of [overture_ccomp.sats] *)
