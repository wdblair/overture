(*
** Overture: typechecking and constraint generation (trans3)
**
** Applications instantiate a node signature's universal variables by
** structural matching of flow types (a bare unbound variable is
** bound; anything else becomes an equality obligation); the guard is
** emitted as a proof obligation; existential results are opened as
** fresh hypothetical variables. Equations discharge flow-type
** equality to clock-equality obligations. All obligations end up in
** c3nstr trees, one per node plus one for the top level.
*)

staload "libats/ML/SATS/basis.sats"
staload "libats/ML/SATS/list0.sats"

staload "./overture_trans12.sats"
staload "./overture_constraint.sats"

(* ****** ****** *)

fun trans3_program (prog: p2rog): list0 (c3nstr)

(* ****** ****** *)

(* end of [overture_trans3.sats] *)
