(*
** Overture: constraint trees
**
** A miniature of pats_trans3_env.sats: while trans3 walks a node it
** accumulates static variables (S3ITMsvar), hypotheses (S3ITMhypo),
** and proof obligations (S3ITMcnstr) into an item list; the solver
** later walks the tree, refuting (hypotheses /\ ~goal) per obligation.
*)

staload "libats/ML/SATS/basis.sats"
staload "libats/ML/SATS/list0.sats"

staload "./overture_location.sats"
staload "./overture_staexp.sats"

(* ****** ****** *)

datatype
h3ypo = H3YPOprop of (location, s2exp)

datatype
s3itm =
| S3ITMsvar of s2var
| S3ITMhypo of h3ypo
| S3ITMcnstr of c3nstr

and
c3nstr_node =
| C3NSTRprop of s2exp
| C3NSTRitmlst of s3itmlst

where
c3nstr = '{
  c3nstr_loc= location
, c3nstr_msg= string (* what obligation this is, for error reporting *)
, c3nstr_node= c3nstr_node
} (* end of [c3nstr] *)

and s3itmlst = list0 (s3itm)

(* ****** ****** *)

fun c3nstr_prop
  (loc: location, msg: string, s2e: s2exp): c3nstr
fun c3nstr_itmlst
  (loc: location, msg: string, items: s3itmlst): c3nstr

fun fprint_c3nstr (out: FILEref, c3t: c3nstr): void

(* ****** ****** *)
//
// the item accumulator used by trans3 (items are kept reversed)
//
typedef c3acc = ref (s3itmlst)

fun c3acc_new (): c3acc
fun c3acc_svar (acc: c3acc, s2v: s2var): void
fun c3acc_hypo (acc: c3acc, loc: location, s2e: s2exp): void
fun c3acc_cnstr (acc: c3acc, c3t: c3nstr): void
fun c3acc_prop
  (acc: c3acc, loc: location, msg: string, s2e: s2exp): void
fun c3acc_takeout (acc: c3acc): s3itmlst (* in declaration order *)

(* ****** ****** *)

(* end of [overture_constraint.sats] *)
