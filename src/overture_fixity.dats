(*
** Overture: operator fixity
*)

#include "share/atspre_staload.hats"

staload "libats/ML/SATS/basis.sats"
staload "libats/ML/SATS/list0.sats"
staload _ = "libats/ML/DATS/list0.dats"

staload "./overture_symbol.sats"
staload "./overture_fixity.sats"

(* ****** ****** *)

typedef fxent = @(symbol, fxty)

val the_fxtytbl = ref<list0(fxent)> (list0_nil ())

(* ****** ****** *)

implement
the_fxtytbl_insert
  (oper, fxty) =
  the_fxtytbl[] :=
    list0_cons (@(oper, fxty), the_fxtytbl[])
(* end of [the_fxtytbl_insert] *)

implement
the_fxtytbl_search
  (oper) = let
//
fun search
  (ents: list0(fxent)): option0 (fxty) =
(
  case+ ents of
  | list0_nil () => None0 ()
  | list0_cons (ent, ents) =>
      if oper = ent.0 then Some0 (ent.1) else search (ents)
) (* end of [search] *)
//
in
  search (the_fxtytbl[])
end // end of [the_fxtytbl_search]

(* ****** ****** *)

implement
fixity_initialize
  ((*void*)) = let
//
fn fxleft
  (name: string, prec: int): void =
  the_fxtytbl_insert
    (symbol_make (name), FXTYinfix (prec, ASSOCleft ()))
fn fxright
  (name: string, prec: int): void =
  the_fxtytbl_insert
    (symbol_make (name), FXTYinfix (prec, ASSOCright ()))
fn fxnone
  (name: string, prec: int): void =
  the_fxtytbl_insert
    (symbol_make (name), FXTYinfix (prec, ASSOCnone ()))
//
val () = fxleft ("||", 40)
val () = fxleft ("&&", 50)
val () = fxnone ("|", 55)
val () = fxnone ("==", 60)
val () = fxnone ("!=", 60)
val () = fxnone ("<", 60)
val () = fxnone ("<=", 60)
val () = fxnone (">", 60)
val () = fxnone (">=", 60)
val () = fxright ("fby", 68)
val () = fxleft ("when", 66)
val () = fxleft ("+", 70)
val () = fxleft ("-", 70)
val () = fxleft ("*", 80)
val () = fxleft ("/", 80)
val () = fxleft ("*/", 90)
val () = fxleft ("^/", 90)
//
in
  // nothing
end // end of [fixity_initialize]

(* ****** ****** *)

(* end of [overture_fixity.dats] *)
