(*
** Overture: operator fixity
**
** A dynamic fixity table in the spirit of pats_fixity: operators are
** declared infixl/infixr/prefix with a precedence, either by the
** built-in defaults (registered by [fixity_initialize]) or by fixity
** declarations in source files. The parser consults the table while
** resolving operator item lists; both static and dynamic expressions
** share the same table.
*)

staload "libats/ML/SATS/basis.sats"

staload "./overture_symbol.sats"

(* ****** ****** *)

datatype assoc = ASSOCleft | ASSOCright | ASSOCnone

datatype
fxty =
| FXTYinfix of (int(*prec*), assoc)
| FXTYprefix of (int(*prec*))

(* ****** ****** *)

fun fixity_initialize (): void (* register the built-in defaults *)

fun the_fxtytbl_insert (oper: symbol, fxty: fxty): void
fun the_fxtytbl_search (oper: symbol): option0 (fxty)

(* ****** ****** *)

(*
** built-in default precedences:
**
**   infixl 40 ||
**   infixl 50 &&
**   nonfix 55 |            (divides)
**   nonfix 60 == != < <= > >=
**   infixr 68 fby
**   infixl 70 + -
**   infixl 80 * /
**   infixl 90 */ ^/
*)

(* ****** ****** *)

(* end of [overture_fixity.sats] *)
