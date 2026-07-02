(*
** Overture: parsing
**
** Hand-written recursive descent over the token list. Expressions
** (both static and dynamic) are collected as operand/operator item
** lists and resolved by precedence climbing against the fixity
** table; fixity declarations take effect immediately, so operators
** declared in a file are usable below the declaration.
*)

staload "libats/ML/SATS/basis.sats"
staload "libats/ML/SATS/list0.sats"

staload "./overture_lexing.sats"
staload "./overture_syntax.sats"

(* ****** ****** *)

fun parsing_program (toks: list0(token)): d0eclst

fun parsing_program_file (path: string): d0eclst

(* ****** ****** *)

(* end of [overture_parsing.sats] *)
