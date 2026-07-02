(*
** Overture: interned symbols
**
** Modeled on pats_symbol.sats: a symbol is a name interned into a
** global table; equality is by stamp comparison.
*)

(* ****** ****** *)

abstype symbol = ptr

(* ****** ****** *)

fun symbol_make (name: string): symbol

fun symbol_get_name (sym: symbol): string
fun symbol_get_stamp (sym: symbol): int

(* ****** ****** *)

fun eq_symbol_symbol (s1: symbol, s2: symbol): bool
fun neq_symbol_symbol (s1: symbol, s2: symbol): bool

overload = with eq_symbol_symbol
overload != with neq_symbol_symbol
overload <> with neq_symbol_symbol

(* ****** ****** *)

fun fprint_symbol (out: FILEref, sym: symbol): void
fun print_symbol (sym: symbol): void

(* ****** ****** *)

(* end of [overture_symbol.sats] *)
