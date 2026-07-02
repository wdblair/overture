(*
** Overture: interned symbols
*)

#include "share/atspre_staload.hats"

staload "libats/ML/SATS/basis.sats"
staload "libats/ML/SATS/list0.sats"
staload _ = "libats/ML/DATS/list0.dats"

staload "./overture_symbol.sats"

(* ****** ****** *)

datatype
symbol_ = SYMBOL of (string, int)

assume symbol = symbol_

(* ****** ****** *)
//
// the global intern table: a simple association list;
// symbol counts stay small for Overture programs
//
val the_symtbl = ref<list0(symbol_)> (list0_nil ())
val the_symcnt = ref<int> (0)
//
(* ****** ****** *)

implement
symbol_make (name) = let
//
fun search
  (syms: list0(symbol_)): Option_vt (symbol_) =
(
  case+ syms of
  | list0_nil () => None_vt ()
  | list0_cons (sym, syms1) => let
      val SYMBOL (name1, _) = sym
    in
      if name = name1 then Some_vt (sym) else search (syms1)
    end
) (* end of [search] *)
//
val opt = search (the_symtbl[])
//
in
//
case+ opt of
| ~Some_vt (sym) => sym
| ~None_vt () => let
    val stamp = the_symcnt[]
    val () = the_symcnt[] := stamp + 1
    val sym = SYMBOL (name, stamp)
    val () = the_symtbl[] := list0_cons (sym, the_symtbl[])
  in
    sym
  end // end of [None_vt]
//
end // end of [symbol_make]

(* ****** ****** *)

implement
symbol_get_name (sym) =
  let val SYMBOL (name, _) = sym in name end
implement
symbol_get_stamp (sym) =
  let val SYMBOL (_, stamp) = sym in stamp end

(* ****** ****** *)

implement
eq_symbol_symbol (s1, s2) =
  symbol_get_stamp (s1) = symbol_get_stamp (s2)
implement
neq_symbol_symbol (s1, s2) =
  symbol_get_stamp (s1) != symbol_get_stamp (s2)

(* ****** ****** *)

implement
fprint_symbol (out, sym) =
  fprint! (out, symbol_get_name (sym))
implement
print_symbol (sym) =
  fprint_symbol (stdout_ref, sym)

(* ****** ****** *)

(* end of [overture_symbol.dats] *)
