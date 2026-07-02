(*
** Overture: source locations
*)

#include "share/atspre_staload.hats"

staload "./overture_location.sats"

(* ****** ****** *)

implement
location_make
(
  file, bline, bcol, bofs, eline, ecol, eofs
) = '{
  loc_file= file
, loc_bline= bline, loc_bcol= bcol, loc_bofs= bofs
, loc_eline= eline, loc_ecol= ecol, loc_eofs= eofs
} (* end of [location_make] *)

implement
location_dummy () =
  location_make ("(dummy)", 0, 0, 0, 0, 0, 0)
// end of [location_dummy]

(* ****** ****** *)

implement
location_combine
  (loc1, loc2) = let
//
val b1 = loc1.loc_bofs
and b2 = loc2.loc_bofs
val e1 = loc1.loc_eofs
and e2 = loc2.loc_eofs
//
val takeb1 = (b1 <= b2): bool
val takee1 = (e1 >= e2): bool
//
in '{
  loc_file= loc1.loc_file
, loc_bline= (if takeb1 then loc1.loc_bline else loc2.loc_bline): int
, loc_bcol= (if takeb1 then loc1.loc_bcol else loc2.loc_bcol): int
, loc_bofs= (if takeb1 then b1 else b2): int
, loc_eline= (if takee1 then loc1.loc_eline else loc2.loc_eline): int
, loc_ecol= (if takee1 then loc1.loc_ecol else loc2.loc_ecol): int
, loc_eofs= (if takee1 then e1 else e2): int
} end // end of [location_combine]

(* ****** ****** *)

implement
fprint_location
  (out, loc) =
(
  fprint! (out
  , loc.loc_file, ": "
  , loc.loc_bofs
  , "(line=", loc.loc_bline, ", offs=", loc.loc_bcol, ")"
  , " -- "
  , loc.loc_eofs
  , "(line=", loc.loc_eline, ", offs=", loc.loc_ecol, ")"
  )
) (* end of [fprint_location] *)

implement
print_location (loc) =
  fprint_location (stdout_ref, loc)
// end of [print_location]

(* ****** ****** *)

(* end of [overture_location.dats] *)
