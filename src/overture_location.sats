(*
** Overture: source locations
**
** Modeled on pats_location.sats: a location is a filename plus a
** begin/end pair of (line, column, byte offset) positions.
*)

(* ****** ****** *)

typedef
location = '{
  loc_file= string
, loc_bline= int, loc_bcol= int, loc_bofs= int
, loc_eline= int, loc_ecol= int, loc_eofs= int
} (* end of [location] *)

(* ****** ****** *)

fun
location_make
(
  file: string
, bline: int, bcol: int, bofs: int
, eline: int, ecol: int, eofs: int
) : location // end of [location_make]

fun location_dummy (): location

(*
** the smallest location spanning both arguments;
** both are assumed to lie in the same file
*)
fun location_combine (loc1: location, loc2: location): location

(* ****** ****** *)

(*
** printed in the style of patsopt:
** file: 10(line=2, offs=3) -- 15(line=2, offs=8)
*)
fun fprint_location (out: FILEref, loc: location): void
fun print_location (loc: location): void

(* ****** ****** *)

(* end of [overture_location.sats] *)
