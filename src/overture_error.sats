(*
** Overture: error reporting
**
** Two flavors, following patsopt:
** - [fatal_error]: unrecoverable (lexing/parsing); print and exit(1)
** - [errmsg]: recoverable (binding/typechecking); bump the error count
**   so multiple errors can be reported per run, then [abort_if_errors]
**   is called between phases.
*)

staload "./overture_location.sats"

(* ****** ****** *)

fun the_errcount_get (): int

(* ****** ****** *)

fun errmsg (loc: location, msg: string): void
fun errmsg_noloc (msg: string): void

(* prints the message and exits with status 1 *)
fun fatal_error (loc: location, msg: string): void

fun abort_if_errors (): void

(* ****** ****** *)

(* end of [overture_error.sats] *)
