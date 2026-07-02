(*
** Overture: error reporting
*)

#include "share/atspre_staload.hats"

staload "./overture_location.sats"
staload "./overture_error.sats"

(* ****** ****** *)

val the_errcount = ref<int> (0)

(* ****** ****** *)

implement
the_errcount_get () = the_errcount[]

(* ****** ****** *)

implement
errmsg (loc, msg) = let
  val () = the_errcount[] := the_errcount[] + 1
  val () = fprint_location (stderr_ref, loc)
  val () = fprint! (stderr_ref, ": error: ", msg, "\n")
in
  // nothing
end // end of [errmsg]

implement
errmsg_noloc (msg) = let
  val () = the_errcount[] := the_errcount[] + 1
  val () = fprint! (stderr_ref, "error: ", msg, "\n")
in
  // nothing
end // end of [errmsg_noloc]

(* ****** ****** *)

implement
fatal_error (loc, msg) = let
  val () = fprint_location (stderr_ref, loc)
  val () = fprint! (stderr_ref, ": fatal error: ", msg, "\n")
in
  $extfcall (void, "exit", 1)
end // end of [fatal_error]

(* ****** ****** *)

implement
abort_if_errors () = let
  val n = the_errcount[]
in
  if n > 0 then let
    val () =
      fprint! (stderr_ref, "overture: there are [", n, "] errors\n")
    // end of [val]
  in
    $extfcall (void, "exit", 1)
  end // end of [if]
end // end of [abort_if_errors]

(* ****** ****** *)

(* end of [overture_error.dats] *)
