(*
** Overture: constraint trees
*)

#include "share/atspre_staload.hats"

staload "libats/ML/SATS/basis.sats"
staload "libats/ML/SATS/list0.sats"
staload _ = "libats/ML/DATS/list0.dats"

staload "./overture_location.sats"
staload "./overture_staexp.sats"
staload "./overture_constraint.sats"

(* ****** ****** *)

implement
c3nstr_prop (loc, msg, s2e) =
  '{ c3nstr_loc= loc, c3nstr_msg= msg, c3nstr_node= C3NSTRprop (s2e) }

implement
c3nstr_itmlst (loc, msg, items) =
  '{ c3nstr_loc= loc, c3nstr_msg= msg, c3nstr_node= C3NSTRitmlst (items) }

(* ****** ****** *)

implement
fprint_c3nstr
  (out, c3t) =
(
case+ c3t.c3nstr_node of
| C3NSTRprop (s2e) => let
    val () = fprint! (out, "PROP[", c3t.c3nstr_msg, "]: ")
  in
    fprint_s2exp (out, s2e)
  end
| C3NSTRitmlst (items) => let
    val () = fprint! (out, "ITMLST[", c3t.c3nstr_msg, "] {\n")
    fun loop
      (out: FILEref, items: s3itmlst): void =
      case+ items of
      | list0_nil () => ()
      | list0_cons (itm, items) => let
          val () = (
            case+ itm of
            | S3ITMsvar (s2v) => let
                val () = fprint! (out, "  svar ")
                val () = fprint_s2var (out, s2v)
                val () = fprint! (out, " : ")
              in
                fprint_s2rt (out, s2var_get_srt (s2v))
              end
            | S3ITMhypo (H3YPOprop (_, s2e)) => let
                val () = fprint! (out, "  hypo ")
              in
                fprint_s2exp (out, s2e)
              end
            | S3ITMcnstr (c3t) => let
                val () = fprint! (out, "  ")
              in
                fprint_c3nstr (out, c3t)
              end
          ) : void
          val () = fprint! (out, "\n")
        in
          loop (out, items)
        end
    val () = loop (out, items)
  in
    fprint! (out, "}")
  end
) (* end of [fprint_c3nstr] *)

(* ****** ****** *)

implement
c3acc_new () =
  ref<s3itmlst> (list0_nil ())

implement
c3acc_svar (acc, s2v) =
  !acc := list0_cons (S3ITMsvar (s2v), !acc)

implement
c3acc_hypo (acc, loc, s2e) =
  !acc := list0_cons (S3ITMhypo (H3YPOprop (loc, s2e)), !acc)

implement
c3acc_cnstr (acc, c3t) =
  !acc := list0_cons (S3ITMcnstr (c3t), !acc)

implement
c3acc_prop (acc, loc, msg, s2e) =
  c3acc_cnstr (acc, c3nstr_prop (loc, msg, s2e))

implement
c3acc_takeout (acc) = let
  val items = !acc
  val () = !acc := list0_nil ()
in
  list0_reverse (items)
end // end of [c3acc_takeout]

(* ****** ****** *)

(* end of [overture_constraint.dats] *)
