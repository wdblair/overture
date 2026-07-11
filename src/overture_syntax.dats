(*
** Overture: parse-level abstract syntax
*)

#include "share/atspre_staload.hats"

staload "libats/ML/SATS/basis.sats"
staload "libats/ML/SATS/list0.sats"
staload _ = "libats/ML/DATS/list0.dats"

staload "./overture_symbol.sats"
staload "./overture_location.sats"
staload "./overture_syntax.sats"

(* ****** ****** *)

implement
i0de_make (loc, sym) =
  '{ i0de_loc= loc, i0de_sym= sym }

(* ****** ****** *)

implement
s0exp_make (loc, node) =
  '{ s0e_loc= loc, s0e_node= node }

implement
q0arg_make (loc, sym, srt) =
  '{ q0arg_loc= loc, q0arg_sym= sym, q0arg_srt= srt }

implement
q0ua_make (loc, args, gua) =
  '{ q0ua_loc= loc, q0ua_args= args, q0ua_guard= gua }

implement
d0exp_make (loc, node) =
  '{ d0e_loc= loc, d0e_node= node }

implement
d0ec_make (loc, node) =
  '{ d0ec_loc= loc, d0ec_node= node }

(* ****** ****** *)

implement
fprint_s0rt (out, srt) =
(
  case+ srt of
  | S0RTide (_, sym) => fprint_symbol (out, sym)
) (* end of [fprint_s0rt] *)

(* ****** ****** *)

fun
fprint_s0explst
  (out: FILEref, s0es: list0(s0exp)): void =
(
  case+ s0es of
  | list0_nil () => ()
  | list0_cons (s0e, s0es) => let
      val () = fprint_s0exp (out, s0e)
      val () =
        (case+ s0es of
         | list0_nil () => ()
         | list0_cons _ => fprint! (out, ", "))
    in
      fprint_s0explst (out, s0es)
    end
) (* end of [fprint_s0explst] *)

implement
fprint_s0exp
  (out, s0e) =
(
case+ s0e.s0e_node of
| S0Eint (i) => fprint! (out, i)
| S0Eide (sym) => fprint_symbol (out, sym)
| S0Eapp (f, args) => let
    val () = fprint_symbol (out, f.i0de_sym)
    val () = fprint! (out, "(")
    val () = fprint_s0explst (out, args)
  in
    fprint! (out, ")")
  end
| S0Eopapp (f, args) => let
    val () = fprint! (out, "OP[", symbol_get_name (f.i0de_sym), "](")
    val () = fprint_s0explst (out, args)
  in
    fprint! (out, ")")
  end
| S0Etup (args) => let
    val () = fprint! (out, "(")
    val () = fprint_s0explst (out, args)
  in
    fprint! (out, ")")
  end
| S0Eexi (qua, s0e) => let
    val () = fprint_q0ua (out, qua)
    val () = fprint! (out, " ")
  in
    fprint_s0exp (out, s0e)
  end
) (* end of [fprint_s0exp] *)

(* ****** ****** *)

fun
fprint_q0arglst
  (out: FILEref, args: list0(q0arg)): void =
(
  case+ args of
  | list0_nil () => ()
  | list0_cons (arg, args) => let
      val () = fprint_symbol (out, arg.q0arg_sym)
      val () =
        (case+ arg.q0arg_srt of
         | Some0 (srt) =>
             (fprint! (out, ":"); fprint_s0rt (out, srt))
         | None0 () => ())
      val () =
        (case+ args of
         | list0_nil () => ()
         | list0_cons _ => fprint! (out, "; "))
    in
      fprint_q0arglst (out, args)
    end
) (* end of [fprint_q0arglst] *)

implement
fprint_q0ua
  (out, qua) = let
  val () = fprint! (out, "{")
  val () = fprint_q0arglst (out, qua.q0ua_args)
  val () =
    (case+ qua.q0ua_guard of
     | Some0 (gua) =>
         (fprint! (out, " | "); fprint_s0exp (out, gua))
     | None0 () => ())
in
  fprint! (out, "}")
end // end of [fprint_q0ua]

(* ****** ****** *)

fun
fprint_d0explst
  (out: FILEref, d0es: list0(d0exp)): void =
(
  case+ d0es of
  | list0_nil () => ()
  | list0_cons (d0e, d0es) => let
      val () = fprint_d0exp (out, d0e)
      val () =
        (case+ d0es of
         | list0_nil () => ()
         | list0_cons _ => fprint! (out, ", "))
    in
      fprint_d0explst (out, d0es)
    end
) (* end of [fprint_d0explst] *)

implement
fprint_d0exp
  (out, d0e) =
(
case+ d0e.d0e_node of
| D0Eint (i) => fprint! (out, i)
| D0Eide (sym) => fprint_symbol (out, sym)
| D0Eapp (f, args) => let
    val () = fprint_symbol (out, f.i0de_sym)
    val () = fprint! (out, "(")
    val () = fprint_d0explst (out, args)
  in
    fprint! (out, ")")
  end
| D0Eopapp (f, args) => let
    val () = fprint! (out, "OP[", symbol_get_name (f.i0de_sym), "](")
    val () = fprint_d0explst (out, args)
  in
    fprint! (out, ")")
  end
| D0Etup (args) => let
    val () = fprint! (out, "(")
    val () = fprint_d0explst (out, args)
  in
    fprint! (out, ")")
  end
) (* end of [fprint_d0exp] *)

(* ****** ****** *)

fun
fprint_p0aramlst
  (out: FILEref, ps: list0(p0aram)): void =
(
  case+ ps of
  | list0_nil () => ()
  | list0_cons (p, ps) => let
      val () = fprint_symbol (out, p.p0aram_sym)
      val () = fprint! (out, " : ")
      val () = fprint_s0exp (out, p.p0aram_typ)
      val () =
        (case+ ps of
         | list0_nil () => ()
         | list0_cons _ => fprint! (out, ", "))
    in
      fprint_p0aramlst (out, ps)
    end
) (* end of [fprint_p0aramlst] *)

fun
fprint_e0qn
  (out: FILEref, eqn: e0qn): void = let
//
fun lhs
  (out: FILEref, ides: list0(i0de)): void =
(
  case+ ides of
  | list0_nil () => ()
  | list0_cons (ide, ides) => let
      val () = fprint_symbol (out, ide.i0de_sym)
      val () =
        (case+ ides of
         | list0_nil () => ()
         | list0_cons _ => fprint! (out, ", "))
    in
      lhs (out, ides)
    end
) (* end of [lhs] *)
//
val () = fprint! (out, "(")
val () = lhs (out, eqn.e0qn_lhs)
val () = fprint! (out, ") = ")
//
in
  fprint_d0exp (out, eqn.e0qn_rhs)
end // end of [fprint_e0qn]

fun
fprint_e0qnlst
  (out: FILEref, eqns: list0(e0qn)): void =
(
  case+ eqns of
  | list0_nil () => ()
  | list0_cons (eqn, eqns) => let
      val () = fprint! (out, "  ")
      val () = fprint_e0qn (out, eqn)
      val () = fprint! (out, ";\n")
    in
      fprint_e0qnlst (out, eqns)
    end
) (* end of [fprint_e0qnlst] *)

(* ****** ****** *)

implement
fprint_d0ec
  (out, dec) =
(
case+ dec.d0ec_node of
//
| D0Csensor (ide, s0e) => let
    val () = fprint! (out, "sensor ")
    val () = fprint_symbol (out, ide.i0de_sym)
    val () = fprint! (out, " : ")
    val () = fprint_s0exp (out, s0e)
  in
    fprint! (out, ";\n")
  end
//
| D0Cactuator (ide, s0e) => let
    val () = fprint! (out, "actuator ")
    val () = fprint_symbol (out, ide.i0de_sym)
    val () = fprint! (out, " : ")
    val () = fprint_s0exp (out, s0e)
  in
    fprint! (out, ";\n")
  end
//
| D0Cabstype (ide) => let
    val () = fprint! (out, "abstype ")
    val () = fprint_symbol (out, ide.i0de_sym)
  in
    fprint! (out, ";\n")
  end
//
| D0Ctypedef (ide, fs) => let
    val () = fprint! (out, "typedef ")
    val () = fprint_symbol (out, ide.i0de_sym)
    val () = fprint! (out, " = { ")
    fun fields (fs: list0(@(i0de, s0exp)), fst: bool): void =
      case+ fs of
      | list0_nil () => ()
      | list0_cons (f, fs) => let
          val () = if not (fst) then fprint! (out, ", ")
          val () = fprint_symbol (out, (f.0).i0de_sym)
          val () = fprint! (out, " : ")
          val () = fprint_s0exp (out, f.1)
        in
          fields (fs, false)
        end
    val () = fields (fs, true)
  in
    fprint! (out, " };\n")
  end
//
| D0Cfixity (knd, prec, opers) => let
    val () =
      (case+ knd of
       | FIXleft () => fprint! (out, "infixl ")
       | FIXright () => fprint! (out, "infixr ")
       | FIXpre () => fprint! (out, "prefix "))
    val () = fprint! (out, prec)
    fun loop (out: FILEref, opers: list0(i0de)): void =
      case+ opers of
      | list0_nil () => ()
      | list0_cons (oper, opers) => let
          val () = fprint! (out, " ", symbol_get_name (oper.i0de_sym))
        in
          loop (out, opers)
        end
    val () = loop (out, opers)
  in
    fprint! (out, ";\n")
  end
//
| D0Cnode (nd) => let
    val () =
      if nd.n0de_extern then fprint! (out, "extern ")
    val () = fprint! (out, "node ")
    fun quas
      (out: FILEref, qs: list0(q0ua), pre: bool): void =
      case+ qs of
      | list0_nil () => ()
      | list0_cons (qua, qs) => let
          val () = if not (pre) then fprint! (out, " ")
          val () = fprint_q0ua (out, qua)
          val () = if pre then fprint! (out, " ")
        in
          quas (out, qs, pre)
        end
    val () = quas (out, nd.n0de_prequa, true)
    val () = fprint_symbol (out, nd.n0de_name.i0de_sym)
    val () = quas (out, nd.n0de_postqua, false)
    val () = fprint! (out, "\n  (")
    val () = fprint_p0aramlst (out, nd.n0de_params)
    val () = fprint! (out, ")\n  returns (")
    val () = fprint_p0aramlst (out, nd.n0de_returns)
    val () = fprint! (out, ")\n")
    fun vars
      (out: FILEref, ps: list0(p0aram)): void =
      case+ ps of
      | list0_nil () => ()
      | list0_cons (p, ps) => let
          val () = fprint! (out, "  var ")
          val () = fprint_symbol (out, p.p0aram_sym)
          val () = fprint! (out, " : ")
          val () = fprint_s0exp (out, p.p0aram_typ)
          val () = fprint! (out, ";\n")
        in
          vars (out, ps)
        end
    val () = vars (out, nd.n0de_vars)
    val () =
      (case+ nd.n0de_eqns of
       | Some0 (eqns) => let
           val () = fprint! (out, "let\n")
           val () = fprint_e0qnlst (out, eqns)
         in
           fprint! (out, "tel\n")
         end
       | None0 () => ())
  in
    fprint! (out, "\n")
  end
//
| D0Ceqn (eqn) => let
    val () = fprint_e0qn (out, eqn)
  in
    fprint! (out, ";\n")
  end
//
) (* end of [fprint_d0ec] *)

implement
fprint_d0eclst
  (out, decs) =
(
  case+ decs of
  | list0_nil () => ()
  | list0_cons (dec, decs) => let
      val () = fprint_d0ec (out, dec)
    in
      fprint_d0eclst (out, decs)
    end
) (* end of [fprint_d0eclst] *)

(* ****** ****** *)

(* end of [overture_syntax.dats] *)
