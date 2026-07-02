(*
** Overture: elaboration and binding (trans1 + trans2 collapsed)
*)

#include "share/atspre_staload.hats"

staload "libats/ML/SATS/basis.sats"
staload "libats/ML/SATS/list0.sats"
staload "libats/ML/SATS/string.sats"
staload _ = "libats/ML/DATS/list0.dats"
staload _ = "libats/ML/DATS/string.dats"

staload "./overture_symbol.sats"
staload "./overture_location.sats"
staload "./overture_error.sats"
staload "./overture_syntax.sats"
staload "./overture_staexp.sats"
staload "./overture_trans12.sats"

(* ****** ****** *)

datatype
d2var_ = D2VAR of (symbol, t2ype, int(*stamp*))

assume d2var = d2var_

implement
d2var_make (sym, t2p) =
  D2VAR (sym, t2p, stamp_new ())

implement
d2var_get_sym (d2v) =
  let val D2VAR (sym, _, _) = d2v in sym end
implement
d2var_get_type (d2v) =
  let val D2VAR (_, t2p, _) = d2v in t2p end
implement
d2var_get_stamp (d2v) =
  let val D2VAR (_, _, stamp) = d2v in stamp end

implement
eq_d2var_d2var (d2v1, d2v2) =
  d2var_get_stamp (d2v1) = d2var_get_stamp (d2v2)

implement
fprint_d2var (out, d2v) =
  fprint!
    (out, symbol_get_name (d2var_get_sym (d2v)), "$", d2var_get_stamp (d2v))
(* end of [fprint_d2var] *)

(* ****** ****** *)

implement
d2exp_make (loc, node) =
  '{ d2e_loc= loc, d2e_node= node }

implement
fprint_d2exp
  (out, d2e) =
(
case+ d2e.d2e_node of
| D2Eint (i) => fprint! (out, i)
| D2Erat (a, b) => fprint! (out, a, "/", b)
| D2Evar (d2v) => fprint_d2var (out, d2v)
| D2Eapp (nsig, args) => let
    val () = fprint_symbol (out, nsig.n2sig_name)
    val () = fprint! (out, "(")
    fun loop
      (out: FILEref, args: d2explst): void =
      case+ args of
      | list0_nil () => ()
      | list0_cons (arg, args) => let
          val () = fprint_d2exp (out, arg)
          val () =
            (case+ args of
             | list0_nil () => ()
             | list0_cons _ => fprint! (out, ", "))
        in
          loop (out, args)
        end
    val () = loop (out, args)
  in
    fprint! (out, ")")
  end
| D2Eerr () => fprint! (out, "ERR")
) (* end of [fprint_d2exp] *)

(* ****** ****** *)
//
// environments
//
typedef
tenv = '{
  tenv_svars= list0 (s2var)    (* static vars in scope *)
, tenv_flows= list0 (d2var)    (* flow vars in scope *)
, tenv_nodes= list0 (n2odesig) (* user/extern nodes in scope *)
} (* end of [tenv] *)

fn tenv_nil (): tenv = '{
  tenv_svars= list0_nil ()
, tenv_flows= list0_nil ()
, tenv_nodes= list0_nil ()
} (* end of [tenv_nil] *)

fn tenv_add_svars
  (env: tenv, s2vs: list0(s2var)): tenv = '{
  tenv_svars= list0_append (s2vs, env.tenv_svars)
, tenv_flows= env.tenv_flows
, tenv_nodes= env.tenv_nodes
} (* end of [tenv_add_svars] *)

fn tenv_set_flows
  (env: tenv, d2vs: list0(d2var)): tenv = '{
  tenv_svars= env.tenv_svars
, tenv_flows= d2vs
, tenv_nodes= env.tenv_nodes
} (* end of [tenv_set_flows] *)

fn tenv_add_node
  (env: tenv, nsig: n2odesig): tenv = '{
  tenv_svars= env.tenv_svars
, tenv_flows= env.tenv_flows
, tenv_nodes= list0_cons (nsig, env.tenv_nodes)
} (* end of [tenv_add_node] *)

(* ****** ****** *)

fn svar_find
  (env: tenv, sym: symbol): option0 (s2var) = let
  fun loop
    (s2vs: list0(s2var)): option0 (s2var) =
    case+ s2vs of
    | list0_nil () => None0 ()
    | list0_cons (s2v, s2vs) =>
        if s2var_get_sym (s2v) = sym then Some0 (s2v) else loop (s2vs)
in
  loop (env.tenv_svars)
end // end of [svar_find]

fn flow_find
  (env: tenv, sym: symbol): option0 (d2var) = let
  fun loop
    (d2vs: list0(d2var)): option0 (d2var) =
    case+ d2vs of
    | list0_nil () => None0 ()
    | list0_cons (d2v, d2vs) =>
        if d2var_get_sym (d2v) = sym then Some0 (d2v) else loop (d2vs)
in
  loop (env.tenv_flows)
end // end of [flow_find]

fn node_find
  (env: tenv, sym: symbol): option0 (n2odesig) = let
  fun loop
    (nsigs: list0(n2odesig)): option0 (n2odesig) =
    case+ nsigs of
    | list0_nil () => None0 ()
    | list0_cons (nsig, nsigs) =>
        if nsig.n2sig_name = sym then Some0 (nsig) else loop (nsigs)
in
  loop (env.tenv_nodes)
end // end of [node_find]

(* ****** ****** *)

fn s2exp_err (loc: location): s2exp =
  s2exp_make (loc, S2RTerr (), S2Eint (0))

(* ****** ****** *)
//
// sort checking of static expressions
//
extern fun s0exp_tr (env: tenv, s0e: s0exp): s2exp
extern fun s0exp_tr_srt
  (env: tenv, s0e: s0exp, srt: s2rt, what: string): s2exp

fun
s0explst_tr
  (env: tenv, s0es: s0explst): s2explst =
(
  case+ s0es of
  | list0_nil () => list0_nil ()
  | list0_cons (s0e, s0es) =>
      list0_cons (s0exp_tr (env, s0e), s0explst_tr (env, s0es))
) (* end of [s0explst_tr] *)

fun
s2explst_srts
  (s2es: s2explst): list0 (s2rt) =
(
  case+ s2es of
  | list0_nil () => list0_nil ()
  | list0_cons (s2e, s2es) =>
      list0_cons (s2e.s2e_srt, s2explst_srts (s2es))
) (* end of [s2explst_srts] *)

(*
** apply a static constant by name with overload resolution;
** constant-folds rational literals built with [/]
*)
fn s0exp_tr_app
(
  env: tenv, loc: location, opsym: symbol, args: s2explst
) : s2exp = let
//
val srts = s2explst_srts (args)
//
fun anyerr
  (srts: list0(s2rt)): bool =
  case+ srts of
  | list0_nil () => false
  | list0_cons (srt, srts) =>
      (case+ srt of S2RTerr () => true | _ => anyerr (srts))
//
in
//
if anyerr (srts) then s2exp_err (loc)
else (
case+ the_s2csttbl_search (opsym, srts) of
| Some0 (s2c) => (
    (* fold literal rationals: a / b *)
    case+ args of
    | list0_cons (l, list0_cons (r, list0_nil ()))
        when symbol_get_name (opsym) = "/" => (
        case+ (l.s2e_node, r.s2e_node) of
        | (S2Eint (a), S2Eint (b)) =>
            if b = 0 then let
              val () = errmsg (loc, "division by zero in a rational literal")
            in
              s2exp_err (loc)
            end else s2exp_rat (loc, a, b)
        | (_, _) =>
            s2exp_make (loc, s2cst_get_res (s2c), S2Eapp (s2c, args))
      )
    | _ (*rest*) =>
        s2exp_make (loc, s2cst_get_res (s2c), S2Eapp (s2c, args))
  )
| None0 () => let
    val () = errmsg (loc,
      string_append (
        string_append ("no static operation [", symbol_get_name (opsym))
      , "] for the given argument sorts"))
  in
    s2exp_err (loc)
  end
) (* end of [if] *)
//
end // end of [s0exp_tr_app]

implement
s0exp_tr
  (env, s0e) = let
  val loc = s0e.s0e_loc
in
//
case+ s0e.s0e_node of
//
| S0Eint (i) => s2exp_int (loc, i)
//
| S0Eide (sym) => (
    case+ symbol_get_name (sym) of
    | "true" => s2exp_bool (loc, true)
    | "false" => s2exp_bool (loc, false)
    | "strict" =>
        s2exp_make (loc, S2RTkind (), S2Ecst (s2cst_strict ()))
    | "gated" =>
        s2exp_make (loc, S2RTkind (), S2Ecst (s2cst_gated ()))
    | _ (*rest*) => (
        case+ svar_find (env, sym) of
        | Some0 (s2v) => s2exp_var (loc, s2v)
        | None0 () => let
            val () = errmsg (loc,
              string_append ("unbound static identifier: ",
                symbol_get_name (sym)))
          in
            s2exp_err (loc)
          end
      )
  )
//
| S0Eopapp (f, args) =>
    s0exp_tr_app (env, loc, f.i0de_sym, s0explst_tr (env, args))
| S0Eapp (f, args) =>
    s0exp_tr_app (env, loc, f.i0de_sym, s0explst_tr (env, args))
//
| S0Etup (args) => (
    case+ args of
    | list0_cons (n, list0_cons (p, list0_nil ())) => let
        val n2 = s0exp_tr_srt (env, n, S2RTint (), "clock period")
        val p2 = s0exp_tr_srt (env, p, S2RTrat (), "clock phase")
        (* validity: n * p must be a natural number (checked when literal) *)
        val () = (
          case+ (n2.s2e_node, p2.s2e_node) of
          | (S2Eint (n), S2Erat (a, b)) =>
              if (n * a) mod b != 0 then
                errmsg (loc,
                  "invalid clock literal: the activation date is not an integer")
              else if n * a < 0 then
                errmsg (loc, "invalid clock literal: negative activation date")
              else ()
          | (S2Eint (n), S2Eint (a)) =>
              if n * a < 0 then
                errmsg (loc, "invalid clock literal: negative activation date")
              else ()
          | (_, _) => ()
        ) : void // end of [val]
        val clkargs = list0_cons (n2, list0_cons (p2, list0_nil ()))
      in
        s2exp_make (loc, S2RTclock (), S2Eapp (s2cst_clk (), clkargs))
      end
    | _ (*rest*) => let
        val () = errmsg (loc, "a clock literal takes exactly two components")
      in
        s2exp_err (loc)
      end
  )
//
| S0Eexi (qua, _) => let
    val () = errmsg (loc,
      "quantifiers may not appear inside a static expression")
  in
    s2exp_err (loc)
  end
//
end // end of [s0exp_tr]

implement
s0exp_tr_srt
  (env, s0e, srt, what) = let
  val s2e = s0exp_tr (env, s0e)
in
//
case+ s2e.s2e_srt of
| S2RTerr () => s2e (* already reported *)
| _ (*rest*) =>
    if s2rt_leq (s2e.s2e_srt, srt) then s2e
    else let
      val () = fprint! (stderr_ref, "  expected sort: ")
      val () = fprint_s2rt (stderr_ref, srt)
      val () = fprint! (stderr_ref, "; actual sort: ")
      val () = fprint_s2rt (stderr_ref, s2e.s2e_srt)
      val () = fprint! (stderr_ref, "\n")
      val () = errmsg (s0e.s0e_loc,
        string_append ("sort mismatch in ", what))
    in
      s2exp_err (s0e.s0e_loc)
    end
  // end of [if]
//
end // end of [s0exp_tr_srt]

(* ****** ****** *)
//
// sorts and quantifiers
//
fn s0rt_tr
  (srt: s0rt): s2rt = let
  val S0RTide (loc, sym) = srt
  val name = symbol_get_name (sym)
in
  case+ name of
  | "type" => S2RTtype ()
  | "clock" => S2RTclock ()
  | "int" => S2RTint ()
  | "rat" => S2RTrat ()
  | "bool" => S2RTbool ()
  | "kind" => S2RTkind ()
  | _ (*rest*) => let
      val () = errmsg (loc, string_append ("unknown sort: ", name))
    in
      S2RTerr ()
    end
end // end of [s0rt_tr]

(*
** bind quantifier args to fresh static vars;
** [dflt] is the sort used for unannotated args
*)
fn q0args_tr
  (args: list0(q0arg), dflt: s2rt): list0 (s2var) = let
//
fun loop
  (args: list0(q0arg)): list0 (s2var) =
(
  case+ args of
  | list0_nil () => list0_nil ()
  | list0_cons (arg, args) => let
      val srt = (
        case+ arg.q0arg_srt of
        | Some0 (srt) => s0rt_tr (srt)
        | None0 () => dflt
      ) : s2rt
      val s2v = s2var_make (arg.q0arg_sym, srt)
    in
      list0_cons (s2v, loop (args))
    end
) (* end of [loop] *)
//
in
  loop (args)
end // end of [q0args_tr]

(* ****** ****** *)
//
// types
//
extern fun t0ype_tr (env: tenv, s0e: s0exp): t2ype

implement
t0ype_tr
  (env, s0e) = let
  val loc = s0e.s0e_loc
in
//
case+ s0e.s0e_node of
//
| S0Eide (sym) => let
    val name = symbol_get_name (sym)
  in
    case+ name of
    | "int" => T2YPEbase (sym)
    | "bool" => T2YPEbase (sym)
    | _ (*rest*) => (
        case+ svar_find (env, sym) of
        | Some0 (s2v) => (
            case+ s2var_get_srt (s2v) of
            | S2RTtype () => T2YPEvar (s2v)
            | _ (*rest*) => let
                val () = errmsg (loc,
                  string_append ("static variable is not a type: ", name))
              in
                T2YPEerr ()
              end
          )
        | None0 () => let
            val () = errmsg (loc,
              string_append ("unknown type: ", name))
          in
            T2YPEerr ()
          end
      )
  end
//
| S0Eapp (f, args)
    when symbol_get_name (f.i0de_sym) = "rate" => (
    case+ args of
    | list0_cons (elt, list0_cons (clk, list0_nil ())) => let
        (* rate(a, c) is sugar for rate(strict, a, c) *)
        val elt2 = t0ype_tr (env, elt)
        val clk2 = s0exp_tr_srt (env, clk, S2RTclock (), "a rate clock")
        val knd = s2exp_make
          (loc, S2RTkind (), S2Ecst (s2cst_strict ()))
      in
        T2YPErate (knd, elt2, clk2)
      end
    | list0_cons (knd, list0_cons (elt, list0_cons (clk, list0_nil ()))) => let
        val knd2 = s0exp_tr_srt (env, knd, S2RTkind (), "a rate kind")
        val elt2 = t0ype_tr (env, elt)
        val clk2 = s0exp_tr_srt (env, clk, S2RTclock (), "a rate clock")
      in
        T2YPErate (knd2, elt2, clk2)
      end
    | _ (*rest*) => let
        val () = errmsg (loc, "rate(...) takes two or three arguments")
      in
        T2YPEerr ()
      end
  )
//
| S0Eexi (qua, body) => let
    val s2vs = q0args_tr (qua.q0ua_args, S2RTint ())
    val env = tenv_add_svars (env, s2vs)
    val gua = (
      case+ qua.q0ua_guard of
      | Some0 (g) => s0exp_tr_srt (env, g, S2RTbool (), "a quantifier guard")
      | None0 () => s2exp_bool (qua.q0ua_loc, true)
    ) : s2exp // end of [val]
    val body2 = t0ype_tr (env, body)
  in
    T2YPEexi (s2vs, gua, body2)
  end
//
| _ (*rest*) => let
    val () = errmsg (loc, "expected a type")
  in
    T2YPEerr ()
  end
//
end // end of [t0ype_tr]

(* ****** ****** *)
//
// dynamic expressions
//
extern fun d0exp_tr (env: tenv, d0e: d0exp): d2exp

fun
d0explst_tr
  (env: tenv, d0es: d0explst): d2explst =
(
  case+ d0es of
  | list0_nil () => list0_nil ()
  | list0_cons (d0e, d0es) =>
      list0_cons (d0exp_tr (env, d0e), d0explst_tr (env, d0es))
) (* end of [d0explst_tr] *)

fn d2exp_err (loc: location): d2exp =
  d2exp_make (loc, D2Eerr ())

fn apply_node
(
  env: tenv
, loc: location, fsym: symbol, args: d0explst
) : d2exp = let
//
val nsigopt = (
  case+ node_find (env, fsym) of
  | Some0 (nsig) => Some0 (nsig)
  | None0 () => the_builtintbl_search (fsym)
) : option0 (n2odesig) // end of [val]
//
in
//
case+ nsigopt of
| Some0 (nsig) => let
    val args2 = d0explst_tr (env, args)
    val nargs = list0_length (args2)
    val nprms = list0_length (nsig.n2sig_params)
  in
    if nargs != nprms then let
      val () = errmsg (loc,
        string_append ("wrong number of arguments to node: ",
          symbol_get_name (fsym)))
    in
      d2exp_err (loc)
    end else d2exp_make (loc, D2Eapp (nsig, args2))
  end
| None0 () => let
    val () = errmsg (loc,
      string_append ("unknown node or operator: ", symbol_get_name (fsym)))
  in
    d2exp_err (loc)
  end
//
end // end of [apply_node]

implement
d0exp_tr
  (env, d0e) = let
  val loc = d0e.d0e_loc
in
//
case+ d0e.d0e_node of
//
| D0Eint (i) => d2exp_make (loc, D2Eint (i))
//
| D0Eide (sym) => (
    case+ flow_find (env, sym) of
    | Some0 (d2v) => d2exp_make (loc, D2Evar (d2v))
    | None0 () => let
        val () = errmsg (loc,
          string_append ("unbound flow: ", symbol_get_name (sym)))
      in
        d2exp_err (loc)
      end
  )
//
| D0Eopapp (f, args) => (
    (* constant rationals: a / b at the term level *)
    case+ args of
    | list0_cons (l, list0_cons (r, list0_nil ()))
        when symbol_get_name (f.i0de_sym) = "/" => (
        case+ (l.d0e_node, r.d0e_node) of
        | (D0Eint (a), D0Eint (b)) =>
            if b = 0 then let
              val () = errmsg (loc, "division by zero in a rational literal")
            in
              d2exp_err (loc)
            end else d2exp_make (loc, D2Erat (a, b))
        | (_, _) => apply_node (env, loc, f.i0de_sym, args)
      )
    | _ (*rest*) => apply_node (env, loc, f.i0de_sym, args)
  )
//
| D0Eapp (f, args) => apply_node (env, loc, f.i0de_sym, args)
//
| D0Etup (args) => let
    val () = errmsg (loc, "tuple expressions are not supported here")
  in
    d2exp_err (loc)
  end
//
end // end of [d0exp_tr]

(* ****** ****** *)
//
// parameters (params, returns, vars)
//
fun
p0arams_tr
  (env: tenv, ps: list0(p0aram)): list0 (d2var) =
(
  case+ ps of
  | list0_nil () => list0_nil ()
  | list0_cons (p, ps) => let
      val t2p = t0ype_tr (env, p.p0aram_typ)
      val d2v = d2var_make (p.p0aram_sym, t2p)
    in
      list0_cons (d2v, p0arams_tr (env, ps))
    end
) (* end of [p0arams_tr] *)

fun
d2vars_types
  (d2vs: list0(d2var)): t2ypelst =
(
  case+ d2vs of
  | list0_nil () => list0_nil ()
  | list0_cons (d2v, d2vs) =>
      list0_cons (d2var_get_type (d2v), d2vars_types (d2vs))
) (* end of [d2vars_types] *)

(* ****** ****** *)
//
// equations
//
fun
e0qns_tr
(
  env: tenv
, defbls: list0(d2var) (* flows an equation may define *)
, eqns: list0(e0qn)
) : list0 (e2qn) = let
//
fn lhs_find
  (defbls: list0(d2var), ide: i0de): option0 (d2var) = let
  fun loop
    (d2vs: list0(d2var)): option0 (d2var) =
    case+ d2vs of
    | list0_nil () => None0 ()
    | list0_cons (d2v, d2vs) =>
        if d2var_get_sym (d2v) = ide.i0de_sym
          then Some0 (d2v) else loop (d2vs)
        // end of [if]
in
  loop (defbls)
end // end of [lhs_find]
//
fun lhs_tr
  (ides: list0(i0de)): list0 (d2var) =
(
  case+ ides of
  | list0_nil () => list0_nil ()
  | list0_cons (ide, ides) => (
      case+ lhs_find (defbls, ide) of
      | Some0 (d2v) => list0_cons (d2v, lhs_tr (ides))
      | None0 () => let
          val () = errmsg (ide.i0de_loc,
            string_append ("not a definable flow: ",
              symbol_get_name (ide.i0de_sym)))
        in
          lhs_tr (ides)
        end
    )
) (* end of [lhs_tr] *)
//
fun loop
  (eqns: list0(e0qn)): list0 (e2qn) =
(
  case+ eqns of
  | list0_nil () => list0_nil ()
  | list0_cons (eqn, eqns) => let
      val lhs = lhs_tr (eqn.e0qn_lhs)
      val rhs = d0exp_tr (env, eqn.e0qn_rhs)
      val e2 = '{
        e2qn_loc= eqn.e0qn_loc, e2qn_lhs= lhs, e2qn_rhs= rhs
      } : e2qn
    in
      list0_cons (e2, loop (eqns))
    end
) (* end of [loop] *)
//
in
  loop (eqns)
end // end of [e0qns_tr]

(*
** each definable flow must be defined by exactly one equation
*)
fun
check_definedness
  (loc: location, defbls: list0(d2var), eqns: list0(e2qn)): void = let
//
fun count
  (d2v: d2var, eqns: list0(e2qn)): int = let
  fun inlhs
    (d2v: d2var, lhs: list0(d2var)): int =
    case+ lhs of
    | list0_nil () => 0
    | list0_cons (d2w, lhs) =>
        if eq_d2var_d2var (d2v, d2w)
          then 1 + inlhs (d2v, lhs) else inlhs (d2v, lhs)
        // end of [if]
in
  case+ eqns of
  | list0_nil () => 0
  | list0_cons (eqn, eqns) =>
      inlhs (d2v, eqn.e2qn_lhs) + count (d2v, eqns)
end // end of [count]
//
fun loop
  (d2vs: list0(d2var)): void =
(
  case+ d2vs of
  | list0_nil () => ()
  | list0_cons (d2v, d2vs) => let
      val n = count (d2v, eqns)
      val name = symbol_get_name (d2var_get_sym (d2v))
      val () =
        if n = 0 then
          errmsg (loc, string_append ("flow is never defined: ", name))
      val () =
        if n > 1 then
          errmsg (loc, string_append ("flow is defined more than once: ", name))
    in
      loop (d2vs)
    end
) (* end of [loop] *)
//
in
  loop (defbls)
end // end of [check_definedness]

(* ****** ****** *)
//
// nodes
//
fn n0de_tr
  (env: tenv, nd: n0de): n2ode = let
//
// quantifier groups: position is stylistic (pre-name groups default
// to sort [type], post-name to [int]); variables classify by sort
// (type -> payload tvars, everything else -> static uvars), and the
// guards of every group are conjoined
//
typedef grp = @(list0(s2var), option0(s0exp))
//
fun bindgrps
  (qs: list0(q0ua), dflt: s2rt): list0 (grp) =
(
  case+ qs of
  | list0_nil () => list0_nil ()
  | list0_cons (qua, qs) =>
      list0_cons
        (@(q0args_tr (qua.q0ua_args, dflt), qua.q0ua_guard),
         bindgrps (qs, dflt))
) (* end of [bindgrps] *)
//
val grps =
  list0_append
    (bindgrps (nd.n0de_prequa, S2RTtype ()),
     bindgrps (nd.n0de_postqua, S2RTint ()))
//
fun allvars (grps: list0(grp)): list0 (s2var) =
  case+ grps of
  | list0_nil () => list0_nil ()
  | list0_cons (g, grps) => list0_append (g.0, allvars (grps))
val svars = allvars (grps)
//
fun keepsort
  (s2vs: list0(s2var), istype: bool): list0 (s2var) =
  case+ s2vs of
  | list0_nil () => list0_nil ()
  | list0_cons (s2v, s2vs) => let
      val ist =
        (case+ s2var_get_srt (s2v) of S2RTtype () => true | _ => false)
    in
      if ist = istype
        then list0_cons (s2v, keepsort (s2vs, istype))
        else keepsort (s2vs, istype)
      // end of [if]
    end
val tvars = keepsort (svars, true)
val uvars = keepsort (svars, false)
//
val env1 = tenv_add_svars (env, svars)
//
val guard = let
  fun conj
    (grps: list0(grp), acc: option0(s2exp)): option0 (s2exp) =
    case+ grps of
    | list0_nil () => acc
    | list0_cons (g, grps) => (
        case+ g.1 of
        | None0 () => conj (grps, acc)
        | Some0 (g0) => let
            val g2 = s0exp_tr_srt (env1, g0, S2RTbool (), "a node guard")
          in
            case+ acc of
            | None0 () => conj (grps, Some0 (g2))
            | Some0 (a) => (
                case+ the_s2csttbl_search
                  (symbol_make ("&&"),
                   list0_cons (S2RTbool (), list0_cons (S2RTbool (), list0_nil ()))) of
                | Some0 (s2c) =>
                    conj (grps, Some0 (s2exp_make
                      (nd.n0de_loc, S2RTbool (),
                       S2Eapp (s2c, list0_cons (a, list0_cons (g2, list0_nil ()))))))
                | None0 () => conj (grps, Some0 (g2))
              )
          end
      )
in
  case+ conj (grps, None0 ()) of
  | Some0 (g) => g
  | None0 () => s2exp_bool (nd.n0de_loc, true)
end // end of [val]
//
val params = p0arams_tr (env1, nd.n0de_params)
val returns = p0arams_tr (env1, nd.n0de_returns)
val vars = p0arams_tr (env1, nd.n0de_vars)
//
val restypes = d2vars_types (returns)
val res = (
  case+ restypes of
  | list0_cons (t2p, list0_nil ()) => t2p
  | _ (*rest*) => T2YPEtup (restypes)
) : t2ype // end of [val]
//
val wcet = (
  case+ nd.n0de_wcet of
  | Some0 (w) =>
      if nd.n0de_extern
        then Some0 (s0exp_tr_srt (env1, w, S2RTint (), "a wcet declaration"))
        else let
          val () = errmsg (nd.n0de_loc,
            "wcet may only be declared on extern nodes")
        in
          None0 ()
        end
      // end of [if]
  | None0 () => None0 ()
) : option0 (s2exp) // end of [val]
//
val nsig = '{
  n2sig_name= nd.n0de_name.i0de_sym
, n2sig_tvars= tvars
, n2sig_uvars= uvars
, n2sig_guard= guard
, n2sig_params= d2vars_types (params)
, n2sig_res= res
, n2sig_wcet= wcet
} : n2odesig // end of [val]
//
val eqns = (
  case+ nd.n0de_eqns of
  | Some0 (eqns0) => let
      val defbls = list0_append (returns, vars)
      val flows =
        list0_append (params, defbls)
      val env2 = tenv_set_flows (env1, flows)
      val eqns = e0qns_tr (env2, defbls, eqns0)
      val () = check_definedness (nd.n0de_loc, defbls, eqns)
    in
      eqns
    end
  | None0 () => list0_nil ()
) : list0 (e2qn) // end of [val]
//
in '{
  n2ode_loc= nd.n0de_loc
, n2ode_extern= nd.n0de_extern
, n2ode_sig= nsig
, n2ode_params= params
, n2ode_returns= returns
, n2ode_vars= vars
, n2ode_eqns= eqns
} end // end of [n0de_tr]

(* ****** ****** *)

implement
trans12_program
  (decs) = let
//
fun loop
(
  env: tenv
, decs: d0eclst
, sensors: list0(d2var)
, actuators: list0(d2var)
, nodes: list0(n2ode)
, eqns: list0(e2qn)
) : p2rog = (
//
case+ decs of
| list0_nil () => '{
    p2rog_sensors= list0_reverse (sensors)
  , p2rog_actuators= list0_reverse (actuators)
  , p2rog_nodes= list0_reverse (nodes)
  , p2rog_eqns= list0_reverse (eqns)
  } // end of [list0_nil]
//
| list0_cons (dec, decs) => (
  case+ dec.d0ec_node of
  //
  | D0Csensor (ide, typ) => let
      val t2p = t0ype_tr (env, typ)
      val () = (
        case+ t2p of
        | T2YPErate (knd, _, _) => (
            case+ knd.s2e_node of
            | S2Ecst (s2c)
                when eq_s2cst_s2cst (s2c, s2cst_strict ()) => ()
            | _ (*rest*) =>
                errmsg (dec.d0ec_loc, "a sensor must be strictly periodic")
          )
        | T2YPEerr _ => ()
        | _ (*rest*) =>
            errmsg (dec.d0ec_loc, "a sensor must carry a rate type")
      ) : void
      val d2v = d2var_make (ide.i0de_sym, t2p)
      val env = tenv_set_flows (env, list0_cons (d2v, env.tenv_flows))
    in
      loop (env, decs, list0_cons (d2v, sensors), actuators, nodes, eqns)
    end
  //
  | D0Cactuator (ide, typ) => let
      val t2p = t0ype_tr (env, typ)
      val () = (
        case+ t2p of
        | T2YPErate _ => () (* any kind: gated actuation skips absent ticks *)
        | T2YPEerr _ => ()
        | _ (*rest*) =>
            errmsg (dec.d0ec_loc, "an actuator must carry a rate type")
      ) : void
      val d2v = d2var_make (ide.i0de_sym, t2p)
      (* actuators are defined, not read: they do not enter the flow env *)
    in
      loop (env, decs, sensors, list0_cons (d2v, actuators), nodes, eqns)
    end
  //
  | D0Cfixity _ =>
      (* already applied during parsing *)
      loop (env, decs, sensors, actuators, nodes, eqns)
  //
  | D0Cnode (nd) => let
      val n2 = n0de_tr (env, nd)
      val env = tenv_add_node (env, n2.n2ode_sig)
    in
      loop (env, decs, sensors, actuators, list0_cons (n2, nodes), eqns)
    end
  //
  | D0Ceqn (eqn) => let
      (* the LHS of a top-level equation must be a single actuator *)
      fn act_find
        (acts: list0(d2var), sym: symbol): option0 (d2var) = let
        fun aux (acts: list0(d2var)): option0 (d2var) =
          case+ acts of
          | list0_nil () => None0 ()
          | list0_cons (d2v, acts) =>
              if d2var_get_sym (d2v) = sym then Some0 (d2v) else aux (acts)
      in
        aux (acts)
      end // end of [act_find]
      val lhs = (
        case+ eqn.e0qn_lhs of
        | list0_cons (ide, list0_nil ()) => (
            case+ act_find (actuators, ide.i0de_sym) of
            | Some0 (d2v) => list0_cons (d2v, list0_nil ())
            | None0 () => let
                val () = errmsg (ide.i0de_loc,
                  string_append ("not an actuator: ",
                    symbol_get_name (ide.i0de_sym)))
              in
                list0_nil ()
              end
          )
        | _ (*rest*) => let
            val () = errmsg (eqn.e0qn_loc,
              "a top-level equation defines exactly one actuator")
          in
            list0_nil ()
          end
      ) : list0 (d2var) // end of [val]
      val rhs = d0exp_tr (env, eqn.e0qn_rhs)
      val e2 = '{
        e2qn_loc= eqn.e0qn_loc, e2qn_lhs= lhs, e2qn_rhs= rhs
      } : e2qn
    in
      loop (env, decs, sensors, actuators, nodes, list0_cons (e2, eqns))
    end
  //
  ) (* end of [list0_cons] *)
//
) (* end of [loop] *)
//
val prog =
  loop
  ( tenv_nil ()
  , decs
  , list0_nil (), list0_nil (), list0_nil (), list0_nil ()
  ) (* end of [val] *)
//
// each actuator must be driven by exactly one top-level equation
//
val () = check_definedness
  (location_dummy (), prog.p2rog_actuators, prog.p2rog_eqns)
//
in
  prog
end // end of [trans12_program]

(* ****** ****** *)

fun
fprint_e2qn
  (out: FILEref, eqn: e2qn): void = let
//
val () = fprint! (out, "(")
fun lhs
  (out: FILEref, d2vs: list0(d2var)): void =
  case+ d2vs of
  | list0_nil () => ()
  | list0_cons (d2v, d2vs) => let
      val () = fprint_d2var (out, d2v)
      val () =
        (case+ d2vs of
         | list0_nil () => ()
         | list0_cons _ => fprint! (out, ", "))
    in
      lhs (out, d2vs)
    end
val () = lhs (out, eqn.e2qn_lhs)
val () = fprint! (out, ") = ")
//
in
  fprint_d2exp (out, eqn.e2qn_rhs)
end // end of [fprint_e2qn]

fun
fprint_d2varlst_types
  (out: FILEref, pfx: string, d2vs: list0(d2var)): void =
(
  case+ d2vs of
  | list0_nil () => ()
  | list0_cons (d2v, d2vs) => let
      val () = fprint! (out, pfx)
      val () = fprint_d2var (out, d2v)
      val () = fprint! (out, " : ")
      val () = fprint_t2ype (out, d2var_get_type (d2v))
      val () = fprint! (out, "\n")
    in
      fprint_d2varlst_types (out, pfx, d2vs)
    end
) (* end of [fprint_d2varlst_types] *)

implement
fprint_p2rog
  (out, prog) = let
//
val () = fprint_d2varlst_types (out, "sensor ", prog.p2rog_sensors)
val () = fprint_d2varlst_types (out, "actuator ", prog.p2rog_actuators)
//
fun nodes
  (out: FILEref, n2s: list0(n2ode)): void =
(
  case+ n2s of
  | list0_nil () => ()
  | list0_cons (n2, n2s) => let
      val () =
        if n2.n2ode_extern then fprint! (out, "extern ")
      val () = fprint! (out, "node ")
      val () = fprint_n2odesig (out, n2.n2ode_sig)
      val () = fprint! (out, "\n")
      val () = fprint_d2varlst_types (out, "  param ", n2.n2ode_params)
      val () = fprint_d2varlst_types (out, "  return ", n2.n2ode_returns)
      val () = fprint_d2varlst_types (out, "  var ", n2.n2ode_vars)
      fun eqs (out: FILEref, eqns: list0(e2qn)): void =
        case+ eqns of
        | list0_nil () => ()
        | list0_cons (eqn, eqns) => let
            val () = fprint! (out, "  ")
            val () = fprint_e2qn (out, eqn)
            val () = fprint! (out, ";\n")
          in
            eqs (out, eqns)
          end
      val () = eqs (out, n2.n2ode_eqns)
    in
      nodes (out, n2s)
    end
) (* end of [nodes] *)
val () = nodes (out, prog.p2rog_nodes)
//
fun eqs (out: FILEref, eqns: list0(e2qn)): void =
  case+ eqns of
  | list0_nil () => ()
  | list0_cons (eqn, eqns) => let
      val () = fprint_e2qn (out, eqn)
      val () = fprint! (out, ";\n")
    in
      eqs (out, eqns)
    end
val () = eqs (out, prog.p2rog_eqns)
//
in
  // nothing
end // end of [fprint_p2rog]

(* ****** ****** *)

(* end of [overture_trans12.dats] *)
