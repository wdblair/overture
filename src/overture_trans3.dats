(*
** Overture: typechecking and constraint generation (trans3)
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
staload "./overture_staexp.sats"
staload "./overture_trans12.sats"
staload "./overture_constraint.sats"
staload "./overture_trans3.sats"

(* ****** ****** *)

fn str3
  (s1: string, s2: string, s3: string): string =
  string_append (string_append (s1, s2), s3)

(* ****** ****** *)
//
// payload type substitution
//
typedef tysub = list0 (@(s2var, t2ype))

fun
t2ype_tysubst
  (t2p: t2ype, sub: tysub): t2ype = let
//
fun assoc
  (sub: tysub, s2v: s2var): option0 (t2ype) =
(
  case+ sub of
  | list0_nil () => None0 ()
  | list0_cons (kx, sub) =>
      if eq_s2var_s2var (kx.0, s2v)
        then Some0 (kx.1) else assoc (sub, s2v)
      // end of [if]
) (* end of [assoc] *)
//
in
//
case+ t2p of
| T2YPEbase _ => t2p
| T2YPEsta _ => t2p
| T2YPEvar (s2v) => (
    case+ assoc (sub, s2v) of
    | Some0 (t2q) => t2q | None0 () => t2p
  )
| T2YPErate (knd, elt, clk) =>
    T2YPErate (knd, t2ype_tysubst (elt, sub), clk)
| T2YPEexi (s2vs, gua, body) =>
    T2YPEexi (s2vs, gua, t2ype_tysubst (body, sub))
| T2YPEtup (t2ps) => let
    fun loop (t2ps: t2ypelst): t2ypelst =
      case+ t2ps of
      | list0_nil () => list0_nil ()
      | list0_cons (t2p, t2ps) =>
          list0_cons (t2ype_tysubst (t2p, sub), loop (t2ps))
  in
    T2YPEtup (loop (t2ps))
  end
| T2YPEerr () => t2p
//
end // end of [t2ype_tysubst]

(* ****** ****** *)
//
// equality goals
//
fn mk_eq_goal
  (loc: location, a: s2exp, b: s2exp): option0 (s2exp) = let
//
fn iserr (s2e: s2exp): bool =
  case+ s2e.s2e_srt of S2RTerr () => true | _ => false
//
in
//
if iserr (a) orelse iserr (b) then None0 ()
else let
  val srts =
    list0_cons (a.s2e_srt, list0_cons (b.s2e_srt, list0_nil ()))
  // end of [val]
in
  case+ the_s2csttbl_search (symbol_make ("=="), srts) of
  | Some0 (s2c) => Some0 (
      s2exp_make (loc, S2RTbool (),
        S2Eapp (s2c, list0_cons (a, list0_cons (b, list0_nil ()))))
    )
  | None0 () => None0 () (* sorts incomparable: reported elsewhere *)
end // end of [if]
//
end // end of [mk_eq_goal]

fn emit_eq
(
  acc: c3acc, loc: location, msg: string, a: s2exp, b: s2exp
) : void =
(
  case+ mk_eq_goal (loc, a, b) of
  | Some0 (g) => c3acc_prop (acc, loc, msg, g)
  | None0 () => ()
) (* end of [emit_eq] *)

(* ****** ****** *)
//
// existential opening: fresh vars, hypotheses into the accumulator
//
fun
open_exi
  (acc: c3acc, loc: location, t2p: t2ype): t2ype =
(
case+ t2p of
| T2YPEexi (s2vs, gua, body) => let
    fun freshen
      (s2vs: list0(s2var)): @(list0(s2var), s2sub) =
    (
      case+ s2vs of
      | list0_nil () => @(list0_nil (), list0_nil ())
      | list0_cons (s2v, s2vs) => let
          val s2w =
            s2var_make (s2var_get_sym (s2v), s2var_get_srt (s2v))
          val r = freshen (s2vs)
          val sub1 = @(s2v, s2exp_var (loc, s2w))
        in
          @(list0_cons (s2w, r.0), list0_cons (sub1, r.1))
        end
    ) (* end of [freshen] *)
    val r = freshen (s2vs)
    fun emit
      (acc: c3acc, s2ws: list0(s2var)): void =
      case+ s2ws of
      | list0_nil () => ()
      | list0_cons (s2w, s2ws) =>
          (c3acc_svar (acc, s2w); emit (acc, s2ws))
    val () = emit (acc, r.0)
    val () = c3acc_hypo (acc, loc, s2exp_subst (gua, r.1))
  in
    open_exi (acc, loc, t2ype_subst (body, r.1))
  end
| T2YPEtup (t2ps) => let
    fun loop (t2ps: t2ypelst): t2ypelst =
      case+ t2ps of
      | list0_nil () => list0_nil ()
      | list0_cons (t2p, t2ps) =>
          list0_cons (open_exi (acc, loc, t2p), loop (t2ps))
  in
    T2YPEtup (loop (t2ps))
  end
| _ (*rest*) => t2p
) (* end of [open_exi] *)

(* ****** ****** *)
//
// structural matching: [pat] may contain flexible variables (the
// signature's universals or the exi vars of a declared return type);
// a bare unbound flexible is bound, anything else becomes an
// equality obligation
//
typedef mstate = '{
  ms_flex= list0 (s2var)  (* bindable static vars *)
, ms_usub= ref (s2sub)    (* accumulated static bindings *)
, ms_tysub= ref (tysub)   (* accumulated payload bindings *)
} (* end of [mstate] *)

fn mstate_new
  (flex: list0(s2var)): mstate = '{
  ms_flex= flex
, ms_usub= ref<s2sub> (list0_nil ())
, ms_tysub= ref<tysub> (list0_nil ())
} (* end of [mstate_new] *)

fn usub_find
  (sub: s2sub, s2v: s2var): option0 (s2exp) = let
  fun loop (sub: s2sub): option0 (s2exp) =
    case+ sub of
    | list0_nil () => None0 ()
    | list0_cons (kx, sub) =>
        if eq_s2var_s2var (kx.0, s2v) then Some0 (kx.1) else loop (sub)
in
  loop (sub)
end // end of [usub_find]

fn tysub_find
  (sub: tysub, s2v: s2var): option0 (t2ype) = let
  fun loop (sub: tysub): option0 (t2ype) =
    case+ sub of
    | list0_nil () => None0 ()
    | list0_cons (kx, sub) =>
        if eq_s2var_s2var (kx.0, s2v) then Some0 (kx.1) else loop (sub)
in
  loop (sub)
end // end of [tysub_find]

fn is_flex
  (ms: mstate, s2v: s2var): bool = let
  fun loop (s2vs: list0(s2var)): bool =
    case+ s2vs of
    | list0_nil () => false
    | list0_cons (s2w, s2vs) =>
        if eq_s2var_s2var (s2v, s2w) then true else loop (s2vs)
in
  loop (ms.ms_flex)
end // end of [is_flex]

(*
** singleton types collapse to their base for payload binding
*)
fn t2ype_unsingleton
  (t2p: t2ype): t2ype =
(
  case+ t2p of
  | T2YPEsta (s2e) => (
      case+ s2e.s2e_srt of
      | S2RTint () => T2YPEbase (symbol_make ("int"))
      | _ (*rest*) => t2p
    )
  | _ (*rest*) => t2p
) (* end of [t2ype_unsingleton] *)

fn payload_eq
  (a: t2ype, b: t2ype): bool =
(
  case+ (a, b) of
  | (T2YPEbase (s1), T2YPEbase (s2)) => s1 = s2
  | (T2YPEvar (v1), T2YPEvar (v2)) => eq_s2var_s2var (v1, v2)
  | (T2YPEerr (), _) => true
  | (_, T2YPEerr ()) => true
  | (_, _) => false
) (* end of [payload_eq] *)

(*
** bind a flexible static var to an actual static expression, with
** int-to-rat promotion of literals
*)
fn ms_bind_svar
(
  ms: mstate, loc: location, s2v: s2var, act: s2exp
) : void = let
  val act = (
    case+ (s2var_get_srt (s2v), act.s2e_node) of
    | (S2RTrat (), S2Eint (i)) => s2exp_rat (loc, i, 1)
    | (_, _) => act
  ) : s2exp
in
  !(ms.ms_usub) := list0_cons (@(s2v, act), !(ms.ms_usub))
end // end of [ms_bind_svar]

fun
match_s2exp
(
  acc: c3acc, ms: mstate
, loc: location, msg: string
, pat: s2exp, act: s2exp
) : void = let
  val pat1 = s2exp_subst (pat, !(ms.ms_usub))
//
(*
** kind indices (strict | gated) are compared structurally: kinds
** have no arithmetic, so syntactic comparison is complete, and they
** must never become solver goals
*)
fn kind_mismatch (): void =
  errmsg (loc, str3 ("flow kind mismatch (strict vs gated) in ", msg, ""))
//
in
//
case+ pat1.s2e_node of
| S2Evar (s2v)
    when is_flex (ms, s2v) => (
    case+ usub_find (!(ms.ms_usub), s2v) of
    | Some0 (s2e) => (
        case+ pat1.s2e_srt of
        | S2RTkind () => (
            case+ (s2e.s2e_node, act.s2e_node) of
            | (S2Ecst (c1), S2Ecst (c2)) =>
                if eq_s2cst_s2cst (c1, c2) then () else kind_mismatch ()
            | (S2Evar (v1), S2Evar (v2)) =>
                if eq_s2var_s2var (v1, v2) then () else kind_mismatch ()
            | (_, _) => kind_mismatch ()
          )
        | _ (*rest*) => emit_eq (acc, loc, msg, s2e, act)
      )
    | None0 () => ms_bind_svar (ms, loc, s2v, act)
  )
| _ (*rest*) => (
    case+ pat1.s2e_srt of
    | S2RTkind () => (
        case+ (pat1.s2e_node, act.s2e_node) of
        | (S2Ecst (c1), S2Ecst (c2)) =>
            if eq_s2cst_s2cst (c1, c2) then () else kind_mismatch ()
        | (S2Evar (v1), S2Evar (v2)) =>
            if eq_s2var_s2var (v1, v2) then () else kind_mismatch ()
        | (_, _) => kind_mismatch ()
      )
    | _ (*rest*) => emit_eq (acc, loc, msg, pat1, act)
  )
//
end // end of [match_s2exp]

fun
match_t2ype
(
  acc: c3acc, ms: mstate
, loc: location, msg: string
, pat: t2ype, act: t2ype
) : void =
(
case+ (pat, act) of
//
| (T2YPEerr (), _) => ()
| (_, T2YPEerr ()) => ()
//
| (T2YPEvar (tv), _)
    when is_flex (ms, tv) => (
    case+ tysub_find (!(ms.ms_tysub), tv) of
    | Some0 (t2p) =>
        if payload_eq (t2p, t2ype_unsingleton (act)) then ()
        else errmsg (loc, str3 ("payload type mismatch in ", msg, ""))
      // end of [Some0]
    | None0 () =>
        !(ms.ms_tysub) :=
          list0_cons (@(tv, t2ype_unsingleton (act)), !(ms.ms_tysub))
      // end of [None0]
  )
//
| (T2YPEsta (pe), T2YPEsta (ae)) =>
    match_s2exp (acc, ms, loc, msg, pe, ae)
//
| (T2YPEbase (s), T2YPEsta (ae)) => (
    (* an integer literal can stand for an int payload value *)
    case+ ae.s2e_srt of
    | S2RTint () when symbol_get_name (s) = "int" => ()
    | _ (*rest*) =>
        errmsg (loc, str3 ("payload type mismatch in ", msg, ""))
  )
//
| (T2YPErate (pk, pe, pc), T2YPErate (ak, ae, ac)) => let
    val () = match_s2exp (acc, ms, loc, msg, pk, ak)
    val () = match_t2ype (acc, ms, loc, msg, pe, ae)
  in
    match_s2exp (acc, ms, loc, msg, pc, ac)
  end
//
| (T2YPEtup (ps), T2YPEtup (ats)) => let
    fun loop
      (ps: t2ypelst, ats: t2ypelst): void =
      case+ (ps, ats) of
      | (list0_nil (), list0_nil ()) => ()
      | (list0_cons (p, ps), list0_cons (a, ats)) => let
          val () = match_t2ype (acc, ms, loc, msg, p, a)
        in
          loop (ps, ats)
        end
      | (_, _) =>
          errmsg (loc, str3 ("arity mismatch in ", msg, ""))
  in
    loop (ps, ats)
  end
//
| (pat, act) =>
    if payload_eq (pat, act) then ()
    else errmsg (loc, str3 ("type mismatch in ", msg, ""))
  // end of [if]
//
) (* end of [match_t2ype] *)

(*
** after matching, every flexible var must have been bound
*)
fn ms_check_complete
  (ms: mstate, loc: location, what: string): bool = let
//
fun loop
  (s2vs: list0(s2var), ok: bool): bool =
(
  case+ s2vs of
  | list0_nil () => ok
  | list0_cons (s2v, s2vs) => (
      case+ s2var_get_srt (s2v) of
      | S2RTtype () => (
          case+ tysub_find (!(ms.ms_tysub), s2v) of
          | Some0 _ => loop (s2vs, ok)
          | None0 () => let
              val () = errmsg (loc,
                str3 ("cannot infer type argument [",
                  symbol_get_name (s2var_get_sym (s2v)),
                  str3 ("] in ", what, "")))
            in
              loop (s2vs, false)
            end
        )
      | _ (*rest*) => (
          case+ usub_find (!(ms.ms_usub), s2v) of
          | Some0 _ => loop (s2vs, ok)
          | None0 () => let
              val () = errmsg (loc,
                str3 ("cannot infer static argument [",
                  symbol_get_name (s2var_get_sym (s2v)),
                  str3 ("] in ", what, "")))
            in
              loop (s2vs, false)
            end
        )
    )
) (* end of [loop] *)
//
in
  loop (ms.ms_flex, true)
end // end of [ms_check_complete]

(* ****** ****** *)
//
// synthesis of dynamic expressions
//
extern fun d2exp_syn (acc: c3acc, d2e: d2exp): t2ype

implement
d2exp_syn
  (acc, d2e) = let
  val loc = d2e.d2e_loc
in
//
case+ d2e.d2e_node of
//
| D2Eint (i) => T2YPEsta (s2exp_int (loc, i))
| D2Erat (a, b) => T2YPEsta (s2exp_rat (loc, a, b))
| D2Evar (d2v) => d2var_get_type (d2v)
| D2Eerr () => T2YPEerr ()
//
| D2Eapp (nsig, args) => let
    val name = symbol_get_name (nsig.n2sig_name)
    val what = str3 ("the application of [", name, "]")
  //
    fun synargs
      (acc: c3acc, args: d2explst): t2ypelst =
      case+ args of
      | list0_nil () => list0_nil ()
      | list0_cons (arg, args) => let
          val t2p = d2exp_syn (acc, arg)
          val t2p = open_exi (acc, arg.d2e_loc, t2p)
        in
          list0_cons (t2p, synargs (acc, args))
        end
    val argts = synargs (acc, args)
  //
    val flex =
      list0_append (nsig.n2sig_tvars, nsig.n2sig_uvars)
    val ms = mstate_new (flex)
  //
    fun mtch
      (ps: t2ypelst, ats: t2ypelst): void =
      case+ (ps, ats) of
      | (list0_cons (p, ps), list0_cons (a, ats)) => let
          val () = match_t2ype (acc, ms, loc, what, p, a)
        in
          mtch (ps, ats)
        end
      | (_, _) => () (* arity checked in trans12 *)
    val () = mtch (nsig.n2sig_params, argts)
  //
    val ok = ms_check_complete (ms, loc, what)
  //
  in
    if ok then let
      val usub = !(ms.ms_usub)
      val gua = s2exp_subst (nsig.n2sig_guard, usub)
      val () = c3acc_prop
        (acc, loc, str3 ("guard of [", name, "]"), gua)
      val res = t2ype_tysubst
        (t2ype_subst (nsig.n2sig_res, usub), !(ms.ms_tysub))
    in
      open_exi (acc, loc, res)
    end else T2YPEerr ()
  end
//
end // end of [d2exp_syn]

(* ****** ****** *)
//
// subsumption: does [act] fit the declared type [dec]?
// existential declarations pick their witnesses by matching.
//
fn check_sub
(
  acc: c3acc
, loc: location, msg: string
, act: t2ype, dec: t2ype
) : void =
(
case+ dec of
| T2YPEexi (s2vs, gua, body) => let
    val ms = mstate_new (s2vs)
    val () = match_t2ype (acc, ms, loc, msg, body, act)
    val ok = ms_check_complete (ms, loc, msg)
  in
    if ok then
      c3acc_prop (acc, loc,
        str3 ("existential witness for ", msg, ""),
        s2exp_subst (gua, !(ms.ms_usub)))
    // end of [if]
  end
| _ (*rest*) => let
    val ms = mstate_new (list0_nil ())
  in
    match_t2ype (acc, ms, loc, msg, dec, act)
  end
) (* end of [check_sub] *)

(* ****** ****** *)
//
// equations
//
fn e2qn_check
  (acc: c3acc, eqn: e2qn): void = let
//
val loc = eqn.e2qn_loc
val act = d2exp_syn (acc, eqn.e2qn_rhs)
val act = open_exi (acc, loc, act)
//
in
//
case+ eqn.e2qn_lhs of
| list0_nil () => () (* lhs errors reported in trans12 *)
| list0_cons (d2v, list0_nil ()) => let
    val msg = str3 ("the equation for [",
      symbol_get_name (d2var_get_sym (d2v)), "]")
  in
    check_sub (acc, loc, msg, act, d2var_get_type (d2v))
  end
| lhs (*multiple*) => (
    case+ act of
    | T2YPEtup (ats) => let
        fun loop
          (lhs: list0(d2var), ats: t2ypelst): void =
          case+ (lhs, ats) of
          | (list0_cons (d2v, lhs), list0_cons (a, ats)) => let
              val msg = str3 ("the equation for [",
                symbol_get_name (d2var_get_sym (d2v)), "]")
              val () = check_sub
                (acc, loc, msg, a, d2var_get_type (d2v))
            in
              loop (lhs, ats)
            end
          | (list0_nil (), list0_nil ()) => ()
          | (_, _) =>
              errmsg (loc, "arity mismatch in a tuple equation")
      in
        loop (lhs, ats)
      end
    | T2YPEerr () => ()
    | _ (*rest*) =>
        errmsg (loc, "the right-hand side does not return multiple flows")
  )
//
end // end of [e2qn_check]

(* ****** ****** *)
//
// type well-formedness: user-written clock operations carry their
// side conditions (divisibility; integral activation dates)
//
fn mk_app2
  (loc: location, name: string, srts: list0(s2rt), a: s2exp, b: s2exp)
  : option0 (s2exp) =
(
  case+ the_s2csttbl_search (symbol_make (name), srts) of
  | Some0 (s2c) => Some0 (
      s2exp_make (loc, s2cst_get_res (s2c),
        S2Eapp (s2c, list0_cons (a, list0_cons (b, list0_nil ()))))
    )
  | None0 () => None0 ()
) (* end of [mk_app2] *)

fn mk_app1
  (loc: location, name: string, srts: list0(s2rt), a: s2exp)
  : option0 (s2exp) =
(
  case+ the_s2csttbl_search (symbol_make (name), srts) of
  | Some0 (s2c) => Some0 (
      s2exp_make (loc, s2cst_get_res (s2c),
        S2Eapp (s2c, list0_cons (a, list0_nil ())))
    )
  | None0 () => None0 ()
) (* end of [mk_app1] *)

fun
wf_s2exp
  (acc: c3acc, s2e: s2exp): void =
(
case+ s2e.s2e_node of
| S2Eapp (s2c, args) => let
    val name = symbol_get_name (s2cst_get_name (s2c))
    val loc = s2e.s2e_loc
    fun wfargs
      (acc: c3acc, args: s2explst): void =
      case+ args of
      | list0_nil () => ()
      | list0_cons (arg, args) =>
          (wf_s2exp (acc, arg); wfargs (acc, args))
    val () = wfargs (acc, args)
    val ii =
      list0_cons (S2RTint (), list0_cons (S2RTint (), list0_nil ()))
    val rr =
      list0_cons (S2RTrat (), list0_cons (S2RTrat (), list0_nil ()))
    val r1 = list0_cons (S2RTrat (), list0_nil ())
    val c1 = list0_cons (S2RTclock (), list0_nil ())
  in
    case+ args of
    | list0_cons (c, list0_cons (k, list0_nil ())) => (
        case+ name of
        | _ when s2cst_is_over (s2c) => let
            (* clock division: (n, d) -> (n/k, d) *)
            val prd = mk_app1 (loc, "period", c1, c)
          in
            case+ prd of
            | Some0 (prd) => (
                case+ mk_app2 (loc, "|", ii, k, prd) of
                | Some0 (g) => c3acc_prop (acc, loc,
                    str3 ("divisibility for the clock operator [", name, "]"), g)
                | None0 () => ()
              )
            | None0 () => ()
          end
        | _ when s2cst_is_under (s2c) => (
            (* clock multiplication: (n, d) -> (n*k, d); integral for
               any k > 0, no divisibility required *)
            case+ mk_app2 (loc, ">", ii, k, s2exp_int (loc, 0)) of
            | Some0 (g) => c3acc_prop (acc, loc,
                "positivity for the clock operator [*]", g)
            | None0 () => ()
          )
        | _ when s2cst_is_shift (s2c) => let
            val prd = mk_app1 (loc, "period", c1, c)
          in
            case+ prd of
            | Some0 (prd) => (
                case+ mk_app2 (loc, "*", rr, k, prd) of
                | Some0 (prod) => (
                    case+ mk_app1 (loc, "isint", r1, prod) of
                    | Some0 (g) => c3acc_prop (acc, loc,
                        "integrality for the clock operator [shift]", g)
                    | None0 () => ()
                  )
                | None0 () => ()
              )
            | None0 () => ()
          end
        | _ when s2cst_is_clk (s2c) => (
            (* literal clocks were validated in trans12 *)
            case+ (c.s2e_node, k.s2e_node) of
            | (S2Eint _, S2Eint _) => ()
            | (S2Eint _, S2Erat _) => ()
            | (_, _) => (
                case+ mk_app2 (loc, "*", rr, k, c) of
                | Some0 (prod) => (
                    case+ mk_app1 (loc, "isint", r1, prod) of
                    | Some0 (g) => c3acc_prop (acc, loc,
                        "integrality of the clock activation date", g)
                    | None0 () => ()
                  )
                | None0 () => ()
              )
          )
        | _ (*rest*) => ()
      )
    | _ (*rest*) => ()
  end
| _ (*rest*) => ()
) (* end of [wf_s2exp] *)

fun
wf_t2ype
  (acc: c3acc, loc: location, t2p: t2ype): void =
(
case+ t2p of
| T2YPErate (_, elt, clk) => let
    val () = wf_t2ype (acc, loc, elt)
  in
    wf_s2exp (acc, clk)
  end
| T2YPEexi (s2vs, gua, body) => let
    (* obligations under the exi vars and guard, as a nested tree *)
    val sub = c3acc_new ()
    fun emit
      (sub: c3acc, s2vs: list0(s2var)): void =
      case+ s2vs of
      | list0_nil () => ()
      | list0_cons (s2v, s2vs) =>
          (c3acc_svar (sub, s2v); emit (sub, s2vs))
    val () = emit (sub, s2vs)
    val () = c3acc_hypo (sub, loc, gua)
    val () = wf_t2ype (sub, loc, body)
    val items = c3acc_takeout (sub)
  in
    case+ items of
    | list0_nil () => ()
    | list0_cons _ =>
        c3acc_cnstr (acc,
          c3nstr_itmlst (loc, "well-formedness of an existential type", items))
  end
| T2YPEtup (t2ps) => let
    fun loop
      (t2ps: t2ypelst): void =
      case+ t2ps of
      | list0_nil () => ()
      | list0_cons (t2p, t2ps) =>
          (wf_t2ype (acc, loc, t2p); loop (t2ps))
  in
    loop (t2ps)
  end
| _ (*rest*) => ()
) (* end of [wf_t2ype] *)

fun
wf_d2vars
  (acc: c3acc, d2vs: list0(d2var)): void =
(
  case+ d2vs of
  | list0_nil () => ()
  | list0_cons (d2v, d2vs) => let
      val loc = location_dummy ()
      val () = wf_t2ype (acc, loc, d2var_get_type (d2v))
    in
      wf_d2vars (acc, d2vs)
    end
) (* end of [wf_d2vars] *)

(* ****** ****** *)
//
// determinacy of extern existential outputs: an extern node's
// implementation chooses the witness at runtime, so codegen can put
// a number in the task table only if the guard pins it. Semantic
// check: assuming G(q) and G(q') for a fresh copy q', the solver
// must prove q == q' (componentwise for clocks).
//
fun
exi_determinacy
(
  acc: c3acc, loc: location, nodename: string, t2p: t2ype
) : void =
(
case+ t2p of
| T2YPEexi (s2vs, gua, body) => let
    val sub = c3acc_new ()
    fun freshen
      (s2vs: list0(s2var)): @(list0(s2var), s2sub) =
    (
      case+ s2vs of
      | list0_nil () => @(list0_nil (), list0_nil ())
      | list0_cons (s2v, s2vs) => let
          val s2w =
            s2var_make (s2var_get_sym (s2v), s2var_get_srt (s2v))
          val r = freshen (s2vs)
          val sub1 = @(s2v, s2exp_var (loc, s2w))
        in
          @(list0_cons (s2w, r.0), list0_cons (sub1, r.1))
        end
    ) (* end of [freshen] *)
    val r = freshen (s2vs)
    fun emit
      (sub: c3acc, s2vs: list0(s2var)): void =
      case+ s2vs of
      | list0_nil () => ()
      | list0_cons (s2v, s2vs) =>
          (c3acc_svar (sub, s2v); emit (sub, s2vs))
    val () = emit (sub, s2vs)
    val () = emit (sub, r.0)
    val () = c3acc_hypo (sub, loc, gua)
    val () = c3acc_hypo (sub, loc, s2exp_subst (gua, r.1))
    val msg = str3
      ("determinacy of the existential output of extern node [", nodename, "]")
    fun goals
      (sub: c3acc, vs: list0(s2var), ws: list0(s2var)): void =
      case+ (vs, ws) of
      | (list0_cons (v, vs), list0_cons (w, ws)) => let
          val () = emit_eq (sub, loc, msg,
            s2exp_var (loc, v), s2exp_var (loc, w))
        in
          goals (sub, vs, ws)
        end
      | (_, _) => ()
    val () = goals (sub, s2vs, r.0)
    val items = c3acc_takeout (sub)
    val () = c3acc_cnstr (acc, c3nstr_itmlst (loc, msg, items))
  in
    exi_determinacy (acc, loc, nodename, body)
  end
| T2YPEtup (t2ps) => let
    fun loop (t2ps: t2ypelst): void =
      case+ t2ps of
      | list0_nil () => ()
      | list0_cons (t2p, t2ps) =>
          (exi_determinacy (acc, loc, nodename, t2p); loop (t2ps))
  in
    loop (t2ps)
  end
| _ (*rest*) => ()
) (* end of [exi_determinacy] *)

(* ****** ****** *)
//
// wcet: under the node's hypotheses, the declared worst-case
// execution time must fit within the period of every clock in the
// node's interface (the per-flow necessary condition of the
// synchronous hypothesis). Clocks under existential outputs are
// skipped: determinacy already pins them to the universals.
//
fun
t2ype_collect_clocks
  (t2p: t2ype, acc: s2explst): s2explst =
(
case+ t2p of
| T2YPErate (_, elt, clk) =>
    t2ype_collect_clocks (elt, list0_cons (clk, acc))
| T2YPEtup (t2ps) => let
    fun loop (t2ps: t2ypelst, acc: s2explst): s2explst =
      case+ t2ps of
      | list0_nil () => acc
      | list0_cons (t2p, t2ps) =>
          loop (t2ps, t2ype_collect_clocks (t2p, acc))
  in
    loop (t2ps, acc)
  end
| _ (*rest: base/var/sta/exi/err*) => acc
) (* end of [t2ype_collect_clocks] *)

fn wcet_check
(
  acc: c3acc, loc: location
, nodename: string, w: s2exp, d2vs: list0(d2var)
) : void = let
//
fun clocks
  (d2vs: list0(d2var), acc0: s2explst): s2explst =
  case+ d2vs of
  | list0_nil () => acc0
  | list0_cons (d2v, d2vs) =>
      clocks (d2vs, t2ype_collect_clocks (d2var_get_type (d2v), acc0))
val clks = clocks (d2vs, list0_nil ())
//
val c1 = list0_cons (S2RTclock (), list0_nil ())
val ii =
  list0_cons (S2RTint (), list0_cons (S2RTint (), list0_nil ()))
val msg = str3 ("wcet of extern node [", nodename, "]")
//
fun emit
  (clks: s2explst): void =
  case+ clks of
  | list0_nil () => ()
  | list0_cons (clk, clks) => let
      val () = (
        case+ mk_app1 (loc, "period", c1, clk) of
        | Some0 (prd) => (
            case+ mk_app2 (loc, "<=", ii, w, prd) of
            | Some0 (g) => c3acc_prop (acc, loc, msg, g)
            | None0 () => ()
          )
        | None0 () => ()
      ) : void
    in
      emit (clks)
    end
//
in
  emit (clks)
end // end of [wcet_check]

(* ****** ****** *)

fn n2ode_check
  (nd: n2ode): c3nstr = let
//
val acc = c3acc_new ()
val nsig = nd.n2ode_sig
//
fun svars
  (acc: c3acc, s2vs: list0(s2var)): void =
  case+ s2vs of
  | list0_nil () => ()
  | list0_cons (s2v, s2vs) =>
      (c3acc_svar (acc, s2v); svars (acc, s2vs))
val () = svars (acc, nsig.n2sig_uvars)
val () = c3acc_hypo (acc, nd.n2ode_loc, nsig.n2sig_guard)
//
val () = wf_d2vars (acc, nd.n2ode_params)
val () = wf_d2vars (acc, nd.n2ode_returns)
val () = wf_d2vars (acc, nd.n2ode_vars)
//
val () =
  if nd.n2ode_extern then let
    val name = symbol_get_name (nsig.n2sig_name)
    fun loop (d2vs: list0(d2var)): void =
      case+ d2vs of
      | list0_nil () => ()
      | list0_cons (d2v, d2vs) => let
          val () = exi_determinacy
            (acc, nd.n2ode_loc, name, d2var_get_type (d2v))
        in
          loop (d2vs)
        end
    val () = loop (nd.n2ode_returns)
  in
    case+ nsig.n2sig_wcet of
    | Some0 (w) =>
        wcet_check (acc, nd.n2ode_loc, name, w,
          list0_append (nd.n2ode_params, nd.n2ode_returns))
    | None0 () => ()
  end // end of [if]
//
fun eqns
  (acc: c3acc, eqs: list0(e2qn)): void =
  case+ eqs of
  | list0_nil () => ()
  | list0_cons (eqn, eqs) =>
      (e2qn_check (acc, eqn); eqns (acc, eqs))
val () = eqns (acc, nd.n2ode_eqns)
//
val items = c3acc_takeout (acc)
//
in
  c3nstr_itmlst (nd.n2ode_loc,
    str3 ("node [", symbol_get_name (nsig.n2sig_name), "]"), items)
end // end of [n2ode_check]

(* ****** ****** *)

implement
trans3_program
  (prog) = let
//
fun nodes
  (n2s: list0(n2ode)): list0 (c3nstr) =
(
  case+ n2s of
  | list0_nil () => list0_nil ()
  | list0_cons (n2, n2s) =>
      list0_cons (n2ode_check (n2), nodes (n2s))
) (* end of [nodes] *)
//
val ncs = nodes (prog.p2rog_nodes)
//
// the top level: sensor/actuator types are ground; equations drive
// actuators
//
val acc = c3acc_new ()
val () = wf_d2vars (acc, prog.p2rog_sensors)
val () = wf_d2vars (acc, prog.p2rog_actuators)
fun eqns
  (acc: c3acc, eqs: list0(e2qn)): void =
  case+ eqs of
  | list0_nil () => ()
  | list0_cons (eqn, eqs) =>
      (e2qn_check (acc, eqn); eqns (acc, eqs))
val () = eqns (acc, prog.p2rog_eqns)
val items = c3acc_takeout (acc)
val topc = c3nstr_itmlst (location_dummy (), "the top level", items)
//
in
  list0_append (ncs, list0_cons (topc, list0_nil ()))
end // end of [trans3_program]

(* ****** ****** *)

(* end of [overture_trans3.dats] *)
