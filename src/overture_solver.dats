(*
** Overture: the constraint solver
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
staload "./overture_constraint.sats"
staload "./overture_solver.sats"

(* ****** ****** *)

fun gcd (a: int, b: int): int = let
  val a = (if a < 0 then ~a else a): int
in
  if b = 0 then a else gcd (b, a mod b)
end // end of [gcd]

(* ****** ****** *)
//
// linear forms: (c + sum ci * xi) / den with den >= 1;
// the xi are integer-valued solver variables (ivars)
//
typedef term = @(int(*ivar*), int(*coef*))

typedef lf = '{
  lf_den= int, lf_c= int, lf_ts= list0(term)
} (* end of [lf] *)

fn lf_make
  (den: int, c: int, ts: list0(term)): lf =
  '{ lf_den= den, lf_c= c, lf_ts= ts }

fn lf_const (c: int): lf =
  lf_make (1, c, list0_nil ())
fn lf_constr (a: int, b: int): lf = (* a/b *)
  if b < 0
    then lf_make (~b, ~a, list0_nil ())
    else lf_make (b, a, list0_nil ())
fn lf_ivar (iv: int): lf =
  lf_make (1, 0, list0_cons (@(iv, 1), list0_nil ()))

fn lf_is_const (f: lf): bool =
  case+ f.lf_ts of list0_nil () => true | _ => false

(*
** combine like terms; drop zeros; reduce by the gcd of everything
*)
fn lf_norm (f: lf): lf = let
//
fun coef_of
  (ts: list0(term), iv: int): int =
  case+ ts of
  | list0_nil () => 0
  | list0_cons (t, ts) =>
      if t.0 = iv then t.1 + coef_of (ts, iv) else coef_of (ts, iv)
//
fun dedup
  (ts: list0(term), seen: list0(int)): list0(term) =
(
  case+ ts of
  | list0_nil () => list0_nil ()
  | list0_cons (t, ts1) => let
      fun mem (ivs: list0(int), iv: int): bool =
        case+ ivs of
        | list0_nil () => false
        | list0_cons (jv, ivs) => if jv = iv then true else mem (ivs, iv)
    in
      if mem (seen, t.0) then dedup (ts1, seen)
      else let
        val c = coef_of (f.lf_ts, t.0)
      in
        if c = 0
          then dedup (ts1, list0_cons (t.0, seen))
          else list0_cons (@(t.0, c), dedup (ts1, list0_cons (t.0, seen)))
      end
    end
) (* end of [dedup] *)
//
val ts = dedup (f.lf_ts, list0_nil ())
//
fun gcdts (ts: list0(term), g: int): int =
  case+ ts of
  | list0_nil () => g
  | list0_cons (t, ts) => gcdts (ts, gcd (g, t.1))
//
val g = gcdts (ts, gcd (f.lf_den, f.lf_c))
val g = (if g = 0 then 1 else g): int
//
fun scale (ts: list0(term), g: int): list0(term) =
  case+ ts of
  | list0_nil () => list0_nil ()
  | list0_cons (t, ts) => list0_cons (@(t.0, t.1 / g), scale (ts, g))
//
in
  lf_make (f.lf_den / g, f.lf_c / g, scale (ts, g))
end // end of [lf_norm]

fn lf_add (f1: lf, f2: lf): lf = let
  fun scale (ts: list0(term), k: int): list0(term) =
    case+ ts of
    | list0_nil () => list0_nil ()
    | list0_cons (t, ts) => list0_cons (@(t.0, t.1 * k), scale (ts, k))
  val d1 = f1.lf_den and d2 = f2.lf_den
in
  lf_norm (lf_make
    ( d1 * d2
    , f1.lf_c * d2 + f2.lf_c * d1
    , list0_append (scale (f1.lf_ts, d2), scale (f2.lf_ts, d1))
    ))
end // end of [lf_add]

fn lf_neg (f: lf): lf = let
  fun neg (ts: list0(term)): list0(term) =
    case+ ts of
    | list0_nil () => list0_nil ()
    | list0_cons (t, ts) => list0_cons (@(t.0, ~(t.1)), neg (ts))
in
  lf_make (f.lf_den, ~(f.lf_c), neg (f.lf_ts))
end // end of [lf_neg]

fn lf_sub (f1: lf, f2: lf): lf =
  lf_add (f1, lf_neg (f2))

(*
** multiply by the rational a/b (b != 0)
*)
fn lf_scale
  (f: lf, a: int, b: int): lf = let
  val (a, b) = (if b < 0 then (~a, ~b) else (a, b)): @(int, int)
  fun scale (ts: list0(term), a: int): list0(term) =
    case+ ts of
    | list0_nil () => list0_nil ()
    | list0_cons (t, ts) => list0_cons (@(t.0, t.1 * a), scale (ts, a))
in
  lf_norm (lf_make (f.lf_den * b, f.lf_c * a, scale (f.lf_ts, a)))
end // end of [lf_scale]

(*
** the integer numerator (denominator cleared)
*)
fn lf_numer (f: lf): lf =
  lf_make (1, f.lf_c, f.lf_ts)

(* ****** ****** *)
//
// lowered constraints
//
// vector kinds: 2 for (>= 0), 1 for (= 0), ~1 for (!= 0)
//
datatype icl =
| ICLvec of (int(*knd*), lf)
| ICLconj of list0(icl)
| ICLdisj of list0(icl)
| ICLtrue of ()
| ICLfalse of ()
| ICLerr of (location, string)
// end of [icl]

(* ****** ****** *)
//
// the lowering context
//
typedef lowctx = '{
  lc_nivar= ref (int)
, lc_imap= ref (list0(@(int(*stamp*), int(*ivar*))))
, lc_cmap= ref (list0(@(int(*stamp*), @(int, int))))
, lc_extra= ref (list0(icl)) (* implicit hypotheses: n >= 1, d >= 0 *)
} (* end of [lowctx] *)

fn lowctx_new (): lowctx = '{
  lc_nivar= ref<int> (0)
, lc_imap= ref<list0(@(int, int))> (list0_nil ())
, lc_cmap= ref<list0(@(int, @(int, int)))> (list0_nil ())
, lc_extra= ref<list0(icl)> (list0_nil ())
} (* end of [lowctx_new] *)

fn fresh_ivar (ctx: lowctx): int = let
  val r = ctx.lc_nivar
  val n = !r
  val () = !r := n + 1
in
  n
end // end of [fresh_ivar]

fn ivar_of_svar
  (ctx: lowctx, s2v: s2var): int = let
  val stamp = s2var_get_stamp (s2v)
  val r = ctx.lc_imap
  fun loop (kxs: list0(@(int, int))): int =
    case+ kxs of
    | list0_nil () => let
        val iv = fresh_ivar (ctx)
        val () = !r := list0_cons (@(stamp, iv), !r)
      in
        iv
      end
    | list0_cons (kx, kxs) =>
        if kx.0 = stamp then kx.1 else loop (kxs)
in
  loop (!r)
end // end of [ivar_of_svar]

fn ivars_of_clock_svar
  (ctx: lowctx, s2v: s2var): @(int, int) = let
  val stamp = s2var_get_stamp (s2v)
  val r = ctx.lc_cmap
  fun loop
    (kxs: list0(@(int, @(int, int)))): @(int, int) =
    case+ kxs of
    | list0_nil () => let
        val pn = fresh_ivar (ctx)
        val pd = fresh_ivar (ctx)
        val () = !r := list0_cons (@(stamp, @(pn, pd)), !r)
        (* implicit: period >= 1, date >= 0 *)
        val ge1 = ICLvec (2, lf_add (lf_ivar (pn), lf_const (~1)))
        val ge0 = ICLvec (2, lf_ivar (pd))
        val xr = ctx.lc_extra
        val () = !xr := list0_cons (ge1, list0_cons (ge0, !xr))
      in
        @(pn, pd)
      end
    | list0_cons (kx, kxs) =>
        if kx.0 = stamp then kx.1 else loop (kxs)
in
  loop (!r)
end // end of [ivars_of_clock_svar]

(* ****** ****** *)
//
// lowering of numeric static expressions
//
datatype lfres =
| LFok of lf
| LFerr of (location, string)

datatype clres =
| CLok of (lf(*period*), lf(*date*))
| CLerr of (location, string)

extern fun lower_num (ctx: lowctx, s2e: s2exp): lfres
extern fun lower_clk (ctx: lowctx, s2e: s2exp): clres

implement
lower_num
  (ctx, s2e) = let
  val loc = s2e.s2e_loc
in
//
case+ s2e.s2e_node of
//
| S2Eint (i) => LFok (lf_const (i))
| S2Erat (a, b) => LFok (lf_constr (a, b))
//
| S2Evar (s2v) => (
    case+ s2var_get_srt (s2v) of
    | S2RTint () => LFok (lf_ivar (ivar_of_svar (ctx, s2v)))
    | _ (*rest*) => LFerr (loc,
        "only integer-sorted variables are supported by the solver")
  )
//
| S2Ecst _ => LFerr (loc, "uninterpreted static constant")
//
| S2Eapp (s2c, args) => let
    val name = symbol_get_name (s2cst_get_name (s2c))
  in
    case+ args of
    | list0_cons (l, list0_cons (r, list0_nil ())) => (
        case+ name of
        | "+" => (
            case+ (lower_num (ctx, l), lower_num (ctx, r)) of
            | (LFok f1, LFok f2) => LFok (lf_add (f1, f2))
            | (LFerr (l0, m0), _) => LFerr (l0, m0)
            | (_, LFerr (l0, m0)) => LFerr (l0, m0)
          )
        | "-" => (
            case+ (lower_num (ctx, l), lower_num (ctx, r)) of
            | (LFok f1, LFok f2) => LFok (lf_sub (f1, f2))
            | (LFerr (l0, m0), _) => LFerr (l0, m0)
            | (_, LFerr (l0, m0)) => LFerr (l0, m0)
          )
        | "*" => (
            case+ (lower_num (ctx, l), lower_num (ctx, r)) of
            | (LFok f1, LFok f2) =>
                if lf_is_const (f2)
                  then LFok (lf_scale (f1, f2.lf_c, f2.lf_den))
                else if lf_is_const (f1)
                  then LFok (lf_scale (f2, f1.lf_c, f1.lf_den))
                else LFerr (loc,
                  "nonlinear constraint: multiply by a literal constant")
            | (LFerr (l0, m0), _) => LFerr (l0, m0)
            | (_, LFerr (l0, m0)) => LFerr (l0, m0)
          )
        | "/" => (
            case+ (lower_num (ctx, l), lower_num (ctx, r)) of
            | (LFok f1, LFok f2) =>
                if lf_is_const (f2) then (
                  if f2.lf_c = 0
                    then LFerr (loc, "division by zero")
                    else LFok (lf_scale (f1, f2.lf_den, f2.lf_c))
                ) else LFerr (loc,
                  "nonlinear constraint: divide by a literal constant")
            | (LFerr (l0, m0), _) => LFerr (l0, m0)
            | (_, LFerr (l0, m0)) => LFerr (l0, m0)
          )
        | _ (*rest*) => LFerr (loc, "uninterpreted static operation")
      )
    | list0_cons (c, list0_nil ())
        when name = "period" => (
        case+ lower_clk (ctx, c) of
        | CLok (prd, _) => LFok (prd)
        | CLerr (e0, e1) => LFerr (e0, e1)
      )
    | list0_cons (c, list0_nil ())
        when name = "date" => (
        case+ lower_clk (ctx, c) of
        | CLok (_, dat) => LFok (dat)
        | CLerr (e0, e1) => LFerr (e0, e1)
      )
    | _ (*rest*) => LFerr (loc, "uninterpreted static operation")
  end
//
end // end of [lower_num]

implement
lower_clk
  (ctx, s2e) = let
  val loc = s2e.s2e_loc
in
//
case+ s2e.s2e_node of
//
| S2Evar (s2v) => (
    case+ s2var_get_srt (s2v) of
    | S2RTclock () => let
        val nd = ivars_of_clock_svar (ctx, s2v)
      in
        CLok (lf_ivar (nd.0), lf_ivar (nd.1))
      end
    | _ (*rest*) => CLerr (loc, "expected a clock variable")
  )
//
| S2Eapp (s2c, args) => let
    val name = symbol_get_name (s2cst_get_name (s2c))
  in
    case+ args of
    | list0_cons (a1, list0_cons (a2, list0_nil ())) => (
        case+ name of
        | "clk" => (
            case+ (lower_num (ctx, a1), lower_num (ctx, a2)) of
            | (LFok fn_, LFok fp) =>
                if lf_is_const (fp)
                  then CLok (fn_, lf_scale (fn_, fp.lf_c, fp.lf_den))
                else if lf_is_const (fn_)
                  then CLok (fn_, lf_scale (fp, fn_.lf_c, fn_.lf_den))
                else CLerr (loc,
                  "nonlinear clock: period or phase must be a constant")
            | (LFerr (l0, m0), _) => CLerr (l0, m0)
            | (_, LFerr (l0, m0)) => CLerr (l0, m0)
          )
        | "*/" => (
            (* dates are invariant: (n/k, d) *)
            case+ (lower_clk (ctx, a1), lower_num (ctx, a2)) of
            | (CLok (prd, dat), LFok fk) =>
                if lf_is_const (fk) then (
                  if fk.lf_c = 0
                    then CLerr (loc, "oversampling by zero")
                    else CLok (lf_scale (prd, fk.lf_den, fk.lf_c), dat)
                ) else CLerr (loc,
                  "nonlinear clock: the sampling factor must be a constant")
            | (CLerr (l0, m0), _) => CLerr (l0, m0)
            | (_, LFerr (l0, m0)) => CLerr (l0, m0)
          )
        | "^/" => (
            case+ (lower_clk (ctx, a1), lower_num (ctx, a2)) of
            | (CLok (prd, dat), LFok fk) =>
                if lf_is_const (fk)
                  then CLok (lf_scale (prd, fk.lf_c, fk.lf_den), dat)
                else CLerr (loc,
                  "nonlinear clock: the sampling factor must be a constant")
            | (CLerr (l0, m0), _) => CLerr (l0, m0)
            | (_, LFerr (l0, m0)) => CLerr (l0, m0)
          )
        | "shift" => (
            (* (n, d + n * k) *)
            case+ (lower_clk (ctx, a1), lower_num (ctx, a2)) of
            | (CLok (prd, dat), LFok fk) =>
                if lf_is_const (fk)
                  then CLok
                    (prd, lf_add (dat, lf_scale (prd, fk.lf_c, fk.lf_den)))
                else CLerr (loc,
                  "nonlinear clock: the shift amount must be a constant")
            | (CLerr (l0, m0), _) => CLerr (l0, m0)
            | (_, LFerr (l0, m0)) => CLerr (l0, m0)
          )
        | _ (*rest*) => CLerr (loc, "uninterpreted clock operation")
      )
    | _ (*rest*) => CLerr (loc, "uninterpreted clock operation")
  end
//
| _ (*rest*) => CLerr (loc, "unsupported clock expression")
//
end // end of [lower_clk]

(* ****** ****** *)
//
// lowering boolean hypotheses; unsupported pieces are weakened away
//
fn relvec
  (name: string, f1: lf, f2: lf): list0 (icl) = let
//
// values of numerators are integers, so strict inequalities
// tighten by one after clearing denominators
//
val d = lf_numer (lf_sub (f1, f2)) (* f1 - f2, cleared *)
val dneg = lf_neg (d)
//
in
//
case+ name of
| "==" =>
    list0_cons (ICLvec (1, d), list0_nil ())
| "!=" =>
    list0_cons (ICLvec (~1, d), list0_nil ())
| "<=" =>
    list0_cons (ICLvec (2, dneg), list0_nil ())
| ">=" =>
    list0_cons (ICLvec (2, d), list0_nil ())
| "<" =>
    list0_cons (ICLvec (2, lf_add (dneg, lf_const (~1))), list0_nil ())
| ">" =>
    list0_cons (ICLvec (2, lf_add (d, lf_const (~1))), list0_nil ())
| _ (*rest*) => list0_nil ()
//
end // end of [relvec]

fn is_relname (name: string): bool =
  (name = "==") orelse (name = "!=") orelse
  (name = "<") orelse (name = "<=") orelse
  (name = ">") orelse (name = ">=")

extern fun lower_hypo (ctx: lowctx, s2e: s2exp): list0 (icl)

implement
lower_hypo
  (ctx, s2e) = let
  val loc = s2e.s2e_loc
in
//
case+ s2e.s2e_node of
//
| S2Ecst (s2c) => let
    val name = symbol_get_name (s2cst_get_name (s2c))
  in
    case+ name of
    | "true" => list0_nil ()
    | "false" => list0_cons (ICLfalse (), list0_nil ())
    | _ (*rest*) => list0_nil () (* weaken *)
  end
//
| S2Eapp (s2c, args) => let
    val name = symbol_get_name (s2cst_get_name (s2c))
  in
    case+ args of
    | list0_cons (l, list0_cons (r, list0_nil ())) => (
        case+ name of
        | "&&" =>
            list0_append (lower_hypo (ctx, l), lower_hypo (ctx, r))
        | "||" => let
            val bl = ICLconj (lower_hypo (ctx, l))
            val br = ICLconj (lower_hypo (ctx, r))
          in
            list0_cons
              (ICLdisj (list0_cons (bl, list0_cons (br, list0_nil ()))),
               list0_nil ())
          end
        | _ when is_relname (name) => (
            case+ l.s2e_srt of
            | S2RTclock () => (
                (* clock equality: periods and dates *)
                case+ (lower_clk (ctx, l), lower_clk (ctx, r)) of
                | (CLok (n1, d1), CLok (n2, d2)) => (
                    case+ name of
                    | "==" =>
                        list0_append
                          (relvec ("==", n1, n2), relvec ("==", d1, d2))
                    | "!=" => let
                        val b1 = ICLconj (relvec ("!=", n1, n2))
                        val b2 = ICLconj (relvec ("!=", d1, d2))
                      in
                        list0_cons
                          (ICLdisj (list0_cons (b1, list0_cons (b2, list0_nil ()))),
                           list0_nil ())
                      end
                    | _ (*rest*) => list0_nil () (* weaken *)
                  )
                | (_, _) => list0_nil () (* weaken *)
              )
            | _ (*int/rat*) => (
                case+ (lower_num (ctx, l), lower_num (ctx, r)) of
                | (LFok f1, LFok f2) => relvec (name, f1, f2)
                | (_, _) => list0_nil () (* weaken *)
              )
          )
        | "|" => (
            (* k | n as a hypothesis: n = k * q for fresh q *)
            case+ (lower_num (ctx, l), lower_num (ctx, r)) of
            | (LFok fk, LFok fn_) =>
                if lf_is_const (fk) then let
                  val q = fresh_ivar (ctx)
                  (* n - k*q = 0, denominators cleared *)
                  val kq = lf_scale (lf_ivar (q), fk.lf_c, fk.lf_den)
                  val d = lf_numer (lf_sub (fn_, kq))
                in
                  list0_cons (ICLvec (1, d), list0_nil ())
                end else list0_nil () (* weaken *)
            | (_, _) => list0_nil () (* weaken *)
          )
        | _ (*rest*) => list0_nil () (* weaken *)
      )
    | list0_cons (e, list0_nil ())
        when name = "isint" => (
        (* isint(e): numer = den * q for fresh q *)
        case+ lower_num (ctx, e) of
        | LFok f =>
            if f.lf_den = 1 then list0_nil ()
            else let
              val q = fresh_ivar (ctx)
              val dq = lf_scale (lf_ivar (q), f.lf_den, 1)
              val d = lf_sub (lf_numer (f), dq)
            in
              list0_cons (ICLvec (1, d), list0_nil ())
            end
        | LFerr _ => list0_nil () (* weaken *)
      )
    | _ (*rest*) => list0_nil () (* weaken *)
  end
//
| _ (*rest*) => list0_nil () (* weaken *)
//
end // end of [lower_hypo]

(* ****** ****** *)
//
// refutation: is the conjunction of [icls] unsatisfiable over the
// integers? (a positive answer is definitive; a negative one is not)
//
typedef vec = @(int(*knd: 2 gte, 1 eq*), lf)

fun
icls_flatten
(
  icls: list0(icl)
, eqs: list0(lf), neqs: list0(lf), gtes: list0(lf)
, disjs: list0(list0(icl))
) : option0 (@(list0(lf), list0(lf), list0(lf), list0(list0(icl)))) =
(
// returns None0 if an ICLfalse was found (immediately unsat)
case+ icls of
| list0_nil () => Some0 (@(eqs, neqs, gtes, disjs))
| list0_cons (c, icls) => (
    case+ c of
    | ICLvec (knd, f) => (
        case+ 0 of
        | _ when knd = 1 =>
            icls_flatten (icls, list0_cons (f, eqs), neqs, gtes, disjs)
        | _ when knd = ~1 =>
            icls_flatten (icls, eqs, list0_cons (f, neqs), gtes, disjs)
        | _ (*2*) =>
            icls_flatten (icls, eqs, neqs, list0_cons (f, gtes), disjs)
      )
    | ICLconj (cs) =>
        icls_flatten (list0_append (cs, icls), eqs, neqs, gtes, disjs)
    | ICLdisj (bs) =>
        icls_flatten (icls, eqs, neqs, gtes, list0_cons (bs, disjs))
    | ICLtrue () => icls_flatten (icls, eqs, neqs, gtes, disjs)
    | ICLfalse () => None0 ()
    | ICLerr _ => icls_flatten (icls, eqs, neqs, gtes, disjs) (* weaken *)
  )
) (* end of [icls_flatten] *)

(*
** eliminate variable [iv] from [target] using equality [eq]
** (integer-sound: scale the target by |a| where a is iv's
** coefficient in eq; positive scaling preserves inequality signs)
*)
fn elim_with_eq
  (target: lf, eq: lf, iv: int): lf = let
//
fun coef_of (ts: list0(term), iv: int): int =
  case+ ts of
  | list0_nil () => 0
  | list0_cons (t, ts) =>
      if t.0 = iv then t.1 else coef_of (ts, iv)
//
val a = coef_of (eq.lf_ts, iv)
val b = coef_of (target.lf_ts, iv)
//
in
//
if b = 0 then target
else if a = 0 then target
else let
  val absa = (if a < 0 then ~a else a): int
  (* target' = |a| * target - sign(a) * b * eq *)
  val t1 = lf_scale (target, absa, 1)
  val s = (if a < 0 then ~1 else 1): int
  val t2 = lf_scale (eq, s * b, 1)
in
  lf_norm (lf_sub (t1, t2))
end // end of [if]
//
end // end of [elim_with_eq]

(*
** integer tightening of a (>= 0) vector: divide by the gcd of the
** variable coefficients, flooring the constant
*)
fn gte_tighten (f: lf): lf = let
//
fun gcdts (ts: list0(term), g: int): int =
  case+ ts of
  | list0_nil () => g
  | list0_cons (t, ts) => gcdts (ts, gcd (g, t.1))
val g = gcdts (f.lf_ts, 0)
//
in
//
if g <= 1 then f
else let
  fun scale (ts: list0(term), g: int): list0(term) =
    case+ ts of
    | list0_nil () => list0_nil ()
    | list0_cons (t, ts) => list0_cons (@(t.0, t.1 / g), scale (ts, g))
  val c = f.lf_c
  (* floor division for possibly negative constants *)
  val cq = (if c >= 0 then c / g else ~((~c + g - 1) / g)): int
in
  lf_make (1, cq, scale (f.lf_ts, g))
end // end of [if]
//
end // end of [gte_tighten]

fun
vars_of
  (fs: list0(lf), acc: list0(int)): list0 (int) = let
//
fun mem (ivs: list0(int), iv: int): bool =
  case+ ivs of
  | list0_nil () => false
  | list0_cons (jv, ivs) => if jv = iv then true else mem (ivs, iv)
//
fun add1
  (ts: list0(term), acc: list0(int)): list0(int) =
  case+ ts of
  | list0_nil () => acc
  | list0_cons (t, ts) =>
      if mem (acc, t.0)
        then add1 (ts, acc) else add1 (ts, list0_cons (t.0, acc))
      // end of [if]
//
in
  case+ fs of
  | list0_nil () => acc
  | list0_cons (f, fs) => vars_of (fs, add1 (f.lf_ts, acc))
end // end of [vars_of]

extern fun refute (icls: list0(icl)): bool

(*
** the Fourier-Motzkin core over (>= 0) vectors
*)
fun
fm_refute
  (gtes: list0(lf)): bool = let
//
fun coef_of (ts: list0(term), iv: int): int =
  case+ ts of
  | list0_nil () => 0
  | list0_cons (t, ts) =>
      if t.0 = iv then t.1 else coef_of (ts, iv)
//
fun ground_contra
  (fs: list0(lf)): bool =
  case+ fs of
  | list0_nil () => false
  | list0_cons (f, fs) => (
      case+ f.lf_ts of
      | list0_nil () =>
          if f.lf_c < 0 then true else ground_contra (fs)
      | _ (*rest*) => ground_contra (fs)
    )
//
in
//
if ground_contra (gtes) then true
else let
  val vars = vars_of (gtes, list0_nil ())
in
  case+ vars of
  | list0_nil () => false
  | list0_cons (iv, _) => let
      (* eliminate iv *)
      fun split
        (fs: list0(lf), pos: list0(lf), neg: list0(lf), none: list0(lf))
        : @(list0(lf), list0(lf), list0(lf)) =
        case+ fs of
        | list0_nil () => @(pos, neg, none)
        | list0_cons (f, fs) => let
            val c = coef_of (f.lf_ts, iv)
          in
            if c > 0 then split (fs, list0_cons (f, pos), neg, none)
            else if c < 0 then split (fs, pos, list0_cons (f, neg), none)
            else split (fs, pos, neg, list0_cons (f, none))
          end
      val pnn = split (gtes, list0_nil (), list0_nil (), list0_nil ())
      (* combine every (pos, neg) pair *)
      fun combine
        (ps: list0(lf), ns: list0(lf), acc: list0(lf)): list0(lf) = let
        fun with1
          (p: lf, ns: list0(lf), acc: list0(lf)): list0(lf) =
          case+ ns of
          | list0_nil () => acc
          | list0_cons (n, ns) => let
              val a = coef_of (p.lf_ts, iv)      (* a > 0 *)
              val c = ~(coef_of (n.lf_ts, iv))   (* c > 0 *)
              (* c*p + a*n eliminates iv *)
              val comb =
                lf_norm (lf_add (lf_scale (p, c, 1), lf_scale (n, a, 1)))
              val comb = gte_tighten (comb)
            in
              with1 (p, ns, list0_cons (comb, acc))
            end
      in
        case+ ps of
        | list0_nil () => acc
        | list0_cons (p, ps) =>
            combine (ps, ns, with1 (p, ns, acc))
      end
      val combined = combine (pnn.0, pnn.1, list0_nil ())
    in
      fm_refute (list0_append (combined, pnn.2))
    end
end // end of [if]
//
end // end of [fm_refute]

fn rebuild_all
  (eqs: list0(lf), gtes: list0(lf)): list0 (icl) = let
  fun vecs
    (knd: int, fs: list0(lf), acc: list0(icl)): list0(icl) =
    case+ fs of
    | list0_nil () => acc
    | list0_cons (f, fs) =>
        vecs (knd, fs, list0_cons (ICLvec (knd, f), acc))
  val acc = vecs (1, eqs, list0_nil ())
in
  vecs (2, gtes, acc)
end // end of [rebuild_all]

implement
refute (icls) = let
  val r =
    icls_flatten
      (icls, list0_nil (), list0_nil (), list0_nil (), list0_nil ())
  // end of [val]
in
//
case+ r of
| None0 () => true (* an ICLfalse hypothesis *)
| Some0 (parts) => let
    val eqs = parts.0
    val neqs = parts.1
    val gtes = parts.2
    val disjs = parts.3
  in
    case+ disjs of
    | list0_cons (branches, disjs) => let
        (* every branch must be refutable *)
        fun rebuild
          (eqs: list0(lf), neqs: list0(lf), gtes: list0(lf)
          , disjs: list0(list0(icl))): list0(icl) = let
          fun vecs
            (knd: int, fs: list0(lf), acc: list0(icl)): list0(icl) =
            case+ fs of
            | list0_nil () => acc
            | list0_cons (f, fs) =>
                vecs (knd, fs, list0_cons (ICLvec (knd, f), acc))
          fun djs
            (ds: list0(list0(icl)), acc: list0(icl)): list0(icl) =
            case+ ds of
            | list0_nil () => acc
            | list0_cons (d, ds) =>
                djs (ds, list0_cons (ICLdisj (d), acc))
          val acc = vecs (1, eqs, list0_nil ())
          val acc = vecs (~1, neqs, acc)
          val acc = vecs (2, gtes, acc)
        in
          djs (disjs, acc)
        end
        val rest = rebuild (eqs, neqs, gtes, disjs)
        fun allrefuted (bs: list0(icl)): bool =
          case+ bs of
          | list0_nil () => true
          | list0_cons (b, bs) =>
              if refute (list0_cons (b, rest))
                then allrefuted (bs) else false
              // end of [if]
      in
        allrefuted (branches)
      end
    | list0_nil () => (
      case+ neqs of
      | list0_cons (f, neqs) => let
          (* explode: f >= 1 or -f >= 1 *)
          fun rebuild
            (eqs: list0(lf), neqs: list0(lf), gtes: list0(lf)): list0(icl) = let
            fun vecs
              (knd: int, fs: list0(lf), acc: list0(icl)): list0(icl) =
              case+ fs of
              | list0_nil () => acc
              | list0_cons (f, fs) =>
                  vecs (knd, fs, list0_cons (ICLvec (knd, f), acc))
            val acc = vecs (1, eqs, list0_nil ())
            val acc = vecs (~1, neqs, acc)
          in
            vecs (2, gtes, acc)
          end
          val rest = rebuild (eqs, neqs, gtes)
          val fpos = lf_add (lf_numer (f), lf_const (~1))
          val fneg = lf_add (lf_neg (lf_numer (f)), lf_const (~1))
          val b1 = refute (list0_cons (ICLvec (2, fpos), rest))
        in
          if b1 then refute (list0_cons (ICLvec (2, fneg), rest)) else false
        end
      | list0_nil () => (
        case+ eqs of
        | list0_cons (eq, eqs) => let
            val eq = lf_norm (lf_numer (eq))
          in
            case+ eq.lf_ts of
            | list0_nil () =>
                (* ground equality *)
                if eq.lf_c != 0 then true
                else refute (rebuild_all (eqs, gtes))
            | list0_cons (t, _) => let
                (* gcd infeasibility check *)
                fun gcdts (ts: list0(term), g: int): int =
                  case+ ts of
                  | list0_nil () => g
                  | list0_cons (t, ts) => gcdts (ts, gcd (g, t.1))
                val g = gcdts (eq.lf_ts, 0)
              in
                if (g > 1) andalso (eq.lf_c mod g != 0) then true
                else let
                  (* substitute eq away everywhere *)
                  val iv = t.0
                  fun elim (fs: list0(lf)): list0(lf) =
                    case+ fs of
                    | list0_nil () => list0_nil ()
                    | list0_cons (f, fs) =>
                        list0_cons (elim_with_eq (f, eq, iv), elim (fs))
                in
                  refute (rebuild_all (elim (eqs), elim (gtes)))
                end
              end
          end
        | list0_nil () => fm_refute (gtes)
      ) (* end of [neqs nil] *)
    ) (* end of [disjs nil] *)
  end
//
end // end of [refute]

(* ****** ****** *)
//
// goal checking
//
typedef horig = @(location, s2exp)

fn report_unsolved
(
  c3t: c3nstr, goal: s2exp, origs: list0(horig), why: string
) : void = let
//
// number colliding variable names (message-locally) so that fresh
// copies print as, e.g., q with subscripts 1 and 2
//
val svars = s2exp_collect_svars (goal, list0_nil ())
val svars = (
  let
    fun loop
      (origs: list0(horig), acc: list0(s2var)): list0(s2var) =
      case+ origs of
      | list0_nil () => acc
      | list0_cons (h, origs) =>
          loop (origs, s2exp_collect_svars (h.1, acc))
  in
    loop (origs, svars)
  end
) : list0 (s2var) // end of [val]
val () = the_dsptbl_set (list0_reverse (svars))
//
val () = fprint! (stderr_ref, "  needed: ")
val () = fprint_s2exp (stderr_ref, goal)
val () = fprint! (stderr_ref, "\n")
val () = (
  case+ origs of
  | list0_nil () => ()
  | list0_cons _ => let
      val () = fprint! (stderr_ref, "  hypotheses:")
      fun loop
        (origs: list0(horig), first: bool): void =
        case+ origs of
        | list0_nil () => ()
        | list0_cons (h, origs) => let
            val () =
              if first then fprint! (stderr_ref, " ")
              else fprint! (stderr_ref, "; ")
            val () = fprint_s2exp (stderr_ref, h.1)
          in
            loop (origs, false)
          end
      val () = loop (origs, true)
    in
      fprint! (stderr_ref, "\n")
    end
) : void // end of [val]
val () =
  if why != "" then
    fprint! (stderr_ref, "  note: ", why, "\n")
//
val () = errmsg (c3t.c3nstr_loc,
  string_append ("unsolved constraint: ", c3t.c3nstr_msg))
//
in
  the_dsptbl_clear ()
end // end of [report_unsolved]

(*
** prove D | N by substituting unit-coefficient equality hypotheses
** into N and checking that D divides every residual coefficient
*)
fn divides_prove
(
  ctx: lowctx, hypos: list0(icl), dvsr: int, num: lf
) : bool = let
//
val dvsr = (if dvsr < 0 then ~dvsr else dvsr): int
//
in
//
if dvsr = 0 then false
else if dvsr = 1 then true
else let
  //
  // collect equality vectors from the hypotheses (top-level only)
  //
  fun eqvecs
    (icls: list0(icl), acc: list0(lf)): list0(lf) =
    case+ icls of
    | list0_nil () => acc
    | list0_cons (c, icls) => (
        case+ c of
        | ICLvec (1, f) => eqvecs (icls, list0_cons (f, acc))
        | ICLconj (cs) => eqvecs (list0_append (cs, icls), acc)
        | _ (*rest*) => eqvecs (icls, acc)
      )
  val eqs = eqvecs (hypos, list0_nil ())
  //
  fun coef_of (ts: list0(term), iv: int): int =
    case+ ts of
    | list0_nil () => 0
    | list0_cons (t, ts) =>
        if t.0 = iv then t.1 else coef_of (ts, iv)
  //
  // substitute unit-pivot equalities into [num] until no variable of
  // [num] has a unit-coefficient equality (fuel-bounded)
  //
  fun subst1
    (num: lf, eqs: list0(lf)): option0 (lf) = let
    fun tryeqs
      (num: lf, eqs0: list0(lf)): option0 (lf) =
      case+ eqs0 of
      | list0_nil () => None0 ()
      | list0_cons (eq, eqs0) => let
          val eq = lf_numer (eq)
          fun unitvar
            (ts: list0(term)): option0 (int) =
            case+ ts of
            | list0_nil () => None0 ()
            | list0_cons (t, ts) =>
                if (t.1 = 1) orelse (t.1 = ~1)
                  then (
                    if coef_of (num.lf_ts, t.0) != 0
                      then Some0 (t.0) else unitvar (ts)
                  ) else unitvar (ts)
        in
          case+ unitvar (eq.lf_ts) of
          | Some0 (iv) => Some0 (elim_with_eq (num, eq, iv))
          | None0 () => tryeqs (num, eqs0)
        end
  in
    tryeqs (num, eqs)
  end // end of [subst1]
  //
  fun saturate
    (num: lf, fuel: int): lf =
    if fuel <= 0 then num
    else (
      case+ subst1 (num, eqs) of
      | Some0 (num1) => saturate (num1, fuel - 1)
      | None0 () => num
    )
  //
  val num = saturate (lf_norm (lf_numer (num)), 32)
  //
  fun alldiv
    (ts: list0(term), d: int): bool =
    case+ ts of
    | list0_nil () => true
    | list0_cons (t, ts) =>
        if t.1 mod d = 0 then alldiv (ts, d) else false
  //
in
  if num.lf_c mod dvsr = 0 then alldiv (num.lf_ts, dvsr) else false
end // end of [if]
//
end // end of [divides_prove]

(*
** negate a relational goal into refutable vectors;
** None0 if the shape is not linearly negatable
*)
fn goal_negate
(
  ctx: lowctx, s2e: s2exp
) : option0 (list0(icl)) = let
  val loc = s2e.s2e_loc
in
//
case+ s2e.s2e_node of
| S2Eapp (s2c, args) => let
    val name = symbol_get_name (s2cst_get_name (s2c))
  in
    case+ args of
    | list0_cons (l, list0_cons (r, list0_nil ()))
        when is_relname (name) => (
        case+ l.s2e_srt of
        | S2RTclock () => (
            case+ (lower_clk (ctx, l), lower_clk (ctx, r)) of
            | (CLok (n1, d1), CLok (n2, d2)) => (
                case+ name of
                | "==" => let
                    (* neg: periods differ or dates differ *)
                    val b1 = ICLconj (relvec ("!=", n1, n2))
                    val b2 = ICLconj (relvec ("!=", d1, d2))
                  in
                    Some0 (list0_cons
                      (ICLdisj (list0_cons (b1, list0_cons (b2, list0_nil ()))),
                       list0_nil ()))
                  end
                | "!=" =>
                    Some0 (list0_append
                      (relvec ("==", n1, n2), relvec ("==", d1, d2)))
                | _ (*rest*) => None0 ()
              )
            | (_, _) => None0 ()
          )
        | _ (*int/rat*) => (
            case+ (lower_num (ctx, l), lower_num (ctx, r)) of
            | (LFok f1, LFok f2) => let
                val negname = (
                  case+ name of
                  | "==" => "!="
                  | "!=" => "=="
                  | "<" => ">="
                  | "<=" => ">"
                  | ">" => "<="
                  | _ (*">="*) => "<"
                ) : string
              in
                Some0 (relvec (negname, f1, f2))
              end
            | (_, _) => None0 ()
          )
      )
    | _ (*rest*) => None0 ()
  end
| _ (*rest*) => None0 ()
//
end // end of [goal_negate]

extern fun check_goal
(
  ctx: lowctx
, hypos: list0(icl), origs: list0(horig)
, c3t: c3nstr, s2e: s2exp
) : void

implement
check_goal
  (ctx, hypos, origs, c3t, s2e) = let
//
fn allhypos
  (ctx: lowctx, hypos: list0(icl)): list0(icl) =
  list0_append (hypos, !(ctx.lc_extra))
//
in
//
case+ s2e.s2e_node of
//
| S2Ecst (s2c)
    when symbol_get_name (s2cst_get_name (s2c)) = "true" => ()
//
| S2Eapp (s2c, args) => let
    val name = symbol_get_name (s2cst_get_name (s2c))
  in
    case+ args of
    | list0_cons (l, list0_cons (r, list0_nil ()))
        when name = "&&" => let
        val () = check_goal (ctx, hypos, origs, c3t, l)
      in
        check_goal (ctx, hypos, origs, c3t, r)
      end
    //
    | list0_cons (k, list0_cons (n, list0_nil ()))
        when name = "|" => (
        (* divisibility goal *)
        case+ (lower_num (ctx, k), lower_num (ctx, n)) of
        | (LFok fk, LFok fn_) =>
            if lf_is_const (fk) then let
              val d = fk.lf_c * fn_.lf_den (* n = N/D: k | n iff D*k | N *)
              val dv = (if fk.lf_den = 1 then d else 0): int
            in
              if dv = 0 then report_unsolved
                (c3t, s2e, origs, "the divisor must be an integer constant")
              else if divides_prove
                (ctx, allhypos (ctx, hypos), dv, fn_) then ()
              else report_unsolved (c3t, s2e, origs, "")
            end
            else report_unsolved
              (c3t, s2e, origs, "the divisor must be a constant")
        | (_, _) => report_unsolved
            (c3t, s2e, origs, "nonlinear divisibility")
      )
    //
    | list0_cons (e, list0_nil ())
        when name = "isint" => (
        case+ lower_num (ctx, e) of
        | LFok f =>
            if f.lf_den = 1 then ()
            else if divides_prove
              (ctx, allhypos (ctx, hypos), f.lf_den, lf_numer (f)) then ()
            else report_unsolved (c3t, s2e, origs, "")
        | LFerr (l0, w) => report_unsolved (c3t, s2e, origs, w)
      )
    //
    | list0_cons (l, list0_cons (r, list0_nil ()))
        when (name = "==") andalso
          (case+ l.s2e_srt of S2RTclock () => true | _ => false) => (
        (* split clock equality for better messages *)
        case+ (lower_clk (ctx, l), lower_clk (ctx, r)) of
        | (CLok (n1, d1), CLok (n2, d2)) => let
            fn chk
              (what: string, f1: lf, f2: lf): void = let
              val negs = relvec ("!=", f1, f2)
            in
              if refute (list0_append (negs, allhypos (ctx, hypos)))
                then ()
                else report_unsolved (c3t, s2e, origs, what)
            end
            val () = chk ("the periods differ", n1, n2)
          in
            chk ("the phases differ", d1, d2)
          end
        | (CLerr (l0, w), _) => report_unsolved (c3t, s2e, origs, w)
        | (_, CLerr (l0, w)) => report_unsolved (c3t, s2e, origs, w)
      )
    //
    | _ (*rest*) => (
        case+ goal_negate (ctx, s2e) of
        | Some0 (negs) =>
            if refute (list0_append (negs, allhypos (ctx, hypos)))
              then ()
              else report_unsolved (c3t, s2e, origs, "")
        | None0 () =>
            report_unsolved (c3t, s2e, origs, "unsupported goal shape")
      )
  end
//
| _ (*rest*) =>
    report_unsolved (c3t, s2e, origs, "unsupported goal shape")
//
end // end of [check_goal]

(* ****** ****** *)

fun
solve_c3nstr
(
  ctx: lowctx
, hypos: list0(icl), origs: list0(horig)
, c3t: c3nstr
) : void =
(
case+ c3t.c3nstr_node of
| C3NSTRprop (s2e) =>
    check_goal (ctx, hypos, origs, c3t, s2e)
| C3NSTRitmlst (items) => let
    fun loop
      (ctx: lowctx
      , hypos: list0(icl), origs: list0(horig)
      , items: s3itmlst): void =
      case+ items of
      | list0_nil () => ()
      | list0_cons (itm, items) => (
          case+ itm of
          | S3ITMsvar _ =>
              (* solver variables are created on demand *)
              loop (ctx, hypos, origs, items)
          | S3ITMhypo (H3YPOprop (loc, s2e)) => let
              val lowered = lower_hypo (ctx, s2e)
              val hypos = list0_append (lowered, hypos)
              val origs = list0_append
                (origs, list0_cons (@(loc, s2e), list0_nil ()))
            in
              loop (ctx, hypos, origs, items)
            end
          | S3ITMcnstr (sub) => let
              val () = solve_c3nstr (ctx, hypos, origs, sub)
            in
              loop (ctx, hypos, origs, items)
            end
        )
  in
    loop (ctx, hypos, origs, items)
  end
) (* end of [solve_c3nstr] *)

implement
solver_solve (cs) = let
//
fun loop (cs: list0(c3nstr)): void =
  case+ cs of
  | list0_nil () => ()
  | list0_cons (c3t, cs) => let
      val ctx = lowctx_new ()
      val () =
        solve_c3nstr (ctx, list0_nil (), list0_nil (), c3t)
    in
      loop (cs)
    end
//
in
  loop (cs)
end // end of [solver_solve]

(* ****** ****** *)

(* end of [overture_solver.dats] *)
