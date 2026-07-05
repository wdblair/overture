(*
** Overture: resolved statics
*)

#include "share/atspre_staload.hats"

staload "libats/ML/SATS/basis.sats"
staload "libats/ML/SATS/list0.sats"
staload _ = "libats/ML/DATS/list0.dats"

staload "./overture_symbol.sats"
staload "./overture_location.sats"
staload "./overture_staexp.sats"

(* ****** ****** *)

implement
fprint_s2rt (out, srt) =
(
  case+ srt of
  | S2RTtype () => fprint! (out, "type")
  | S2RTclock () => fprint! (out, "clock")
  | S2RTint () => fprint! (out, "int")
  | S2RTrat () => fprint! (out, "rat")
  | S2RTbool () => fprint! (out, "bool")
  | S2RTkind () => fprint! (out, "kind")
  | S2RTerr () => fprint! (out, "ERR")
) (* end of [fprint_s2rt] *)

implement
eq_s2rt_s2rt (srt1, srt2) =
(
  case+ (srt1, srt2) of
  | (S2RTtype (), S2RTtype ()) => true
  | (S2RTclock (), S2RTclock ()) => true
  | (S2RTint (), S2RTint ()) => true
  | (S2RTrat (), S2RTrat ()) => true
  | (S2RTbool (), S2RTbool ()) => true
  | (S2RTkind (), S2RTkind ()) => true
  | (_, _) => false
) (* end of [eq_s2rt_s2rt] *)

implement
s2rt_leq (srt1, srt2) =
(
  case+ (srt1, srt2) of
  | (S2RTint (), S2RTrat ()) => true (* int <= rat *)
  | (_, _) => eq_s2rt_s2rt (srt1, srt2)
) (* end of [s2rt_leq] *)

(* ****** ****** *)

val the_stamp = ref<int> (0)

implement
stamp_new () = let
  val n = the_stamp[]
  val () = the_stamp[] := n + 1
in
  n
end // end of [stamp_new]

(* ****** ****** *)

datatype
s2var_ = S2VAR of (symbol, s2rt, int(*stamp*))

assume s2var = s2var_

implement
s2var_make (sym, srt) =
  S2VAR (sym, srt, stamp_new ())

implement
s2var_get_sym (s2v) =
  let val S2VAR (sym, _, _) = s2v in sym end
implement
s2var_get_srt (s2v) =
  let val S2VAR (_, srt, _) = s2v in srt end
implement
s2var_get_stamp (s2v) =
  let val S2VAR (_, _, stamp) = s2v in stamp end

implement
eq_s2var_s2var (s2v1, s2v2) =
  s2var_get_stamp (s2v1) = s2var_get_stamp (s2v2)

(* ****** ****** *)
//
// message-local disambiguation of colliding variable names
//
val the_dsptbl = ref<list0(@(int(*stamp*), int(*idx*)))> (list0_nil ())

implement
the_dsptbl_clear () =
  the_dsptbl[] := list0_nil ()

implement
the_dsptbl_set (s2vs) = let
//
fun mem
  (s2vs: list0(s2var), s2v: s2var): bool =
  case+ s2vs of
  | list0_nil () => false
  | list0_cons (s2w, s2vs) =>
      if eq_s2var_s2var (s2v, s2w) then true else mem (s2vs, s2v)
//
fun dedup
  (s2vs: list0(s2var), seen: list0(s2var)): list0(s2var) =
  case+ s2vs of
  | list0_nil () => list0_reverse (seen)
  | list0_cons (s2v, s2vs) =>
      if mem (seen, s2v)
        then dedup (s2vs, seen)
        else dedup (s2vs, list0_cons (s2v, seen))
      // end of [if]
//
val s2vs = dedup (s2vs, list0_nil ())
//
fun samename
  (s2vs: list0(s2var), s2v: s2var): int =
  case+ s2vs of
  | list0_nil () => 0
  | list0_cons (s2w, s2vs) =>
      if s2var_get_sym (s2w) = s2var_get_sym (s2v)
        then 1 + samename (s2vs, s2v) else samename (s2vs, s2v)
      // end of [if]
//
// number the collisions in order of appearance, starting from 1
//
fun assign
  (rest: list0(s2var), before: list0(s2var)): void =
  case+ rest of
  | list0_nil () => ()
  | list0_cons (s2v, rest) => let
      val () =
        if samename (s2vs, s2v) > 1 then let
          val idx = samename (before, s2v) + 1
        in
          the_dsptbl[] := list0_cons
            (@(s2var_get_stamp (s2v), idx), the_dsptbl[])
        end // end of [if]
    in
      assign (rest, list0_cons (s2v, before))
    end
//
val () = the_dsptbl[] := list0_nil ()
//
in
  assign (s2vs, list0_nil ())
end // end of [the_dsptbl_set]

(*
** Unicode subscript digits: U+2080 + d is E2 82 (80+d) in UTF-8
*)
fun
fprint_subscript
  (out: FILEref, i: int): void = let
  val () = if i >= 10 then fprint_subscript (out, i / 10)
  val d = i mod 10
  val () = fprint! (out, int2char0 (0xE2))
  val () = fprint! (out, int2char0 (0x82))
  val () = fprint! (out, int2char0 (0x80 + d))
in
  // nothing
end // end of [fprint_subscript]

implement
fprint_s2var (out, s2v) = let
  val () = fprint! (out, symbol_get_name (s2var_get_sym (s2v)))
  val stamp = s2var_get_stamp (s2v)
  fun find
    (kxs: list0(@(int, int))): option0 (int) =
    case+ kxs of
    | list0_nil () => None0 ()
    | list0_cons (kx, kxs) =>
        if kx.0 = stamp then Some0 (kx.1) else find (kxs)
in
  case+ find (the_dsptbl[]) of
  | Some0 (idx) => fprint_subscript (out, idx)
  | None0 () => ()
end // end of [fprint_s2var]

(* ****** ****** *)

datatype
s2cst_ = S2CST of (symbol, list0(s2rt), s2rt, int(*stamp*))

assume s2cst = s2cst_

implement
s2cst_make (name, prms, res) =
  S2CST (name, prms, res, stamp_new ())

implement
s2cst_get_name (s2c) =
  let val S2CST (name, _, _, _) = s2c in name end
implement
s2cst_get_prms (s2c) =
  let val S2CST (_, prms, _, _) = s2c in prms end
implement
s2cst_get_res (s2c) =
  let val S2CST (_, _, res, _) = s2c in res end
implement
s2cst_get_stamp (s2c) =
  let val S2CST (_, _, _, stamp) = s2c in stamp end

implement
eq_s2cst_s2cst (s2c1, s2c2) =
  s2cst_get_stamp (s2c1) = s2cst_get_stamp (s2c2)

(* ****** ****** *)

implement
s2exp_make (loc, srt, node) =
  '{ s2e_loc= loc, s2e_srt= srt, s2e_node= node }

implement
s2exp_int (loc, i) =
  s2exp_make (loc, S2RTint (), S2Eint (i))

implement
s2exp_var (loc, s2v) =
  s2exp_make (loc, s2var_get_srt (s2v), S2Evar (s2v))

(* ****** ****** *)

fun gcd (a: int, b: int): int =
  if b = 0 then (if a >= 0 then a else ~a) else gcd (b, a mod b)

implement
s2exp_rat (loc, a, b) = let
//
val (a, b) = (if b < 0 then (~a, ~b) else (a, b)): @(int, int)
val g = gcd (a, b)
val g = (if g = 0 then 1 else g): int
//
in
  s2exp_make (loc, S2RTrat (), S2Erat (a / g, b / g))
end // end of [s2exp_rat]

(* ****** ****** *)

implement
s2exp_bool (loc, b) =
  if b
    then s2exp_make (loc, S2RTbool (), S2Ecst (s2cst_true ()))
    else s2exp_make (loc, S2RTbool (), S2Ecst (s2cst_false ()))
  // end of [if]

(* ****** ****** *)

implement
fprint_s2exp
  (out, s2e) =
(
case+ s2e.s2e_node of
| S2Eint (i) => fprint! (out, i)
| S2Erat (a, b) =>
    if b = 1 then fprint! (out, a) else fprint! (out, a, "/", b)
| S2Evar (s2v) => fprint_s2var (out, s2v)
| S2Ecst (s2c) => fprint_symbol (out, s2cst_get_name (s2c))
| S2Eapp (s2c, args) => let
    val name = symbol_get_name (s2cst_get_name (s2c))
  in
    case+ args of
    | list0_cons (l, list0_cons (r, list0_nil ()))
        when name = "clk" => let
        (* clock literals print as (n, p) *)
        val () = fprint! (out, "(")
        val () = fprint_s2exp (out, l)
        val () = fprint! (out, ", ")
        val () = fprint_s2exp (out, r)
      in
        fprint! (out, ")")
      end
    | list0_cons (l, list0_cons (r, list0_nil ())) => let
        (* binary constants print infix *)
        val () = fprint! (out, "(")
        val () = fprint_s2exp (out, l)
        val () = fprint! (out, " ", name, " ")
        val () = fprint_s2exp (out, r)
      in
        fprint! (out, ")")
      end
    | _ (*rest*) => let
        val () = fprint! (out, name, "(")
        fun loop
          (out: FILEref, args: s2explst): void =
          case+ args of
          | list0_nil () => ()
          | list0_cons (arg, args) => let
              val () = fprint_s2exp (out, arg)
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
  end
) (* end of [fprint_s2exp] *)

(* ****** ****** *)

implement
s2exp_collect_svars
  (s2e, acc) = let
//
fun mem
  (s2vs: list0(s2var), s2v: s2var): bool =
  case+ s2vs of
  | list0_nil () => false
  | list0_cons (s2w, s2vs) =>
      if eq_s2var_s2var (s2v, s2w) then true else mem (s2vs, s2v)
//
in
//
case+ s2e.s2e_node of
| S2Eint _ => acc
| S2Erat _ => acc
| S2Ecst _ => acc
| S2Evar (s2v) =>
    if mem (acc, s2v) then acc else list0_cons (s2v, acc)
| S2Eapp (_, args) => let
    fun loop
      (args: s2explst, acc: list0(s2var)): list0(s2var) =
      case+ args of
      | list0_nil () => acc
      | list0_cons (arg, args) =>
          loop (args, s2exp_collect_svars (arg, acc))
  in
    loop (args, acc)
  end
//
end // end of [s2exp_collect_svars]

(* ****** ****** *)

implement
s2exp_subst
  (s2e, sub) = let
//
fun assoc
  (sub: s2sub, s2v: s2var): option0 (s2exp) =
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
case+ s2e.s2e_node of
| S2Eint _ => s2e
| S2Erat _ => s2e
| S2Ecst _ => s2e
| S2Evar (s2v) => (
    case+ assoc (sub, s2v) of
    | Some0 (s2f) => s2f
    | None0 () => s2e
  )
| S2Eapp (s2c, args) =>
    s2exp_make
      (s2e.s2e_loc, s2e.s2e_srt, S2Eapp (s2c, s2explst_subst (args, sub)))
//
end // end of [s2exp_subst]

implement
s2explst_subst
  (s2es, sub) =
(
  case+ s2es of
  | list0_nil () => list0_nil ()
  | list0_cons (s2e, s2es) =>
      list0_cons (s2exp_subst (s2e, sub), s2explst_subst (s2es, sub))
) (* end of [s2explst_subst] *)

(* ****** ****** *)

implement
t2ype_subst
  (t2p, sub) =
(
case+ t2p of
| T2YPEbase _ => t2p
| T2YPEvar _ => t2p (* payload type vars: substituted structurally in trans3 *)
| T2YPEsta (s2e) => T2YPEsta (s2exp_subst (s2e, sub))
| T2YPErate (knd, elt, clk) =>
    T2YPErate
      (s2exp_subst (knd, sub), t2ype_subst (elt, sub), s2exp_subst (clk, sub))
| T2YPEexi (s2vs, gua, body) =>
    T2YPEexi (s2vs, s2exp_subst (gua, sub), t2ype_subst (body, sub))
| T2YPEtup (t2ps) => let
    fun loop (t2ps: t2ypelst): t2ypelst =
      case+ t2ps of
      | list0_nil () => list0_nil ()
      | list0_cons (t2p, t2ps) =>
          list0_cons (t2ype_subst (t2p, sub), loop (t2ps))
  in
    T2YPEtup (loop (t2ps))
  end
| T2YPEerr () => t2p
) (* end of [t2ype_subst] *)

(* ****** ****** *)

implement
fprint_t2ype
  (out, t2p) =
(
case+ t2p of
| T2YPEbase (sym) => fprint_symbol (out, sym)
| T2YPEvar (s2v) => fprint_s2var (out, s2v)
| T2YPEsta (s2e) => let
    val () = (
      case+ s2e.s2e_srt of
      | S2RTrat () => fprint! (out, "rat(")
      | _ (*int*) => fprint! (out, "int(")
    ) : void
    val () = fprint_s2exp (out, s2e)
  in
    fprint! (out, ")")
  end
| T2YPErate (knd, elt, clk) => let
    val () = fprint! (out, "rate(")
    val isstrict = (
      case+ knd.s2e_node of
      | S2Ecst (s2c) => eq_s2cst_s2cst (s2c, s2cst_strict ())
      | _ (*rest*) => false
    ) : bool
    val () =
      if not (isstrict) then
        (fprint_s2exp (out, knd); fprint! (out, ", "))
    val () = fprint_t2ype (out, elt)
    val () = fprint! (out, ", ")
    val () = fprint_s2exp (out, clk)
  in
    fprint! (out, ")")
  end
| T2YPEexi (s2vs, gua, body) => let
    val () = fprint! (out, "[")
    fun loop
      (out: FILEref, s2vs: list0(s2var)): void =
      case+ s2vs of
      | list0_nil () => ()
      | list0_cons (s2v, s2vs) => let
          val () = fprint_s2var (out, s2v)
          val () = fprint! (out, ":")
          val () = fprint_s2rt (out, s2var_get_srt (s2v))
          val () =
            (case+ s2vs of
             | list0_nil () => ()
             | list0_cons _ => fprint! (out, "; "))
        in
          loop (out, s2vs)
        end
    val () = loop (out, s2vs)
    val () = fprint! (out, " | ")
    val () = fprint_s2exp (out, gua)
    val () = fprint! (out, "] ")
  in
    fprint_t2ype (out, body)
  end
| T2YPEtup (t2ps) => let
    val () = fprint! (out, "(")
    fun loop
      (out: FILEref, t2ps: t2ypelst): void =
      case+ t2ps of
      | list0_nil () => ()
      | list0_cons (t2p, t2ps) => let
          val () = fprint_t2ype (out, t2p)
          val () =
            (case+ t2ps of
             | list0_nil () => ()
             | list0_cons _ => fprint! (out, ", "))
        in
          loop (out, t2ps)
        end
    val () = loop (out, t2ps)
  in
    fprint! (out, ")")
  end
| T2YPEerr () => fprint! (out, "ERR")
) (* end of [fprint_t2ype] *)

(* ****** ****** *)

implement
fprint_n2odesig
  (out, nsig) = let
//
val () = fprint_symbol (out, nsig.n2sig_name)
//
val tvs = nsig.n2sig_tvars
val () = (
  case+ tvs of
  | list0_nil () => ()
  | list0_cons _ => let
      val () = fprint! (out, " {")
      fun loop
        (out: FILEref, s2vs: list0(s2var)): void =
        case+ s2vs of
        | list0_nil () => ()
        | list0_cons (s2v, s2vs) => let
            val () = fprint_s2var (out, s2v)
            val () =
              (case+ s2vs of
               | list0_nil () => ()
               | list0_cons _ => fprint! (out, "; "))
          in
            loop (out, s2vs)
          end
      val () = loop (out, tvs)
    in
      fprint! (out, ":type}")
    end
) : void // end of [val]
//
val uvs = nsig.n2sig_uvars
val () = (
  case+ uvs of
  | list0_nil () => ()
  | list0_cons _ => let
      val () = fprint! (out, " {")
      fun loop
        (out: FILEref, s2vs: list0(s2var)): void =
        case+ s2vs of
        | list0_nil () => ()
        | list0_cons (s2v, s2vs) => let
            val () = fprint_s2var (out, s2v)
            val () = fprint! (out, ":")
            val () = fprint_s2rt (out, s2var_get_srt (s2v))
            val () =
              (case+ s2vs of
               | list0_nil () => ()
               | list0_cons _ => fprint! (out, "; "))
          in
            loop (out, s2vs)
          end
      val () = loop (out, uvs)
      val () = fprint! (out, " | ")
      val () = fprint_s2exp (out, nsig.n2sig_guard)
    in
      fprint! (out, "}")
    end
) : void // end of [val]
//
val () = fprint! (out, " (")
fun loop
  (out: FILEref, t2ps: t2ypelst): void =
  case+ t2ps of
  | list0_nil () => ()
  | list0_cons (t2p, t2ps) => let
      val () = fprint_t2ype (out, t2p)
      val () =
        (case+ t2ps of
         | list0_nil () => ()
         | list0_cons _ => fprint! (out, ", "))
    in
      loop (out, t2ps)
    end
val () = loop (out, nsig.n2sig_params)
val () = fprint! (out, ") -> ")
val () = fprint_t2ype (out, nsig.n2sig_res)
//
in
  case+ nsig.n2sig_wcet of
  | Some0 (w) =>
      (fprint! (out, " wcet "); fprint_s2exp (out, w))
  | None0 () => ()
end // end of [fprint_n2odesig]

(* ****** ****** *)
//
// the built-in constant tables
//
val the_s2csttbl = ref<list0(s2cst)> (list0_nil ())
val the_builtintbl = ref<list0(n2odesig)> (list0_nil ())
//
val the_cst_true = ref<option0(s2cst)> (None0 ())
val the_cst_false = ref<option0(s2cst)> (None0 ())
val the_cst_clk = ref<option0(s2cst)> (None0 ())
val the_cst_over = ref<option0(s2cst)> (None0 ())
val the_cst_under = ref<option0(s2cst)> (None0 ())
val the_cst_shift = ref<option0(s2cst)> (None0 ())
val the_cst_divides = ref<option0(s2cst)> (None0 ())
val the_cst_strict = ref<option0(s2cst)> (None0 ())
val the_cst_gated = ref<option0(s2cst)> (None0 ())
//
(* ****** ****** *)

fn cst_get
  (r: ref(option0(s2cst))): s2cst =
(
  case+ !r of
  | Some0 (s2c) => s2c
  | None0 () => let
      val () = prerrln! ("overture: INTERNAL: statics not initialized")
    in
      exit (1)
    end
) (* end of [cst_get] *)

implement s2cst_true () = cst_get (the_cst_true)
implement s2cst_false () = cst_get (the_cst_false)
implement s2cst_clk () = cst_get (the_cst_clk)
implement s2cst_over () = cst_get (the_cst_over)
implement s2cst_under () = cst_get (the_cst_under)
implement s2cst_shift () = cst_get (the_cst_shift)
implement s2cst_divides () = cst_get (the_cst_divides)
implement s2cst_strict () = cst_get (the_cst_strict)
implement s2cst_gated () = cst_get (the_cst_gated)

(* ****** ****** *)

implement
the_s2csttbl_search
  (name, argsrts) = let
//
fun accepts_exact
  (prms: list0(s2rt), args: list0(s2rt)): bool =
(
  case+ (prms, args) of
  | (list0_nil (), list0_nil ()) => true
  | (list0_cons (p, prms), list0_cons (a, args)) =>
      if eq_s2rt_s2rt (a, p) then accepts_exact (prms, args) else false
  | (_, _) => false
) (* end of [accepts_exact] *)
//
fun accepts
  (prms: list0(s2rt), args: list0(s2rt)): bool =
(
  case+ (prms, args) of
  | (list0_nil (), list0_nil ()) => true
  | (list0_cons (p, prms), list0_cons (a, args)) =>
      if s2rt_leq (a, p) then accepts (prms, args) else false
  | (_, _) => false
) (* end of [accepts] *)
//
(* exact sort matches take priority over int-to-rat coercion *)
fun search
  (csts: list0(s2cst), exact: bool): option0 (s2cst) =
(
  case+ csts of
  | list0_nil () => None0 ()
  | list0_cons (s2c, csts) =>
      if s2cst_get_name (s2c) = name
        then (
          if (if exact
                then accepts_exact (s2cst_get_prms (s2c), argsrts)
                else accepts (s2cst_get_prms (s2c), argsrts))
            then Some0 (s2c) else search (csts, exact)
        ) else search (csts, exact)
      // end of [if]
) (* end of [search] *)
//
in
  case+ search (the_s2csttbl[], true) of
  | Some0 (s2c) => Some0 (s2c)
  | None0 () => search (the_s2csttbl[], false)
end // end of [the_s2csttbl_search]

implement
the_builtintbl_search
  (name) = let
//
fun search
  (sigs: list0(n2odesig)): option0 (n2odesig) =
(
  case+ sigs of
  | list0_nil () => None0 ()
  | list0_cons (nsig, sigs) =>
      if nsig.n2sig_name = name
        then Some0 (nsig) else search (sigs)
      // end of [if]
) (* end of [search] *)
//
in
  search (the_builtintbl[])
end // end of [the_builtintbl_search]

(* ****** ****** *)

implement
staexp_initialize
  ((*void*)) = let
//
val loc0 = location_dummy ()
//
fn mkcst
(
  name: string, prms: list0(s2rt), res: s2rt
) : s2cst = let
  val s2c = s2cst_make (symbol_make (name), prms, res)
  val () = the_s2csttbl[] := list0_cons (s2c, the_s2csttbl[])
in
  s2c
end // end of [mkcst]
//
fn l0 (): list0(s2rt) = list0_nil ()
fn l1 (s1: s2rt): list0(s2rt) =
  list0_cons (s1, list0_nil ())
fn l2 (s1: s2rt, s2: s2rt): list0(s2rt) =
  list0_cons (s1, list0_cons (s2, list0_nil ()))
//
val int = S2RTint () and rat = S2RTrat ()
val bool = S2RTbool () and clock = S2RTclock ()
//
// sort-level constants
//
val cst_true = mkcst ("true", l0 (), bool)
val cst_false = mkcst ("false", l0 (), bool)
//
val cst_strict = mkcst ("strict", l0 (), S2RTkind ())
val cst_gated = mkcst ("gated", l0 (), S2RTkind ())
//
val cst_clk = mkcst ("clk", l2 (int, rat), clock)
val cst_over = mkcst ("*/", l2 (clock, int), clock)
val cst_under = mkcst ("^/", l2 (clock, int), clock)
val cst_shift = mkcst ("shift", l2 (clock, rat), clock)
//
val _period = mkcst ("period", l1 (clock), int)
val _date = mkcst ("date", l1 (clock), int)
val _isint = mkcst ("isint", l1 (rat), bool)
//
val _addi = mkcst ("+", l2 (int, int), int)
val _subi = mkcst ("-", l2 (int, int), int)
val _muli = mkcst ("*", l2 (int, int), int)
val _addr = mkcst ("+", l2 (rat, rat), rat)
val _subr = mkcst ("-", l2 (rat, rat), rat)
val _mulr = mkcst ("*", l2 (rat, rat), rat)
val _divi = mkcst ("/", l2 (int, int), rat)
val _divr = mkcst ("/", l2 (rat, rat), rat)
//
val _eqi = mkcst ("==", l2 (int, int), bool)
val _neqi = mkcst ("!=", l2 (int, int), bool)
val _lti = mkcst ("<", l2 (int, int), bool)
val _ltei = mkcst ("<=", l2 (int, int), bool)
val _gti = mkcst (">", l2 (int, int), bool)
val _gtei = mkcst (">=", l2 (int, int), bool)
val _eqr = mkcst ("==", l2 (rat, rat), bool)
val _neqr = mkcst ("!=", l2 (rat, rat), bool)
val _ltr = mkcst ("<", l2 (rat, rat), bool)
val _lter = mkcst ("<=", l2 (rat, rat), bool)
val _gtr = mkcst (">", l2 (rat, rat), bool)
val _gter = mkcst (">=", l2 (rat, rat), bool)
//
val _eqc = mkcst ("==", l2 (clock, clock), bool)
val _neqc = mkcst ("!=", l2 (clock, clock), bool)
//
val cst_divides = mkcst ("|", l2 (int, int), bool)
//
val _andb = mkcst ("&&", l2 (bool, bool), bool)
val _orb = mkcst ("||", l2 (bool, bool), bool)
//
val () = the_cst_true[] := Some0 (cst_true)
val () = the_cst_false[] := Some0 (cst_false)
val () = the_cst_clk[] := Some0 (cst_clk)
val () = the_cst_over[] := Some0 (cst_over)
val () = the_cst_under[] := Some0 (cst_under)
val () = the_cst_shift[] := Some0 (cst_shift)
val () = the_cst_divides[] := Some0 (cst_divides)
val () = the_cst_strict[] := Some0 (cst_strict)
val () = the_cst_gated[] := Some0 (cst_gated)
//
// the built-in term operators, as node signatures
//
fn strue (): s2exp = s2exp_bool (loc0, true)
//
fn kstrict (): s2exp =
  s2exp_make (loc0, S2RTkind (), S2Ecst (cst_strict))
fn kgated (): s2exp =
  s2exp_make (loc0, S2RTkind (), S2Ecst (cst_gated))
fn rt (a: t2ype, c: s2exp): t2ype = T2YPErate (kstrict (), a, c)
fn gt (a: t2ype, c: s2exp): t2ype = T2YPErate (kgated (), a, c)
//
fn v2exp (s2v: s2var): s2exp = s2exp_var (loc0, s2v)
//
fn app2
(
  s2c: s2cst, e1: s2exp, e2: s2exp, res: s2rt
) : s2exp =
  s2exp_make
    (loc0, res, S2Eapp (s2c, list0_cons (e1, list0_cons (e2, list0_nil ()))))
fn app1
  (s2c: s2cst, e1: s2exp, res: s2rt): s2exp =
  s2exp_make (loc0, res, S2Eapp (s2c, list0_cons (e1, list0_nil ())))
//
fn mksig
(
  name: string
, tvars: list0(s2var), uvars: list0(s2var)
, guard: s2exp, params: t2ypelst, res: t2ype
) : void = let
  val nsig = '{
    n2sig_name= symbol_make (name)
  , n2sig_tvars= tvars, n2sig_uvars= uvars
  , n2sig_guard= guard, n2sig_params= params, n2sig_res= res
  , n2sig_wcet= None0 ()
  } : n2odesig
in
  the_builtintbl[] := list0_cons (nsig, the_builtintbl[])
end // end of [mksig]
//
fn lt1 (t: t2ype): t2ypelst = list0_cons (t, list0_nil ())
fn lt2 (t1: t2ype, t2: t2ype): t2ypelst =
  list0_cons (t1, list0_cons (t2, list0_nil ()))
fn lt3 (t1: t2ype, t2: t2ype, t3: t2ype): t2ypelst =
  list0_cons (t1, list0_cons (t2, list0_cons (t3, list0_nil ())))
fn lv1 (v: s2var): list0(s2var) = list0_cons (v, list0_nil ())
fn lv2 (v1: s2var, v2: s2var): list0(s2var) =
  list0_cons (v1, list0_cons (v2, list0_nil ()))
//
val sym_a = symbol_make ("a")
val sym_c = symbol_make ("c")
val sym_k = symbol_make ("k")
//
// */ : {a:type}{c:clock; k:int | k > 0 && k | period(c)}
//      (rate(a, c), int(k)) -> rate(a, c */ k)
//
val () = let
  val a = s2var_make (sym_a, S2RTtype ())
  val c = s2var_make (sym_c, clock)
  val k = s2var_make (sym_k, int)
  val gt = the_s2csttbl_search (symbol_make (">"), l2 (int, int))
  val- Some0 (cst_gt) = gt
  val g1 = app2 (cst_gt, v2exp (k), s2exp_int (loc0, 0), bool)
  val prd = app1 (_period, v2exp (c), int)
  val g2 = app2 (cst_divides, v2exp (k), prd, bool)
  val g = app2 (_andb, g1, g2, bool)
  val resclk = app2 (cst_over, v2exp (c), v2exp (k), clock)
in
  mksig ("*/", lv1 (a), lv2 (c, k), g
  , lt2 (rt (T2YPEvar (a), v2exp (c)), T2YPEsta (v2exp (k)))
  , rt (T2YPEvar (a), resclk))
end // end of [val]
//
// ^/ : {a:type}{c:clock; k:int | k > 0}
//      (rate(a, c), int(k)) -> rate(a, c ^/ k)
//
// undersampling multiplies the period, (n, d) -> (n*k, d), so the
// lowered clock is integral for any positive k; only oversampling
// (which divides the period) carries a divisibility guard.
//
val () = let
  val a = s2var_make (sym_a, S2RTtype ())
  val c = s2var_make (sym_c, clock)
  val k = s2var_make (sym_k, int)
  val gt = the_s2csttbl_search (symbol_make (">"), l2 (int, int))
  val- Some0 (cst_gt) = gt
  val g = app2 (cst_gt, v2exp (k), s2exp_int (loc0, 0), bool)
  val resclk = app2 (cst_under, v2exp (c), v2exp (k), clock)
in
  mksig ("^/", lv1 (a), lv2 (c, k), g
  , lt2 (rt (T2YPEvar (a), v2exp (c)), T2YPEsta (v2exp (k)))
  , rt (T2YPEvar (a), resclk))
end // end of [val]
//
// shift : {a:type}{c:clock; k:rat | isint(k * period(c))}
//         (rate(a, c), rat(k)) -> rate(a, shift(c, k))
//
val () = let
  val a = s2var_make (sym_a, S2RTtype ())
  val c = s2var_make (sym_c, clock)
  val k = s2var_make (sym_k, rat)
  val prd = app1 (_period, v2exp (c), int)
  val prod = app2 (_mulr, v2exp (k), prd, rat)
  val g = app1 (_isint, prod, bool)
  val resclk = app2 (cst_shift, v2exp (c), v2exp (k), clock)
in
  mksig ("shift", lv1 (a), lv2 (c, k), g
  , lt2 (rt (T2YPEvar (a), v2exp (c)), T2YPEsta (v2exp (k)))
  , rt (T2YPEvar (a), resclk))
end // end of [val]
//
// fby : {a:type}{c:clock} (a, rate(a, c)) -> rate(a, c)
//
val () = let
  val a = s2var_make (sym_a, S2RTtype ())
  val c = s2var_make (sym_c, clock)
in
  mksig ("fby", lv1 (a), lv1 (c), strue ()
  , lt2 (T2YPEvar (a), rt (T2YPEvar (a), v2exp (c)))
  , rt (T2YPEvar (a), v2exp (c)))
end // end of [val]
//
// cons : {a:type}{c:clock | date(c) >= period(c)}
//        (a, rate(a, c)) -> rate(a, shift(c, -1))
//
val () = let
  val a = s2var_make (sym_a, S2RTtype ())
  val c = s2var_make (sym_c, clock)
  val gte = the_s2csttbl_search (symbol_make (">="), l2 (int, int))
  val- Some0 (cst_gte) = gte
  val dat = app1 (_date, v2exp (c), int)
  val prd = app1 (_period, v2exp (c), int)
  val g = app2 (cst_gte, dat, prd, bool)
  val m1 = s2exp_rat (loc0, ~1, 1)
  val resclk = app2 (cst_shift, v2exp (c), m1, clock)
in
  mksig ("cons", lv1 (a), lv1 (c), g
  , lt2 (T2YPEvar (a), rt (T2YPEvar (a), v2exp (c)))
  , rt (T2YPEvar (a), resclk))
end // end of [val]
//
// tail : {a:type}{c:clock} rate(a, c) -> rate(a, shift(c, 1))
//
val () = let
  val a = s2var_make (sym_a, S2RTtype ())
  val c = s2var_make (sym_c, clock)
  val p1 = s2exp_rat (loc0, 1, 1)
  val resclk = app2 (cst_shift, v2exp (c), p1, clock)
in
  mksig ("tail", lv1 (a), lv1 (c), strue ()
  , lt1 (rt (T2YPEvar (a), v2exp (c)))
  , rt (T2YPEvar (a), resclk))
end // end of [val]
//
// merge : {a:type}{c:clock}
//         (rate(bool, c), rate(a, c), rate(a, c)) -> rate(a, c)
//
val () = let
  val a = s2var_make (sym_a, S2RTtype ())
  val c = s2var_make (sym_c, clock)
  val tbool = T2YPEbase (symbol_make ("bool"))
in
  mksig ("merge", lv1 (a), lv1 (c), strue ()
  , lt3 ( rt (tbool, v2exp (c))
        , rt (T2YPEvar (a), v2exp (c))
        , rt (T2YPEvar (a), v2exp (c)))
  , rt (T2YPEvar (a), v2exp (c)))
end // end of [val]
//
// when : {a:type}{c:clock}
//        (rate(a, c), rate(bool, c)) -> brate(a, c)
// (infix: f when b; gated flows are terminal in the clock calculus)
//
val () = let
  val a = s2var_make (sym_a, S2RTtype ())
  val c = s2var_make (sym_c, clock)
  val tbool = T2YPEbase (symbol_make ("bool"))
in
  mksig ("when", lv1 (a), lv1 (c), strue ()
  , lt2 ( rt (T2YPEvar (a), v2exp (c))
        , rt (tbool, v2exp (c)))
  , gt (T2YPEvar (a), v2exp (c)))
end // end of [val]
//
// current : {a:type}{c:clock}
//           (a, brate(a, c)) -> rate(a, c)
// (holds the last present value; the head initializes)
//
val () = let
  val a = s2var_make (sym_a, S2RTtype ())
  val c = s2var_make (sym_c, clock)
in
  mksig ("current", lv1 (a), lv1 (c), strue ()
  , lt2 (T2YPEvar (a), gt (T2YPEvar (a), v2exp (c)))
  , rt (T2YPEvar (a), v2exp (c)))
end // end of [val]
//
in
  // nothing
end // end of [staexp_initialize]

(* ****** ****** *)

(* end of [overture_staexp.dats] *)
