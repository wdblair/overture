(*
** Overture: resolved statics
**
** Modeled on pats_staexp2.sats. There are no per-operation
** constructors: clock operations, arithmetic, and relations are all
** applications (S2Eapp) of built-in static constants (s2cst), each
** carrying a sorted signature. The constraint solver interprets the
** known constants; anything else falls back to congruence.
**
** Sorts: type (payload types), clock, int, rat, bool.
** A clock denotes a pair (period n : int, phase p : rat) subject to
** the validity condition n * p being a natural number; the solver
** lowers every clock to the integer pair (period, activation date).
*)

staload "libats/ML/SATS/basis.sats"
staload "libats/ML/SATS/list0.sats"

staload "./overture_symbol.sats"
staload "./overture_location.sats"

(* ****** ****** *)

datatype s2rt =
| S2RTtype | S2RTclock | S2RTint | S2RTrat | S2RTbool
| S2RTkind (* classifier of rate types: strict | gated *)
| S2RTerr (* error indication; compares equal to nothing *)

fun fprint_s2rt (out: FILEref, srt: s2rt): void
fun eq_s2rt_s2rt (srt1: s2rt, srt2: s2rt): bool

(*
** subsort coercion: int is a subsort of rat
*)
fun s2rt_leq (srt1: s2rt, srt2: s2rt): bool

(* ****** ****** *)

fun stamp_new (): int (* global stamp counter *)

(* ****** ****** *)

abstype s2var = ptr

fun s2var_make (sym: symbol, srt: s2rt): s2var
fun s2var_get_sym (s2v: s2var): symbol
fun s2var_get_srt (s2v: s2var): s2rt
fun s2var_get_stamp (s2v: s2var): int
fun eq_s2var_s2var (s2v1: s2var, s2v2: s2var): bool

fun fprint_s2var (out: FILEref, s2v: s2var): void

(* ****** ****** *)

abstype s2cst = ptr

fun s2cst_make
  (name: symbol, prms: list0(s2rt), res: s2rt): s2cst
fun s2cst_get_name (s2c: s2cst): symbol
fun s2cst_get_prms (s2c: s2cst): list0(s2rt)
fun s2cst_get_res (s2c: s2cst): s2rt
fun s2cst_get_stamp (s2c: s2cst): int
fun eq_s2cst_s2cst (s2c1: s2cst, s2c2: s2cst): bool

(* ****** ****** *)

datatype
s2exp_node =
| S2Eint of int
| S2Erat of (int, int)      (* normalized a/b with b > 0 *)
| S2Evar of s2var
| S2Ecst of s2cst           (* nullary: true, false *)
| S2Eapp of (s2cst, s2explst)
// end of [s2exp_node]

where
s2exp = '{
  s2e_loc= location, s2e_srt= s2rt, s2e_node= s2exp_node
} (* end of [s2exp] *)

and s2explst = list0 (s2exp)

fun s2exp_make
  (loc: location, srt: s2rt, node: s2exp_node): s2exp

fun s2exp_int (loc: location, i: int): s2exp
fun s2exp_rat (loc: location, a: int, b: int): s2exp (* normalizes *)
fun s2exp_var (loc: location, s2v: s2var): s2exp
fun s2exp_bool (loc: location, b: bool): s2exp

fun fprint_s2exp (out: FILEref, s2e: s2exp): void

(*
** the free static variables of an expression (deduplicated)
*)
fun s2exp_collect_svars
  (s2e: s2exp, acc: list0(s2var)): list0 (s2var)

(*
** local disambiguation for error messages: given every variable
** appearing in one message, names shared by distinct variables are
** numbered with Unicode subscripts (q -> q1, q2 in subscript form);
** unambiguous names print bare. [fprint_s2var] consults the table.
*)
fun the_dsptbl_set (s2vs: list0(s2var)): void
fun the_dsptbl_clear (): void

(* ****** ****** *)
//
// substitution of static expressions for static variables
//
typedef s2sub = list0 (@(s2var, s2exp))

fun s2exp_subst (s2e: s2exp, sub: s2sub): s2exp
fun s2explst_subst (s2es: s2explst, sub: s2sub): s2explst

(* ****** ****** *)

datatype
t2ype =
| T2YPEbase of symbol             (* int, bool *)
| T2YPEvar of s2var               (* payload type variable *)
| T2YPEsta of s2exp               (* singleton int(k) / rat(k) *)
| T2YPErate of (s2exp(*kind*), t2ype, s2exp(*clock*))
    (* rate(k, a, c): k classifies the flow (strict | gated);
       rate(a, c) is surface sugar for rate(strict, a, c) *)
| T2YPEexi of (list0(s2var), s2exp(*guard*), t2ype)
| T2YPEtup of t2ypelst            (* multi-return *)
| T2YPEerr of ()
// end of [t2ype]

where t2ypelst = list0 (t2ype)

fun t2ype_subst (t2p: t2ype, sub: s2sub): t2ype
fun fprint_t2ype (out: FILEref, t2p: t2ype): void

(* ****** ****** *)
//
// node signatures (user nodes, extern nodes, and builtin operators)
//
typedef
n2odesig = '{
  n2sig_name= symbol
, n2sig_tvars= list0 (s2var) (* payload type variables *)
, n2sig_uvars= list0 (s2var) (* universal static variables *)
, n2sig_guard= s2exp         (* bool; trivially true if none *)
, n2sig_params= t2ypelst
, n2sig_res= t2ype
, n2sig_wcet= option0 (s2exp) (* static int expr; extern nodes only *)
} (* end of [n2odesig] *)

fun fprint_n2odesig (out: FILEref, nsig: n2odesig): void

(* ****** ****** *)
//
// the built-in static constants
//
fun staexp_initialize (): void

(*
** overload resolution: find a constant [name] whose parameter sorts
** accept [argsrts] (up to int <= rat coercion)
*)
fun the_s2csttbl_search
  (name: symbol, argsrts: list0(s2rt)): option0 (s2cst)
// end of [the_s2csttbl_search]

fun s2cst_true (): s2cst
fun s2cst_false (): s2cst
fun s2cst_strict (): s2cst   (* strict : kind *)
fun s2cst_gated (): s2cst    (* gated : kind *)
fun s2cst_clk (): s2cst      (* clk : (int, rat) -> clock *)
fun s2cst_over (): s2cst     (* / : (clock, int) -> clock, period/k *)
fun s2cst_under (): s2cst    (* * : (clock, int) -> clock, period*k *)
fun s2cst_shift (): s2cst    (* shift : (clock, rat) -> clock *)
fun s2cst_divides (): s2cst  (* | : (int, int) -> bool *)

(*
** identity tests: overload resolution happens once, in
** elaboration; every later phase dispatches on the resolved
** constant, never on the surface name
*)
fun s2cst_is_clk (s2c: s2cst): bool
fun s2cst_is_over (s2c: s2cst): bool
fun s2cst_is_under (s2c: s2cst): bool
fun s2cst_is_shift (s2c: s2cst): bool

(*
** the built-in term-level operators, as node signatures:
** */  ^/  shift  fby  cons  tail  merge
*)
fun the_builtintbl_search (name: symbol): option0 (n2odesig)

(* ****** ****** *)

(* end of [overture_staexp.sats] *)
