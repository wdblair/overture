(*
** Overture: parse-level abstract syntax
**
** Modeled on pats_syntax.sats. At the parse level, types and static
** expressions share one syntactic class [s0exp]: a type like
** rate(int, c ^/ 10) is an ident-headed application whose arguments
** are static expressions; sorting them out is trans12's job.
*)

staload "libats/ML/SATS/basis.sats"
staload "libats/ML/SATS/list0.sats"

staload "./overture_symbol.sats"
staload "./overture_location.sats"

(* ****** ****** *)

typedef i0de = '{ i0de_loc= location, i0de_sym= symbol }

fun i0de_make (loc: location, sym: symbol): i0de

(* ****** ****** *)

datatype s0rt = S0RTide of (location, symbol)

fun fprint_s0rt (out: FILEref, srt: s0rt): void

(* ****** ****** *)

datatype
s0exp_node =
| S0Eint of int
| S0Eide of symbol
| S0Eapp of (i0de, s0explst)   (* ident-headed application: rate(...), clk lit *)
| S0Eopapp of (i0de, s0explst) (* operator application after fixity *)
| S0Etup of s0explst           (* tuple: the clock literal (n, p) *)
| S0Eexi of (q0ua, s0exp)      (* [qargs | guard] t / exists-form *)
// end of [s0exp_node]

where
s0exp = '{
  s0e_loc= location, s0e_node= s0exp_node
} (* end of [s0exp] *)

and s0explst = list0 (s0exp)
and s0expopt = option0 (s0exp)

and q0arg = '{
  q0arg_loc= location
, q0arg_sym= symbol
, q0arg_srt= option0 (s0rt)
} (* end of [q0arg] *)

and q0ua = '{
  q0ua_loc= location
, q0ua_args= list0 (q0arg)
, q0ua_guard= option0 (s0exp)
} (* end of [q0ua] *)

fun s0exp_make (loc: location, node: s0exp_node): s0exp
fun q0arg_make (loc: location, sym: symbol, srt: option0(s0rt)): q0arg
fun q0ua_make (loc: location, args: list0(q0arg), gua: option0(s0exp)): q0ua

fun fprint_s0exp (out: FILEref, s0e: s0exp): void
fun fprint_q0ua (out: FILEref, qua: q0ua): void

(* ****** ****** *)

datatype
d0exp_node =
| D0Eint of int
| D0Eide of symbol
| D0Eapp of (i0de, d0explst)   (* node application: f(e1, ..., en) *)
| D0Eopapp of (i0de, d0explst) (* operator application after fixity *)
| D0Etup of d0explst
// end of [d0exp_node]

where
d0exp = '{
  d0e_loc= location, d0e_node= d0exp_node
} (* end of [d0exp] *)

and d0explst = list0 (d0exp)

fun d0exp_make (loc: location, node: d0exp_node): d0exp

fun fprint_d0exp (out: FILEref, d0e: d0exp): void

(* ****** ****** *)

typedef
p0aram = '{
  p0aram_loc= location, p0aram_sym= symbol, p0aram_typ= s0exp
} (* end of [p0aram] *)

typedef
e0qn = '{
  e0qn_loc= location, e0qn_lhs= list0(i0de), e0qn_rhs= d0exp
} (* end of [e0qn] *)

(* ****** ****** *)

datatype fixkind = FIXleft | FIXright | FIXpre

datatype
d0ec_node =
| D0Csensor of (i0de, s0exp)
| D0Cactuator of (i0de, s0exp)
| D0Cfixity of (fixkind, int, list0(i0de))
| D0Cabstype of i0de
| D0Ctypedef of (i0de, list0(@(i0de, s0exp)))
| D0Cnode of n0de
| D0Ceqn of e0qn (* top-level equation driving an actuator *)
// end of [d0ec_node]

where
n0de = '{
  n0de_loc= location
, n0de_extern= bool
, n0de_name= i0de
, n0de_prequa= list0 (q0ua)  (* quantifier groups before the name *)
, n0de_postqua= list0 (q0ua) (* quantifier groups after the name *)
, n0de_params= list0 (p0aram)
, n0de_returns= list0 (p0aram)
, n0de_wcet= option0 (s0exp) (* static int expr; extern nodes only *)
, n0de_vars= list0 (p0aram)
, n0de_eqns= option0 (list0(e0qn)) (* None iff extern *)
} (* end of [n0de] *)

and d0ec = '{
  d0ec_loc= location, d0ec_node= d0ec_node
} (* end of [d0ec] *)

and d0eclst = list0 (d0ec)

fun d0ec_make (loc: location, node: d0ec_node): d0ec

fun fprint_d0ec (out: FILEref, dec: d0ec): void
fun fprint_d0eclst (out: FILEref, decs: d0eclst): void

(* ****** ****** *)

(* end of [overture_syntax.sats] *)
