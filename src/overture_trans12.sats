(*
** Overture: elaboration and binding (trans1 + trans2 collapsed)
**
** One pass over the parse-level AST that resolves scopes, checks
** sorts, and translates types. Overture has no macros, overloading,
** or user-defined statics, so ATS's trans1 and trans2 collapse into
** a single phase; the names are kept for the mental mapping.
**
** Node bodies are systems of equations: every parameter, return,
** and var flow is bound up front, then equations are checked as an
** unordered set (use-before-define is legal); each return/var must
** be defined by exactly one equation.
*)

staload "libats/ML/SATS/basis.sats"
staload "libats/ML/SATS/list0.sats"

staload "./overture_symbol.sats"
staload "./overture_location.sats"
staload "./overture_syntax.sats"
staload "./overture_staexp.sats"

(* ****** ****** *)

abstype d2var = ptr

fun d2var_make (sym: symbol, t2p: t2ype): d2var
fun d2var_get_sym (d2v: d2var): symbol
fun d2var_get_type (d2v: d2var): t2ype
fun d2var_get_stamp (d2v: d2var): int
fun eq_d2var_d2var (d2v1: d2var, d2v2: d2var): bool

fun fprint_d2var (out: FILEref, d2v: d2var): void

(* ****** ****** *)

datatype
d2exp_node =
| D2Eint of int
| D2Erat of (int, int)             (* constant rational, e.g. shift amount *)
| D2Evar of d2var                  (* flow reference *)
| D2Eapp of (n2odesig, d2explst)   (* node or builtin application *)
| D2Eerr of ()
// end of [d2exp_node]

where
d2exp = '{
  d2e_loc= location, d2e_node= d2exp_node
} (* end of [d2exp] *)

and d2explst = list0 (d2exp)

fun d2exp_make (loc: location, node: d2exp_node): d2exp
fun fprint_d2exp (out: FILEref, d2e: d2exp): void

(* ****** ****** *)

typedef
e2qn = '{
  e2qn_loc= location
, e2qn_lhs= list0 (d2var)
, e2qn_rhs= d2exp
} (* end of [e2qn] *)

typedef
n2ode = '{
  n2ode_loc= location
, n2ode_extern= bool
, n2ode_sig= n2odesig
, n2ode_params= list0 (d2var)
, n2ode_returns= list0 (d2var)
, n2ode_vars= list0 (d2var)
, n2ode_eqns= list0 (e2qn)
} (* end of [n2ode] *)

typedef
p2rog = '{
  p2rog_sensors= list0 (d2var)
, p2rog_actuators= list0 (d2var)
, p2rog_nodes= list0 (n2ode)
, p2rog_eqns= list0 (e2qn) (* top-level actuator equations *)
} (* end of [p2rog] *)

(* ****** ****** *)

fun trans12_program (decs: d0eclst): p2rog

fun fprint_p2rog (out: FILEref, prog: p2rog): void

(* ****** ****** *)

(* end of [overture_trans12.sats] *)
