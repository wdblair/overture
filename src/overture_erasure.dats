(*
** Overture: erasure
*)

#include "share/atspre_staload.hats"

staload "libats/ML/SATS/basis.sats"
staload "libats/ML/SATS/list0.sats"
staload _ = "libats/ML/DATS/list0.dats"

staload "./overture_symbol.sats"
staload "./overture_location.sats"
staload "./overture_staexp.sats"
staload "./overture_trans12.sats"
staload "./overture_erasure.sats"

(* ****** ****** *)
//
// ground clocks print as [period, date]; symbolic ones as null
//
fn jclock
  (out: FILEref, t2p: t2ype): void = let
//
val clkopt = (
  case+ t2p of
  | T2YPErate (_, _, clk) => Some0 (clk)
  | _ (*rest*) => None0 ()
) : option0 (s2exp)
//
in
//
case+ clkopt of
| Some0 (clk) => (
    case+ clk.s2e_node of
    | S2Eapp (s2c, args)
        when s2cst_is_clk (s2c) => (
        case+ args of
        | list0_cons (n, list0_cons (p, list0_nil ())) => (
            case+ (n.s2e_node, p.s2e_node) of
            | (S2Eint (n), S2Eint (a)) =>
                fprint! (out, "[", n, ", ", n * a, "]")
            | (S2Eint (n), S2Erat (a, b)) =>
                fprint! (out, "[", n, ", ", (n * a) / b, "]")
            | (_, _) => fprint! (out, "null")
          )
        | _ (*rest*) => fprint! (out, "null")
      )
    | _ (*rest*) => fprint! (out, "null")
  )
| _ (*rest*) => fprint! (out, "null")
//
end (* end of [jclock] *)

fn jname (out: FILEref, d2v: d2var): void =
  fprint!
    (out, "\"", symbol_get_name (d2var_get_sym (d2v)),
     "$", d2var_get_stamp (d2v), "\"")
(* end of [jname] *)

(* ****** ****** *)

fun
jexp (out: FILEref, d2e: d2exp): void =
(
case+ d2e.d2e_node of
| D2Eint (i) =>
    fprint! (out, "{\"int\": ", i, "}")
| D2Erat (a, b) =>
    fprint! (out, "{\"rat\": [", a, ", ", b, "]}")
| D2Evar (d2v) => let
    val () = fprint! (out, "{\"var\": ")
    val () = jname (out, d2v)
  in
    fprint! (out, "}")
  end
| D2Eapp (nsig, args) => let
    val () = fprint!
      (out, "{\"apply\": \"", symbol_get_name (nsig.n2sig_name), "\", \"args\": [")
    fun loop
      (out: FILEref, args: d2explst, first: bool): void =
      case+ args of
      | list0_nil () => ()
      | list0_cons (arg, args) => let
          val () = if not (first) then fprint! (out, ", ")
          val () = jexp (out, arg)
        in
          loop (out, args, false)
        end
    val () = loop (out, args, true)
  in
    fprint! (out, "]}")
  end
| D2Eerr () => fprint! (out, "{\"err\": true}")
) (* end of [jexp] *)

(* ****** ****** *)
//
// dependencies: flows read by an expression, tagged strict or not;
// the stream inputs of fby and cons are non-strict
//
typedef dep = @(d2var, bool(*strict*))

fun
deps_of
  (d2e: d2exp, strict: bool, acc: list0(dep)): list0 (dep) =
(
case+ d2e.d2e_node of
| D2Evar (d2v) => list0_cons (@(d2v, strict), acc)
| D2Eapp (nsig, args) => let
    val name = symbol_get_name (nsig.n2sig_name)
    val delayed = (name = "fby") orelse (name = "cons")
    fun loop
      (args: d2explst, i: int, acc: list0(dep)): list0(dep) =
      case+ args of
      | list0_nil () => acc
      | list0_cons (arg, args) => let
          val s =
            if delayed andalso (i = 1) then false else strict
          val acc = deps_of (arg, s, acc)
        in
          loop (args, i + 1, acc)
        end
  in
    loop (args, 0, acc)
  end
| _ (*rest*) => acc
) (* end of [deps_of] *)

(* ****** ****** *)
//
// topological order of a node's defined flows over strict edges;
// a strict cycle yields a causality warning
//
implement
erasure_topo_order
(
  nodename, defined, eqns
) = let
//
fn is_defined
  (d2v: d2var): bool = let
  fun loop (d2vs: list0(d2var)): bool =
    case+ d2vs of
    | list0_nil () => false
    | list0_cons (d2w, d2vs) =>
        if eq_d2var_d2var (d2v, d2w) then true else loop (d2vs)
in
  loop (defined)
end // end of [is_defined]
//
// strict dependencies of each defined flow (via its equation)
//
fn strict_deps
  (d2v: d2var): list0 (d2var) = let
  fun find
    (eqns: list0(e2qn)): option0 (e2qn) =
    case+ eqns of
    | list0_nil () => None0 ()
    | list0_cons (eqn, eqns) => let
        fun inlhs (lhs: list0(d2var)): bool =
          case+ lhs of
          | list0_nil () => false
          | list0_cons (d2w, lhs) =>
              if eq_d2var_d2var (d2v, d2w) then true else inlhs (lhs)
      in
        if inlhs (eqn.e2qn_lhs) then Some0 (eqn) else find (eqns)
      end
in
  case+ find (eqns) of
  | None0 () => list0_nil ()
  | Some0 (eqn) => let
      val deps = deps_of (eqn.e2qn_rhs, true, list0_nil ())
      fun keep (deps: list0(dep)): list0(d2var) =
        case+ deps of
        | list0_nil () => list0_nil ()
        | list0_cons (d, deps) =>
            if d.1 andalso is_defined (d.0)
              then list0_cons (d.0, keep (deps)) else keep (deps)
            // end of [if]
    in
      keep (deps)
    end
end // end of [strict_deps]
//
// Kahn-style: repeatedly take a flow whose strict deps are all
// already ordered
//
fun mem
  (d2vs: list0(d2var), d2v: d2var): bool =
  case+ d2vs of
  | list0_nil () => false
  | list0_cons (d2w, d2vs) =>
      if eq_d2var_d2var (d2v, d2w) then true else mem (d2vs, d2v)
//
fun ready
  (d2v: d2var, done: list0(d2var)): bool = let
  fun loop (deps: list0(d2var)): bool =
    case+ deps of
    | list0_nil () => true
    | list0_cons (d, deps) =>
        if eq_d2var_d2var (d, d2v) then false (* strict self-loop *)
        else if mem (done, d) then loop (deps)
        else false
in
  loop (strict_deps (d2v))
end // end of [ready]
//
fun pick
  (pending: list0(d2var), done: list0(d2var))
  : option0 (d2var) =
  case+ pending of
  | list0_nil () => None0 ()
  | list0_cons (d2v, pending) =>
      if ready (d2v, done)
        then Some0 (d2v) else pick (pending, done)
      // end of [if]
//
fun remove
  (pending: list0(d2var), d2v: d2var): list0(d2var) =
  case+ pending of
  | list0_nil () => list0_nil ()
  | list0_cons (d2w, pending) =>
      if eq_d2var_d2var (d2v, d2w)
        then pending else list0_cons (d2w, remove (pending, d2v))
      // end of [if]
//
fun loop
  (pending: list0(d2var), done: list0(d2var)): list0(d2var) =
(
  case+ pending of
  | list0_nil () => list0_reverse (done)
  | list0_cons _ => (
      case+ pick (pending, done) of
      | Some0 (d2v) =>
          loop (remove (pending, d2v), list0_cons (d2v, done))
      | None0 () => let
          (* strict cycle: instantaneous loop not broken by fby *)
          val () = fprint!
            (stderr_ref, "warning: possible instantaneous cycle in node [",
             nodename, "] among:")
          fun warn (d2vs: list0(d2var)): void =
            case+ d2vs of
            | list0_nil () => ()
            | list0_cons (d2v, d2vs) => let
                val () = fprint! (stderr_ref, " ",
                  symbol_get_name (d2var_get_sym (d2v)))
              in
                warn (d2vs)
              end
          val () = warn (pending)
          val () = fprint! (stderr_ref, "\n")
        in
          (* fall back: append the remainder unordered *)
          list0_append (list0_reverse (done), pending)
        end
    )
) (* end of [loop] *)
//
in
  loop (defined, list0_nil ())
end // end of [erasure_topo_order]

(* ****** ****** *)

fn jflowlst
(
  out: FILEref
, kind: string, d2vs: list0(d2var), first: bool
) : bool = let
  fun loop
    (d2vs: list0(d2var), first: bool): bool =
    case+ d2vs of
    | list0_nil () => first
    | list0_cons (d2v, d2vs) => let
        val () = if not (first) then fprint! (out, ", ")
        val () = fprint! (out, "{\"name\": ")
        val () = jname (out, d2v)
        val () = fprint! (out, ", \"kind\": \"", kind, "\", \"clock\": ")
        val () = jclock (out, d2var_get_type (d2v))
        val () = fprint! (out, "}")
      in
        loop (d2vs, false)
      end
in
  loop (d2vs, first)
end // end of [jflowlst]

fn jeqns
  (out: FILEref, eqns: list0(e2qn)): void = let
  fun loop
    (eqns: list0(e2qn), first: bool): void =
    case+ eqns of
    | list0_nil () => ()
    | list0_cons (eqn, eqns) => let
        val () = if not (first) then fprint! (out, ", ")
        val () = fprint! (out, "{\"defines\": [")
        fun lhs (d2vs: list0(d2var), first: bool): void =
          case+ d2vs of
          | list0_nil () => ()
          | list0_cons (d2v, d2vs) => let
              val () = if not (first) then fprint! (out, ", ")
              val () = jname (out, d2v)
            in
              lhs (d2vs, false)
            end
        val () = lhs (eqn.e2qn_lhs, true)
        val () = fprint! (out, "], \"expr\": ")
        val () = jexp (out, eqn.e2qn_rhs)
        val () = fprint! (out, "}")
      in
        loop (eqns, false)
      end
in
  loop (eqns, true)
end // end of [jeqns]

(* ****** ****** *)

implement
erasure_emit
  (out, prog) = let
//
val () = fprint! (out, "{\n  \"sensors\": [")
val _ = jflowlst (out, "sensor", prog.p2rog_sensors, true)
val () = fprint! (out, "],\n  \"actuators\": [")
val _ = jflowlst (out, "actuator", prog.p2rog_actuators, true)
//
val () = fprint! (out, "],\n  \"nodes\": [")
fun nodes
  (out: FILEref, n2s: list0(n2ode), first: bool): void =
  case+ n2s of
  | list0_nil () => ()
  | list0_cons (n2, n2s) => let
      val name = symbol_get_name (n2.n2ode_sig.n2sig_name)
      val () = if not (first) then fprint! (out, ", ")
      val () = fprint! (out, "\n    {\"name\": \"", name, "\"")
      val () = fprint! (out, ", \"extern\": ",
        (if n2.n2ode_extern then "true" else "false"): string)
      val () = fprint! (out, ", \"wcet\": ")
      val () = (
        case+ n2.n2ode_sig.n2sig_wcet of
        | Some0 (w) => (
            case+ w.s2e_node of
            | S2Eint (i) => fprint! (out, i)
            | _ (*symbolic: ground only after instantiation*) =>
                fprint! (out, "null")
          )
        | None0 () => fprint! (out, "null")
      ) : void
      val () = fprint! (out, ",\n     \"flows\": [")
      val f = jflowlst (out, "param", n2.n2ode_params, true)
      val f = jflowlst (out, "return", n2.n2ode_returns, f)
      val _ = jflowlst (out, "var", n2.n2ode_vars, f)
      val () = fprint! (out, "],\n     \"equations\": [")
      val () = jeqns (out, n2.n2ode_eqns)
      val () = fprint! (out, "],\n     \"order\": [")
      val defined =
        list0_append (n2.n2ode_returns, n2.n2ode_vars)
      val order = (
        if n2.n2ode_extern
          then list0_nil ()
          else erasure_topo_order (name, defined, n2.n2ode_eqns)
      ) : list0 (d2var)
      fun ord (d2vs: list0(d2var), first: bool): void =
        case+ d2vs of
        | list0_nil () => ()
        | list0_cons (d2v, d2vs) => let
            val () = if not (first) then fprint! (out, ", ")
            val () = jname (out, d2v)
          in
            ord (d2vs, false)
          end
      val () = ord (order, true)
      val () = fprint! (out, "]}")
    in
      nodes (out, n2s, false)
    end
val () = nodes (out, prog.p2rog_nodes, true)
//
val () = fprint! (out, "],\n  \"toplevel\": [")
val () = jeqns (out, prog.p2rog_eqns)
val () = fprint! (out, "]\n}\n")
//
in
  // nothing
end // end of [erasure_emit]

(* ****** ****** *)

(* end of [overture_erasure.dats] *)
