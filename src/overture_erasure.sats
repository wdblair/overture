(*
** Overture: erasure (stub)
**
** After typechecking, clocks are erased and the program is dumped as
** a JSON dataflow graph: sensors, actuators, per-node flow tables
** and equation trees, plus a topological order of each node's
** defined flows in which the stream inputs of fby/cons are treated
** as non-strict edges. A cycle through strict edges only is an
** instantaneous loop: it produces a causality warning (full
** causality checking is future work). This graph is the attachment
** point for C code generation.
*)

staload "libats/ML/SATS/basis.sats"
staload "libats/ML/SATS/list0.sats"

staload "./overture_trans12.sats"

(* ****** ****** *)

fun erasure_emit (out: FILEref, prog: p2rog): void

(*
** causal order of a node's defined flows: fby/cons stream inputs
** are non-strict edges; a strict cycle warns and falls back to an
** unordered remainder (also reused by code generation)
*)
fun erasure_topo_order
(
  nodename: string
, defined: list0(d2var), eqns: list0(e2qn)
) : list0 (d2var)

(* ****** ****** *)

(* end of [overture_erasure.sats] *)
