(*
** Overture: code generation to an ATS harness
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
staload "./overture_erasure.sats"
staload "./overture_ccomp.sats"

(* ****** ****** *)

fun gcd (a: int, b: int): int = let
  val a = (if a < 0 then ~a else a): int
in
  if b = 0 then a else gcd (b, a mod b)
end // end of [gcd]

fn lcm (a: int, b: int): int =
  if (a = 0) orelse (b = 0) then 0 else (a / gcd (a, b)) * b

fn str3
  (s1: string, s2: string, s3: string): string =
  string_append (string_append (s1, s2), s3)

(* ****** ****** *)
//
// ground evaluators: the program is typechecked and the statics are
// substituted, so failures here are internal errors (except where
// noted)
//
fn cgerr (msg: string): void = errmsg_noloc (msg)

extern fun eval_rat (s2e: s2exp): @(int, int) (* (a, b), b >= 1 *)
extern fun eval_clk (s2e: s2exp): @(int, int) (* (period, date) *)

implement
eval_rat (s2e) = let
  val loc = s2e.s2e_loc
in
//
case+ s2e.s2e_node of
| S2Eint (i) => @(i, 1)
| S2Erat (a, b) => @(a, b)
| S2Eapp (s2c, args) => let
    val name = symbol_get_name (s2cst_get_name (s2c))
  in
    case+ args of
    | list0_cons (l, list0_cons (r, list0_nil ())) => let
        val x = eval_rat (l) and y = eval_rat (r)
      in
        case+ name of
        | "+" => @(x.0 * y.1 + y.0 * x.1, x.1 * y.1)
        | "-" => @(x.0 * y.1 - y.0 * x.1, x.1 * y.1)
        | "*" => @(x.0 * y.0, x.1 * y.1)
        | "/" =>
            if y.0 = 0 then (cgerr ("codegen: division by zero"); @(0, 1))
            else if y.0 < 0 then @(~(x.0 * y.1), x.1 * (~(y.0)))
            else @(x.0 * y.1, x.1 * y.0)
        | _ (*rest*) => let
            val () = cgerr (str3
              ("codegen: cannot evaluate static operation [", name, "]"))
          in
            @(0, 1)
          end
      end
    | list0_cons (c, list0_nil ()) => (
        case+ name of
        | "period" => let
            val nd = eval_clk (c) in @(nd.0, 1)
          end
        | "date" => let
            val nd = eval_clk (c) in @(nd.1, 1)
          end
        | _ (*rest*) => let
            val () = cgerr (str3
              ("codegen: cannot evaluate static operation [", name, "]"))
          in
            @(0, 1)
          end
      )
    | _ (*rest*) => let
        val () = cgerr ("codegen: cannot evaluate a static expression")
      in
        @(0, 1)
      end
  end
| _ (*S2Evar/S2Ecst*) => let
    val () = cgerr ("codegen: unbound static variable (internal)")
  in
    @(0, 1)
  end
//
end // end of [eval_rat]

fn eval_int (s2e: s2exp): int = let
  val r = eval_rat (s2e)
in
  if r.1 = 0 then 0
  else if r.0 mod r.1 != 0 then let
    val () = cgerr ("codegen: expected an integer static value")
  in
    0
  end else r.0 / r.1
end // end of [eval_int]

implement
eval_clk (s2e) =
(
case+ s2e.s2e_node of
| S2Eapp (s2c, args) => let
    val name = symbol_get_name (s2cst_get_name (s2c))
  in
    case+ args of
    | list0_cons (a1, list0_cons (a2, list0_nil ())) => (
        case+ name of
        | _ when s2cst_is_clk (s2c) => let
            val n = eval_int (a1)
            val p = eval_rat (a2)
            val d = (
              if p.1 = 0 then 0
              else if (n * p.0) mod p.1 != 0 then let
                val () = cgerr ("codegen: non-integer activation date")
              in
                0
              end else (n * p.0) / p.1
            ) : int
          in
            @(n, d)
          end
        | _ when s2cst_is_over (s2c) => let
            val nd = eval_clk (a1)
            val k = eval_int (a2)
          in
            if k = 0 then (cgerr ("codegen: clock division by zero"); @(1, 0))
            else @(nd.0 / k, nd.1)
          end
        | _ when s2cst_is_under (s2c) => let
            val nd = eval_clk (a1)
            val k = eval_int (a2)
          in
            @(nd.0 * k, nd.1)
          end
        | _ when s2cst_is_shift (s2c) => let
            val nd = eval_clk (a1)
            val k = eval_rat (a2)
            val s = (
              if k.1 = 0 then 0
              else if (nd.0 * k.0) mod k.1 != 0 then let
                val () = cgerr ("codegen: non-integer shift amount")
              in
                0
              end else (nd.0 * k.0) / k.1
            ) : int
          in
            @(nd.0, nd.1 + s)
          end
        | _ (*rest*) => let
            val () = cgerr ("codegen: cannot evaluate a clock expression")
          in
            @(1, 0)
          end
      )
    | _ (*rest*) => let
        val () = cgerr ("codegen: cannot evaluate a clock expression")
      in
        @(1, 0)
      end
  end
| _ (*rest*) => let
    val () = cgerr ("codegen: unbound clock variable (internal)")
  in
    @(1, 0)
  end
) (* end of [eval_clk] *)

(* ****** ****** *)
//
// cells, buffers, actions
//
typedef cell = '{
  c_id= int, c_bool= bool, c_n= int, c_d= int
} (* end of [cell] *)

datatype fval =
| FVcell of cell
| FVgated of (cell(*value*), cell(*presence*))
| FVint of int
| FVrat of (int, int)

typedef buf = '{
  b_id= int, b_bool= bool, b_depth= int
} (* end of [buf] *)

datatype action =
| ACTsense of (string, cell)
| ACTcall of (string, list0(fval), list0(cell))
| ACTcopy of (fval, cell)
| ACTfbyemit of (int(*v0*), cell(*state*), cell(*out*))
| ACTfbystore of (cell(*in*), cell(*state*))
| ACTconsemit of (int(*v0*), cell(*in*), cell(*out*))
| ACTbufwrite of (cell(*in*), buf)
| ACTbufread of (buf, cell(*out*))
| ACTcurrent of (int(*v0*), cell(*value*), cell(*presence*), cell(*out*))
| ACTactuate of (string, cell)
| ACTactuate_gated of (string, cell(*value*), cell(*presence*))
// end of [action]

typedef payenv = list0 (@(int(*tyvar stamp*), bool))

typedef
ccctx = '{
  x_nextid= ref (int)
, x_cells= ref (list0(cell))   (* clocked value cells *)
, x_states= ref (list0(cell))  (* unclocked state cells (fby) *)
, x_bufs= ref (list0(buf))
, x_pre= ref (list0(action))   (* reversed; senses + fby emits *)
, x_strict= ref (list0(action))
, x_post= ref (list0(action))  (* fby stores *)
, x_senses= ref (list0(@(string, bool)))
, x_acts= ref (list0(@(string, bool)))
, x_calls= ref (list0(@(string, list0(bool)(*args*), list0(bool)(*outs*))))
, x_tasks= ref (list0(@(string, int(*n*), int(*d*), int(*wcet*))))
, x_nodes= list0 (n2ode)
} (* end of [ccctx] *)

fn ctx_new
  (nodes: list0(n2ode)): ccctx = '{
  x_nextid= ref<int> (0)
, x_cells= ref<list0(cell)> (list0_nil ())
, x_states= ref<list0(cell)> (list0_nil ())
, x_bufs= ref<list0(buf)> (list0_nil ())
, x_pre= ref<list0(action)> (list0_nil ())
, x_strict= ref<list0(action)> (list0_nil ())
, x_post= ref<list0(action)> (list0_nil ())
, x_senses= ref<list0(@(string, bool))> (list0_nil ())
, x_acts= ref<list0(@(string, bool))> (list0_nil ())
, x_calls= ref<list0(@(string, list0(bool), list0(bool)))> (list0_nil ())
, x_tasks= ref<list0(@(string, int, int, int))> (list0_nil ())
, x_nodes= nodes
} (* end of [ctx_new] *)

fn ctx_id (ctx: ccctx): int = let
  val r = ctx.x_nextid
  val n = !r
  val () = !r := n + 1
in
  n
end // end of [ctx_id]

fn cell_new
  (ctx: ccctx, isbool: bool, n: int, d: int): cell = let
  val c = '{ c_id= ctx_id (ctx), c_bool= isbool, c_n= n, c_d= d } : cell
  val r = ctx.x_cells
  val () = !r := list0_cons (c, !r)
in
  c
end // end of [cell_new]

fn state_new
  (ctx: ccctx, isbool: bool): cell = let
  val c = '{ c_id= ctx_id (ctx), c_bool= isbool, c_n= 1, c_d= 0 } : cell
  val r = ctx.x_states
  val () = !r := list0_cons (c, !r)
in
  c
end // end of [state_new]

fn buf_new
  (ctx: ccctx, isbool: bool, depth: int): buf = let
  val b = '{ b_id= ctx_id (ctx), b_bool= isbool, b_depth= depth } : buf
  val r = ctx.x_bufs
  val () = !r := list0_cons (b, !r)
in
  b
end // end of [buf_new]

fn emit_pre (ctx: ccctx, a: action): void =
  let val r = ctx.x_pre in !r := list0_cons (a, !r) end
fn emit_strict (ctx: ccctx, a: action): void =
  let val r = ctx.x_strict in !r := list0_cons (a, !r) end
fn emit_post (ctx: ccctx, a: action): void =
  let val r = ctx.x_post in !r := list0_cons (a, !r) end

(* ****** ****** *)
//
// payload resolution
//
fn pay_find
  (pe: payenv, stamp: int): bool = let
  fun loop (pe: payenv): bool =
    case+ pe of
    | list0_nil () => false (* default int *)
    | list0_cons (kx, pe) =>
        if kx.0 = stamp then kx.1 else loop (pe)
in
  loop (pe)
end // end of [pay_find]

fn t2ype_isbool
  (t2p: t2ype, pe: payenv): bool =
(
  case+ t2p of
  | T2YPEbase (sym) => symbol_get_name (sym) = "bool"
  | T2YPEvar (s2v) => pay_find (pe, s2var_get_stamp (s2v))
  | _ (*rest*) => false
) (* end of [t2ype_isbool] *)

(*
** evaluate a ground kind index: true iff gated
*)
fn eval_gated (knd: s2exp): bool =
(
  case+ knd.s2e_node of
  | S2Ecst (s2c) => eq_s2cst_s2cst (s2c, s2cst_gated ())
  | _ (*rest*) => let
      val () = cgerr ("codegen: unbound kind variable (internal)")
    in
      false
    end
) (* end of [eval_gated] *)

(* ****** ****** *)
//
// clock s2exp from a ground (n, d) pair: clk(n, d/n)
//
fn clk_s2exp
  (n: int, d: int): s2exp = let
  val loc = location_dummy ()
  val nn = s2exp_int (loc, n)
  val pp = s2exp_rat (loc, d, n)
in
  s2exp_make (loc, S2RTclock (),
    S2Eapp (s2cst_clk (), list0_cons (nn, list0_cons (pp, list0_nil ()))))
end // end of [clk_s2exp]

fn fval_presence
  (fv: fval): cell =
(
  case+ fv of
  | FVgated (_, p) => p
  | _ (*rest*) => '{ c_id= 0, c_bool= true, c_n= 1, c_d= 0 }
) (* end of [fval_presence] *)

fn fval_cell
  (fv: fval, what: string): cell =
(
  case+ fv of
  | FVcell (c) => c
  | _ (*rest*) => let
      val () = cgerr (str3 ("codegen: expected a flow value in ", what, ""))
    in
      '{ c_id= 0, c_bool= false, c_n= 1, c_d= 0 }
    end
) (* end of [fval_cell] *)

(* ****** ****** *)
//
// binding a signature's statics against ground argument values
// (mirrors trans3's matching, on ground data)
//
fn app_bind
(
  nsig: n2odesig, fvs: list0(fval)
) : @(s2sub, payenv) = let
//
val loc = location_dummy ()
//
fun bound
  (sub: s2sub, s2v: s2var): bool = let
  fun loop (sub: s2sub): bool =
    case+ sub of
    | list0_nil () => false
    | list0_cons (kx, sub) =>
        if eq_s2var_s2var (kx.0, s2v) then true else loop (sub)
in
  loop (sub)
end // end of [bound]
//
fun loop
(
  prms: t2ypelst, fvs: list0(fval)
, sub: s2sub, pe: payenv
) : @(s2sub, payenv) =
(
case+ (prms, fvs) of
| (list0_cons (p, prms), list0_cons (fv, fvs)) => (
    case+ p of
    | T2YPErate (pknd, pelt, pclk) => let
        (* the value cell carries the clock and payload *)
        val c = (
          case+ fv of
          | FVgated (v, _) => v
          | _ (*rest*) => fval_cell (fv, "an application")
        ) : cell
        val sub = (
          case+ pknd.s2e_node of
          | S2Evar (s2v) =>
              if bound (sub, s2v) then sub
              else let
                (* bind a kind variable from the value shape *)
                val kc = (
                  case+ fv of
                  | FVgated _ => s2cst_gated ()
                  | _ (*rest*) => s2cst_strict ()
                ) : s2cst
                val ke = s2exp_make
                  (location_dummy (), S2RTkind (), S2Ecst (kc))
              in
                list0_cons (@(s2v, ke), sub)
              end
          | _ (*rest*) => sub
        ) : s2sub
        val sub = (
          case+ pclk.s2e_node of
          | S2Evar (s2v) =>
              if bound (sub, s2v) then sub
              else list0_cons (@(s2v, clk_s2exp (c.c_n, c.c_d)), sub)
          | _ (*compound: already verified*) => sub
        ) : s2sub
        val pe = (
          case+ pelt of
          | T2YPEvar (tv) =>
              list0_cons (@(s2var_get_stamp (tv), c.c_bool), pe)
          | _ (*rest*) => pe
        ) : payenv
      in
        loop (prms, fvs, sub, pe)
      end
    | T2YPEsta (ps) => let
        val sub = (
          case+ ps.s2e_node of
          | S2Evar (s2v) =>
              if bound (sub, s2v) then sub
              else let
                val v = (
                  case+ fv of
                  | FVint (i) => (
                      case+ s2var_get_srt (s2v) of
                      | S2RTrat () => s2exp_rat (loc, i, 1)
                      | _ (*int*) => s2exp_int (loc, i)
                    )
                  | FVrat (a, b) => s2exp_rat (loc, a, b)
                  | _ (*flow*) => let
                      val () = cgerr
                        ("codegen: expected a static argument (internal)")
                    in
                      s2exp_int (loc, 0)
                    end
                ) : s2exp
              in
                list0_cons (@(s2v, v), sub)
              end
          | _ (*rest*) => sub
        ) : s2sub
      in
        loop (prms, fvs, sub, pe)
      end
    | _ (*base/others*) => loop (prms, fvs, sub, pe)
  )
| (_, _) => @(sub, pe)
) (* end of [loop] *)
//
in
  loop (nsig.n2sig_params, fvs, list0_nil (), list0_nil ())
end // end of [app_bind]

(* ****** ****** *)
//
// existential witness extraction: the determinacy check guarantees
// the guard pins the witness; here we require the pinned form
// syntactically (conjunctions of period(q) == E and date(q) == E'),
// evaluating E, E' to ground values
//
fun
guard_find_eq
(
  gua: s2exp, q: s2var, proj: string
) : option0 (s2exp) = let
//
fn isproj
  (s2e: s2exp): bool =
  case+ s2e.s2e_node of
  | S2Eapp (s2c, args) => (
      case+ args of
      | list0_cons (a, list0_nil ()) => (
          case+ a.s2e_node of
          | S2Evar (s2v) =>
              (symbol_get_name (s2cst_get_name (s2c)) = proj)
                andalso eq_s2var_s2var (s2v, q)
          | _ (*rest*) => false
        )
      | _ (*rest*) => false
    )
  | _ (*rest*) => false
//
in
//
case+ gua.s2e_node of
| S2Eapp (s2c, args) => let
    val name = symbol_get_name (s2cst_get_name (s2c))
  in
    case+ args of
    | list0_cons (l, list0_cons (r, list0_nil ())) => (
        case+ name of
        | "&&" => (
            case+ guard_find_eq (l, q, proj) of
            | Some0 (e) => Some0 (e)
            | None0 () => guard_find_eq (r, q, proj)
          )
        | "==" =>
            if isproj (l) then Some0 (r)
            else if isproj (r) then Some0 (l)
            else None0 ()
        | _ (*rest*) => None0 ()
      )
    | _ (*rest*) => None0 ()
  end
| _ (*rest*) => None0 ()
//
end // end of [guard_find_eq]

(*
** open the existentials of a result type by computing witnesses
*)
fun
open_exi_ground
  (t2p: t2ype, name: string): t2ype =
(
case+ t2p of
| T2YPEexi (s2vs, gua, body) => let
    fun witness
      (s2vs: list0(s2var), sub: s2sub): s2sub =
      case+ s2vs of
      | list0_nil () => sub
      | list0_cons (q, s2vs) => let
          val gua1 = s2exp_subst (gua, sub)
          val w = (
            case+ s2var_get_srt (q) of
            | S2RTclock () => (
                case+ (guard_find_eq (gua1, q, "period"),
                       guard_find_eq (gua1, q, "date")) of
                | (Some0 (en), Some0 (ed)) =>
                    clk_s2exp (eval_int (en), eval_int (ed))
                | (_, _) => let
                    val () = cgerr (str3
                      ("codegen: cannot extract the existential witness of [",
                       name, "] (guard not in pinned form)"))
                  in
                    clk_s2exp (1, 0)
                  end
              )
            | _ (*int/rat*) => let
                val () = cgerr (str3
                  ("codegen: non-clock existential in [", name,
                   "] is not supported"))
              in
                s2exp_int (location_dummy (), 0)
              end
          ) : s2exp
        in
          witness (s2vs, list0_cons (@(q, w), sub))
        end
    val sub = witness (s2vs, list0_nil ())
  in
    open_exi_ground (t2ype_subst (body, sub), name)
  end
| _ (*rest*) => t2p
) (* end of [open_exi_ground] *)

(* ****** ****** *)
//
// flattening
//
typedef fenv = list0 (@(int(*d2var stamp*), fval))

fn fenv_find
  (env: fenv, d2v: d2var): fval = let
  val stamp = d2var_get_stamp (d2v)
  fun loop (env: fenv): fval =
    case+ env of
    | list0_nil () => let
        val () = cgerr ("codegen: unbound flow (internal)")
      in
        FVint (0)
      end
    | list0_cons (kx, env) =>
        if kx.0 = stamp then kx.1 else loop (env)
in
  loop (env)
end // end of [fenv_find]

extern fun flatten
  (ctx: ccctx, env: fenv, d2e: d2exp): list0 (fval)

fn flatten1
  (ctx: ccctx, env: fenv, d2e: d2exp): fval = let
  val fvs = flatten (ctx, env, d2e)
in
  case+ fvs of
  | list0_cons (fv, list0_nil ()) => fv
  | _ (*rest*) => let
      val () = cgerr ("codegen: expected a single flow value")
    in
      FVint (0)
    end
end // end of [flatten1]

(*
** ring-buffered delay by s ticks on period n (shift and tail):
** depth s/n + 1; the writer runs at the input's clock, the reader
** at the output's; slot arithmetic guarantees a value is never
** overwritten before its read
*)
fn delay_line
(
  ctx: ccctx, inc: cell, s: int
) : cell = let
  val depth = s / inc.c_n + 1
  val b = buf_new (ctx, inc.c_bool, depth)
  val out = cell_new (ctx, inc.c_bool, inc.c_n, inc.c_d + s)
  val () = emit_strict (ctx, ACTbufwrite (inc, b))
  val () = emit_strict (ctx, ACTbufread (b, out))
in
  out
end // end of [delay_line]

fn node_find2
  (ctx: ccctx, name: symbol): option0 (n2ode) = let
  fun loop (n2s: list0(n2ode)): option0 (n2ode) =
    case+ n2s of
    | list0_nil () => None0 ()
    | list0_cons (n2, n2s) =>
        if n2.n2ode_sig.n2sig_name = name
          then Some0 (n2) else loop (n2s)
        // end of [if]
in
  loop (ctx.x_nodes)
end // end of [node_find2]

extern fun inline_node
  (ctx: ccctx, n2: n2ode, fvs: list0(fval)): list0 (fval)

extern fun apply_extern
  (ctx: ccctx, nsig: n2odesig, fvs: list0(fval)): list0 (fval)

extern fun apply_builtin
  (ctx: ccctx, name: string, fvs: list0(fval)): list0 (fval)

implement
flatten (ctx, env, d2e) =
(
case+ d2e.d2e_node of
| D2Eint (i) => list0_cons (FVint (i), list0_nil ())
| D2Erat (a, b) => list0_cons (FVrat (a, b), list0_nil ())
| D2Evar (d2v) => list0_cons (fenv_find (env, d2v), list0_nil ())
| D2Eerr () => list0_cons (FVint (0), list0_nil ())
| D2Eapp (nsig, args) => let
    fun fargs
      (args: d2explst): list0 (fval) =
      case+ args of
      | list0_nil () => list0_nil ()
      | list0_cons (arg, args) =>
          list0_cons (flatten1 (ctx, env, arg), fargs (args))
    val fvs = fargs (args)
    val name = nsig.n2sig_name
  in
    case+ node_find2 (ctx, name) of
    | Some0 (n2) =>
        if n2.n2ode_extern
          then apply_extern (ctx, nsig, fvs)
          else inline_node (ctx, n2, fvs)
        // end of [if]
    | None0 () =>
        apply_builtin (ctx, symbol_get_name (name), fvs)
  end
) (* end of [flatten] *)

implement
apply_builtin
  (ctx, name, fvs) =
(
case+ name of
//
| _ when (name = "*^") orelse (name = "/^") => (
    case+ fvs of
    | list0_cons (f, list0_cons (FVint (k), list0_nil ())) => let
        val c = fval_cell (f, name)
        val n2 = (if name = "*^" then c.c_n / k else c.c_n * k): int
        val out = cell_new (ctx, c.c_bool, n2, c.c_d)
        val () = emit_strict (ctx, ACTcopy (FVcell (c), out))
      in
        list0_cons (FVcell (out), list0_nil ())
      end
    | _ (*rest*) => let
        val () = cgerr (str3 ("codegen: bad arguments to [", name, "]"))
      in
        list0_cons (FVint (0), list0_nil ())
      end
  )
//
| "shift" => (
    case+ fvs of
    | list0_cons (f, list0_cons (k, list0_nil ())) => let
        val c = fval_cell (f, "shift")
        val kr = (
          case+ k of
          | FVint (i) => @(i, 1)
          | FVrat (a, b) => @(a, b)
          | _ (*flow*) => (cgerr ("codegen: shift amount must be a constant"); @(0, 1))
        ) : @(int, int)
        val s = (c.c_n * kr.0) / kr.1
      in
        if s < 0 then let
          val () = cgerr ("codegen: non-causal shift (negative amount)")
        in
          list0_cons (FVcell (c), list0_nil ())
        end else
          list0_cons (FVcell (delay_line (ctx, c, s)), list0_nil ())
      end
    | _ (*rest*) => let
        val () = cgerr ("codegen: bad arguments to [shift]")
      in
        list0_cons (FVint (0), list0_nil ())
      end
  )
//
| "tail" => (
    case+ fvs of
    | list0_cons (f, list0_nil ()) => let
        val c = fval_cell (f, "tail")
      in
        list0_cons (FVcell (delay_line (ctx, c, c.c_n)), list0_nil ())
      end
    | _ (*rest*) => let
        val () = cgerr ("codegen: bad arguments to [tail]")
      in
        list0_cons (FVint (0), list0_nil ())
      end
  )
//
| "fby" => (
    case+ fvs of
    | list0_cons (FVint (v0), list0_cons (f, list0_nil ())) => let
        val c = fval_cell (f, "fby")
        val st = state_new (ctx, c.c_bool)
        val out = cell_new (ctx, c.c_bool, c.c_n, c.c_d)
        val () = emit_pre (ctx, ACTfbyemit (v0, st, out))
        val () = emit_post (ctx, ACTfbystore (c, st))
      in
        list0_cons (FVcell (out), list0_nil ())
      end
    | _ (*rest*) => let
        val () = cgerr ("codegen: the head of [fby] must be a literal")
      in
        list0_cons (FVint (0), list0_nil ())
      end
  )
//
| "cons" => (
    case+ fvs of
    | list0_cons (FVint (v0), list0_cons (f, list0_nil ())) => let
        val c = fval_cell (f, "cons")
        val out = cell_new (ctx, c.c_bool, c.c_n, c.c_d - c.c_n)
        val () = emit_strict (ctx, ACTconsemit (v0, c, out))
      in
        list0_cons (FVcell (out), list0_nil ())
      end
    | _ (*rest*) => let
        val () = cgerr ("codegen: the head of [cons] must be a literal")
      in
        list0_cons (FVint (0), list0_nil ())
      end
  )
//
| "when" => (
    (* a gated flow is a (value, presence) cell pair *)
    case+ fvs of
    | list0_cons (f, list0_cons (b, list0_nil ())) => let
        val cf = fval_cell (f, "when")
        val cb = fval_cell (b, "when")
        val vout = cell_new (ctx, cf.c_bool, cf.c_n, cf.c_d)
        val pout = cell_new (ctx, true(*bool*), cf.c_n, cf.c_d)
        val () = emit_strict (ctx, ACTcopy (FVcell (cf), vout))
        val () = emit_strict (ctx, ACTcopy (FVcell (cb), pout))
      in
        list0_cons (FVgated (vout, pout), list0_nil ())
      end
    | _ (*rest*) => let
        val () = cgerr ("codegen: bad arguments to [when]")
      in
        list0_cons (FVint (0), list0_nil ())
      end
  )
//
| "current" => (
    (* hold the last present value; the head initializes *)
    case+ fvs of
    | list0_cons (FVint (v0), list0_cons (g, list0_nil ())) => (
        case+ g of
        | FVgated (v, p) => let
            val out = cell_new (ctx, v.c_bool, v.c_n, v.c_d)
            val () = emit_strict (ctx, ACTcurrent (v0, v, p, out))
          in
            list0_cons (FVcell (out), list0_nil ())
          end
        | _ (*rest*) => let
            val () = cgerr ("codegen: [current] expects a gated flow")
          in
            list0_cons (FVint (0), list0_nil ())
          end
      )
    | _ (*rest*) => let
        val () = cgerr ("codegen: the head of [current] must be a literal")
      in
        list0_cons (FVint (0), list0_nil ())
      end
  )
//
| "merge" => (
    case+ fvs of
    | list0_cons (b, list0_cons (x, list0_cons (y, list0_nil ()))) => let
        val cb = fval_cell (b, "merge")
        val cx = fval_cell (x, "merge")
        val out = cell_new (ctx, cx.c_bool, cx.c_n, cx.c_d)
        (* out := if b then x else y, at the common clock *)
        val () = emit_strict (ctx, ACTcall ("%merge",
          list0_cons (FVcell (cb),
            list0_cons (x, list0_cons (y, list0_nil ()))),
          list0_cons (out, list0_nil ())))
      in
        list0_cons (FVcell (out), list0_nil ())
      end
    | _ (*rest*) => let
        val () = cgerr ("codegen: bad arguments to [merge]")
      in
        list0_cons (FVint (0), list0_nil ())
      end
  )
//
| _ (*rest*) => let
    val () = cgerr (str3 ("codegen: unknown builtin [", name, "]"))
  in
    list0_cons (FVint (0), list0_nil ())
  end
) (* end of [apply_builtin] *)

(* ****** ****** *)

fn restypes_of
  (t2p: t2ype): t2ypelst =
(
  case+ t2p of
  | T2YPEtup (t2ps) => t2ps
  | _ (*rest*) => list0_cons (t2p, list0_nil ())
) (* end of [restypes_of] *)

implement
apply_extern
  (ctx, nsig, fvs) = let
//
val name = symbol_get_name (nsig.n2sig_name)
val bp = app_bind (nsig, fvs)
val res = t2ype_subst (nsig.n2sig_res, bp.0)
//
fun outcells
  (t2ps: t2ypelst): list0 (cell) =
(
  case+ t2ps of
  | list0_nil () => list0_nil ()
  | list0_cons (t2p, t2ps) => let
      val t2p = open_exi_ground (t2p, name)
      val c = (
        case+ t2p of
        | T2YPErate (knd, elt, clk) =>
            if eval_gated (knd) then let
              val () = cgerr (str3
                ("codegen: gated flows on extern node [", name,
                 "] are not supported yet"))
            in
              cell_new (ctx, false, 1, 0)
            end else let
              val nd = eval_clk (clk)
            in
              cell_new (ctx, t2ype_isbool (elt, bp.1), nd.0, nd.1)
            end
        | _ (*rest*) => let
            val () = cgerr (str3
              ("codegen: extern node [", name, "] must return flows"))
          in
            cell_new (ctx, false, 1, 0)
          end
      ) : cell
    in
      list0_cons (c, outcells (t2ps))
    end
) (* end of [outcells] *)
//
val () = let
  fun checkargs (fvs: list0(fval)): void =
    case+ fvs of
    | list0_nil () => ()
    | list0_cons (fv, fvs) => let
        val () = (
          case+ fv of
          | FVgated _ => cgerr (str3
              ("codegen: gated flows on extern node [", name,
               "] are not supported yet"))
          | _ (*rest*) => ()
        ) : void
      in
        checkargs (fvs)
      end
in
  checkargs (fvs)
end // end of [val]
//
val outs = outcells (restypes_of (res))
val () = emit_strict (ctx, ACTcall (name, fvs, outs))
//
val () = (
  case+ outs of
  | list0_cons (o0, _) => let
      val w = (
        case+ nsig.n2sig_wcet of
        | Some0 (w) => eval_int (s2exp_subst (w, bp.0))
        | None0 () => 0
      ) : int
      val r = ctx.x_tasks
    in
      !r := list0_cons (@(name, o0.c_n, o0.c_d, w), !r)
    end
  | list0_nil () => ()
) : void // end of [val]
//
// record the extern declaration (deduplicated)
//
fun argbools
  (fvs: list0(fval)): list0 (bool) =
  case+ fvs of
  | list0_nil () => list0_nil ()
  | list0_cons (fv, fvs) => let
      val b = (
        case+ fv of FVcell (c) => c.c_bool | _ => false
      ) : bool
    in
      list0_cons (b, argbools (fvs))
    end
fun outbools
  (cs: list0(cell)): list0 (bool) =
  case+ cs of
  | list0_nil () => list0_nil ()
  | list0_cons (c, cs) => list0_cons (c.c_bool, outbools (cs))
val r = ctx.x_calls
fun seen
  (xs: list0(@(string, list0(bool), list0(bool)))): bool =
  case+ xs of
  | list0_nil () => false
  | list0_cons (x, xs) => if x.0 = name then true else seen (xs)
val () =
  if not (seen (!r)) then
    !r := list0_cons (@(name, argbools (fvs), outbools (outs)), !r)
//
fun tofvals
  (cs: list0(cell)): list0 (fval) =
  case+ cs of
  | list0_nil () => list0_nil ()
  | list0_cons (c, cs) => list0_cons (FVcell (c), tofvals (cs))
//
in
  tofvals (outs)
end // end of [apply_extern]

(* ****** ****** *)

implement
inline_node
  (ctx, n2, fvs) = let
//
val nsig = n2.n2ode_sig
val name = symbol_get_name (nsig.n2sig_name)
val bp = app_bind (nsig, fvs)
//
// fresh cells for returns and vars, with ground clocks
//
fun mkcells
  (d2vs: list0(d2var), env: fenv): @(list0(fval), fenv) =
(
  case+ d2vs of
  | list0_nil () => @(list0_nil (), env)
  | list0_cons (d2v, d2vs) => let
      val t2p = t2ype_subst (d2var_get_type (d2v), bp.0)
      (* existential returns: the witness comes from the pinned
         guard (the body was checked to match it) *)
      val t2p = open_exi_ground (t2p, name)
      val fv = (
        case+ t2p of
        | T2YPErate (knd, elt, clk) => let
            val nd = eval_clk (clk)
            val v = cell_new (ctx, t2ype_isbool (elt, bp.1), nd.0, nd.1)
          in
            if eval_gated (knd) then
              FVgated (v, cell_new (ctx, true(*bool*), nd.0, nd.1))
            else FVcell (v)
          end
        | _ (*rest*) => let
            val () = cgerr (str3
              ("codegen: flow of node [", name, "] must carry a rate type"))
          in
            FVcell (cell_new (ctx, false, 1, 0))
          end
      ) : fval
      val r = mkcells (d2vs, env)
    in
      @(list0_cons (fv, r.0),
        list0_cons (@(d2var_get_stamp (d2v), fv), r.1))
    end
) (* end of [mkcells] *)
//
// bind params to argument values
//
fun bindargs
  (d2vs: list0(d2var), fvs: list0(fval), env: fenv): fenv =
(
  case+ (d2vs, fvs) of
  | (list0_cons (d2v, d2vs), list0_cons (fv, fvs)) =>
      bindargs (d2vs, fvs,
        list0_cons (@(d2var_get_stamp (d2v), fv), env))
  | (_, _) => env
) (* end of [bindargs] *)
//
val env = bindargs (n2.n2ode_params, fvs, list0_nil ())
val rr = mkcells (n2.n2ode_returns, env)
val rv = mkcells (n2.n2ode_vars, rr.1)
val env = rv.1
//
// flatten the equations in causal order; a tuple equation is
// flattened once, when its first defined flow comes up
//
val defined = list0_append (n2.n2ode_returns, n2.n2ode_vars)
val order = erasure_topo_order (name, defined, n2.n2ode_eqns)
//
fn eqn_of
  (d2v: d2var): option0 (e2qn) = let
  fun inlhs (lhs: list0(d2var)): bool =
    case+ lhs of
    | list0_nil () => false
    | list0_cons (d2w, lhs) =>
        if eq_d2var_d2var (d2v, d2w) then true else inlhs (lhs)
  fun loop (eqns: list0(e2qn)): option0 (e2qn) =
    case+ eqns of
    | list0_nil () => None0 ()
    | list0_cons (eqn, eqns) =>
        if inlhs (eqn.e2qn_lhs) then Some0 (eqn) else loop (eqns)
in
  loop (n2.n2ode_eqns)
end // end of [eqn_of]
//
fun doeqns
  (order: list0(d2var), done: list0(int)): void =
(
  case+ order of
  | list0_nil () => ()
  | list0_cons (d2v, order) => (
    case+ eqn_of (d2v) of
    | None0 () => doeqns (order, done)
    | Some0 (eqn) => let
        val key = (
          case+ eqn.e2qn_lhs of
          | list0_cons (d2w, _) => d2var_get_stamp (d2w)
          | list0_nil () => ~1
        ) : int
        fun mem (xs: list0(int)): bool =
          case+ xs of
          | list0_nil () => false
          | list0_cons (x, xs) => if x = key then true else mem (xs)
      in
        if mem (done) then doeqns (order, done)
        else let
          val rhs = flatten (ctx, env, eqn.e2qn_rhs)
          fun copies
            (lhs: list0(d2var), rhs: list0(fval)): void =
            case+ (lhs, rhs) of
            | (list0_cons (d2v, lhs), list0_cons (fv, rhs)) => let
                val () = (
                  case+ (fenv_find (env, d2v), fv) of
                  | (FVgated (lv, lp), FVgated (rv, rp)) => let
                      val () = emit_strict (ctx, ACTcopy (FVcell (rv), lv))
                    in
                      emit_strict (ctx, ACTcopy (FVcell (rp), lp))
                    end
                  | (lfv, rfv) => let
                      val lc = fval_cell (lfv, "an equation")
                    in
                      emit_strict (ctx, ACTcopy (rfv, lc))
                    end
                ) : void
              in
                copies (lhs, rhs)
              end
            | (_, _) => ()
          val () = copies (eqn.e2qn_lhs, rhs)
        in
          doeqns (order, list0_cons (key, done))
        end
      end
    )
) (* end of [doeqns] *)
//
val () = doeqns (order, list0_nil ())
//
in
  rr.0
end // end of [inline_node]

(* ****** ****** *)
//
// emission
//
fn cellname (out: FILEref, c: cell): void =
  fprint! (out, "c", c.c_id)

fn emit_fval
  (out: FILEref, fv: fval): void =
(
  case+ fv of
  | FVcell (c) => fprint! (out, "!c", c.c_id)
  | FVint (i) =>
      if i < 0 then fprint! (out, "(~", ~i, ")") else fprint! (out, i)
  | FVrat (a, b) => fprint! (out, "(", a, " / ", b, ")") (* unused *)
  | FVgated (v, _) => fprint! (out, "!c", v.c_id) (* value component *)
) (* end of [emit_fval] *)

fn emit_fire
  (out: FILEref, n: int, d: int): void =
  fprint! (out, "if fire (t, ", n, ", ", d, ") then ")

fn emit_action
  (out: FILEref, a: action): void =
(
case+ a of
//
| ACTsense (name, c) => let
    val () = emit_fire (out, c.c_n, c.c_d)
  in
    fprint! (out, "!c", c.c_id, " := ", name, "_sense (t)")
  end
//
| ACTcall (name, args, outs) => (
    case+ name of
    | "%merge" => (
        case+ (args, outs) of
        | (list0_cons (b, list0_cons (x, list0_cons (y, list0_nil ()))),
           list0_cons (o, list0_nil ())) => let
            val () = emit_fire (out, o.c_n, o.c_d)
            val () = fprint! (out, "!c", o.c_id, " := (if ")
            val () = emit_fval (out, b)
            val () = fprint! (out, " then ")
            val () = emit_fval (out, x)
            val () = fprint! (out, " else ")
            val () = emit_fval (out, y)
          in
            fprint! (out, ")")
          end
        | (_, _) => ()
      )
    | _ (*extern call*) => (
        case+ outs of
        | list0_cons (o0, list0_nil ()) => let
            val () = emit_fire (out, o0.c_n, o0.c_d)
            val () = fprint! (out, "!c", o0.c_id, " := ", name, " (")
            fun loop (args: list0(fval), first: bool): void =
              case+ args of
              | list0_nil () => ()
              | list0_cons (fv, args) => let
                  val () = if not (first) then fprint! (out, ", ")
                  val () = emit_fval (out, fv)
                in
                  loop (args, false)
                end
            val () = loop (args, true)
          in
            fprint! (out, ")")
          end
        | list0_cons (o0, _) => let
            val () = emit_fire (out, o0.c_n, o0.c_d)
            val () = fprint! (out, "let val (")
            fun routs (outs: list0(cell), i: int): void =
              case+ outs of
              | list0_nil () => ()
              | list0_cons (_, outs) => let
                  val () = if i > 0 then fprint! (out, ", ")
                  val () = fprint! (out, "r", i)
                in
                  routs (outs, i + 1)
                end
            val () = routs (outs, 0)
            val () = fprint! (out, ") = ", name, " (")
            fun loop (args: list0(fval), first: bool): void =
              case+ args of
              | list0_nil () => ()
              | list0_cons (fv, args) => let
                  val () = if not (first) then fprint! (out, ", ")
                  val () = emit_fval (out, fv)
                in
                  loop (args, false)
                end
            val () = loop (args, true)
            val () = fprint! (out, ") in ")
            fun wouts (outs: list0(cell), i: int): void =
              case+ outs of
              | list0_nil () => ()
              | list0_cons (o, outs) => let
                  val () = if i > 0 then fprint! (out, "; ")
                  val () = fprint! (out, "!c", o.c_id, " := r", i)
                in
                  wouts (outs, i + 1)
                end
            val () = wouts (outs, 0)
          in
            fprint! (out, " end")
          end
        | list0_nil () => ()
      )
  )
//
| ACTcopy (fv, o) => let
    val () = emit_fire (out, o.c_n, o.c_d)
    val () = fprint! (out, "!c", o.c_id, " := ")
  in
    emit_fval (out, fv)
  end
//
| ACTfbyemit (v0, st, o) => let
    val () = emit_fire (out, o.c_n, o.c_d)
    val () = fprint!
      (out, "!c", o.c_id, " := (if t = ", o.c_d, " then ")
    val () =
      if v0 < 0 then fprint! (out, "(~", ~v0, ")") else fprint! (out, v0)
  in
    fprint! (out, " else !c", st.c_id, ")")
  end
//
| ACTfbystore (i, st) => let
    val () = emit_fire (out, i.c_n, i.c_d)
  in
    fprint! (out, "!c", st.c_id, " := !c", i.c_id)
  end
//
| ACTconsemit (v0, i, o) => let
    val () = emit_fire (out, o.c_n, o.c_d)
    val () = fprint!
      (out, "!c", o.c_id, " := (if t = ", o.c_d, " then ")
    val () =
      if v0 < 0 then fprint! (out, "(~", ~v0, ")") else fprint! (out, v0)
  in
    fprint! (out, " else !c", i.c_id, ")")
  end
//
| ACTbufwrite (i, b) => let
    val () = emit_fire (out, i.c_n, i.c_d)
  in
    fprint! (out,
      "(b", b.b_id, "[!b", b.b_id, "_w] := !c", i.c_id,
      "; !b", b.b_id, "_w := (!b", b.b_id, "_w + 1) mod ", b.b_depth, ")")
  end
//
| ACTbufread (b, o) => let
    val () = emit_fire (out, o.c_n, o.c_d)
  in
    fprint! (out,
      "(!c", o.c_id, " := b", b.b_id, "[!b", b.b_id, "_r]",
      "; !b", b.b_id, "_r := (!b", b.b_id, "_r + 1) mod ", b.b_depth, ")")
  end
//
| ACTcurrent (v0, v, p, o) => let
    val () = emit_fire (out, o.c_n, o.c_d)
    val () = fprint! (out,
      "!c", o.c_id, " := (if !c", p.c_id, " then !c", v.c_id,
      " else (if t = ", o.c_d, " then ")
    val () =
      if v0 < 0 then fprint! (out, "(~", ~v0, ")") else fprint! (out, v0)
  in
    fprint! (out, " else !c", o.c_id, "))")
  end
//
| ACTactuate (name, c) => let
    val () = emit_fire (out, c.c_n, c.c_d)
  in
    fprint! (out, name, "_actuate (t, !c", c.c_id, ")")
  end
//
| ACTactuate_gated (name, v, p) => let
    val () = emit_fire (out, v.c_n, v.c_d)
  in
    fprint! (out,
      "(if !c", p.c_id, " then ", name, "_actuate (t, !c", v.c_id, "))")
  end
//
) (* end of [emit_action] *)

fn emit_actions
  (out: FILEref, acts: list0(action)): void = let
  fun loop (acts: list0(action)): void =
    case+ acts of
    | list0_nil () => ()
    | list0_cons (a, acts) => let
        val () = fprint! (out, "  val () = ")
        val () = emit_action (out, a)
        val () = fprint! (out, "\n")
      in
        loop (acts)
      end
in
  loop (acts)
end // end of [emit_actions]

(* ****** ****** *)
//
// extern declarations (shared verbatim by both generated files)
//
fn emit_externs
  (out: FILEref, ctx: ccctx): void = let
//
fn tystr (b: bool): string = if b then "bool" else "int"
//
fun senses
  (xs: list0(@(string, bool))): void =
  case+ xs of
  | list0_nil () => ()
  | list0_cons (x, xs) => let
      val () = fprint! (out,
        "extern fun ", x.0, "_sense (t: int): ", tystr (x.1),
        " = \"ext#", x.0, "_sense\"\n")
    in
      senses (xs)
    end
val () = senses (list0_reverse (!(ctx.x_senses)))
//
fun acts
  (xs: list0(@(string, bool))): void =
  case+ xs of
  | list0_nil () => ()
  | list0_cons (x, xs) => let
      val () = fprint! (out,
        "extern fun ", x.0, "_actuate (t: int, v: ", tystr (x.1),
        "): void = \"ext#", x.0, "_actuate\"\n")
    in
      acts (xs)
    end
val () = acts (list0_reverse (!(ctx.x_acts)))
//
fun calls
  (xs: list0(@(string, list0(bool), list0(bool)))): void =
  case+ xs of
  | list0_nil () => ()
  | list0_cons (x, xs) => let
      val () = fprint! (out, "extern fun ", x.0, " (")
      fun args (bs: list0(bool), i: int): void =
        case+ bs of
        | list0_nil () => ()
        | list0_cons (b, bs) => let
            val () = if i > 0 then fprint! (out, ", ")
            val () = fprint! (out, "a", i, ": ", tystr (b))
          in
            args (bs, i + 1)
          end
      val () = args (x.1, 0)
      val () = fprint! (out, "): ")
      val nouts = list0_length (x.2)
      val () = (
        case+ x.2 of
        | list0_cons (b, list0_nil ()) => fprint! (out, tystr (b))
        | _ (*tuple*) => let
            val () = fprint! (out, "(")
            fun outs (bs: list0(bool), i: int): void =
              case+ bs of
              | list0_nil () => ()
              | list0_cons (b, bs) => let
                  val () = if i > 0 then fprint! (out, ", ")
                  val () = fprint! (out, tystr (b))
                in
                  outs (bs, i + 1)
                end
            val () = outs (x.2, 0)
          in
            fprint! (out, ")")
          end
      ) : void
      val () = fprint! (out, " = \"ext#", x.0, "\"\n")
    in
      calls (xs)
    end
val () = calls (list0_reverse (!(ctx.x_calls)))
//
in
  // nothing
end // end of [emit_externs]

(* ****** ****** *)

(* ****** ****** *)
//
// the schedule certificate (ATS backend): tasks released in each
// base-tick slot must fit within it -- the timing model of the
// sequential step(t). The generated program constructs the schedule
// through a budget-indexed builder, so patsopt proves each slot's
// capacity on ground literals; an infeasible task set fails to
// typecheck.
//
fn ctx_timing
  (ctx: ccctx): @(int(*base*), int(*hyper*), int(*maxd*)) = let
  fun basetick (cs: list0(cell), g: int): int =
    case+ cs of
    | list0_nil () => g
    | list0_cons (c, cs) =>
        basetick (cs, gcd (gcd (g, c.c_n), c.c_d))
  val base = basetick (!(ctx.x_cells), 0)
  val base = (if base <= 0 then 1 else base): int
  fun hyper (cs: list0(cell), h: int): int =
    case+ cs of
    | list0_nil () => h
    | list0_cons (c, cs) => hyper (cs, lcm (h, c.c_n))
  val hp = hyper (!(ctx.x_cells), 1)
  val hp = (if hp <= 0 then 1 else hp): int
  fun maxd (cs: list0(cell), m: int): int =
    case+ cs of
    | list0_nil () => m
    | list0_cons (c, cs) =>
        maxd (cs, if c.c_d > m then c.c_d else m)
  val md = maxd (!(ctx.x_cells), 0)
in
  @(base, hp, md)
end // end of [ctx_timing]

fn emit_schedlib_sats
  (out: FILEref): void =
(
  fprint! (out,
"(*\n** generated by overture --codegen=ats: the schedule certificate.\n** Constructing a [schedule] value proves, slot by slot, that the\n** work released in each base-tick slot fits within it; a task set\n** that overflows a slot makes the generated program ill-typed.\n*)\n\nabstype slotbuild (r: int) = ptr (* remaining capacity *)\nabstype schedbuild = ptr\nabstype schedule = ptr\n\nfun schedule_begin (base: int, hyper: int): schedbuild\nfun slot_begin {g:int | g > 0} (g: int (g)): slotbuild (g)\nfun slot_add {r,c:int | 0 <= c; c <= r}\n  (sl: slotbuild (r), tid: int, wcet: int (c)): slotbuild (r - c)\nfun schedule_slot {r:int | r >= 0}\n  (sb: schedbuild, date: int, sl: slotbuild (r)): schedbuild\nfun schedule_end (sb: schedbuild): schedule\n")
) (* end of [emit_schedlib_sats] *)

fn emit_schedlib_dats
  (out: FILEref): void =
(
  fprint! (out,
"(*\n** generated by overture --codegen=ats: the schedule certificate.\n** The runtime representation is a token; the value of a schedule\n** is its typability.\n*)\n\n#define ATS_DYNLOADFLAG 0\n\n#include \"share/atspre_staload.hats\"\n\nstaload \"./overture_sched.sats\"\n\ndatatype schedrep = SCHEDREP of ()\n\nassume slotbuild (r: int) = schedrep\nassume schedbuild = schedrep\nassume schedule = schedrep\n\nimplement schedule_begin (b, h) = SCHEDREP ()\nimplement slot_begin {g} (g) = SCHEDREP ()\nimplement slot_add {r,c} (sl, tid, w) = sl\nimplement schedule_slot {r} (sb, d, sl) = sb\nimplement schedule_end (sb) = sb\n")
) (* end of [emit_schedlib_dats] *)

fn emit_schedule_ats
  (out: FILEref, ctx: ccctx): void = let
//
val tm = ctx_timing (ctx)
val g = tm.0 and hp = tm.1 and md = tm.2
val tasks = list0_reverse (!(ctx.x_tasks))
//
val () = fprint! (out,
  "(*\n** the schedule certificate: constructing [the_schedule]\n",
  "** proves the per-slot capacity condition (base tick ", g,
  ", hyperperiod ", hp, ")\n*)\n")
val () = fprint! (out, "val the_schedule = let\n")
val () = fprint! (out,
  "  val b = schedule_begin (", g, ", ", hp, ")\n")
//
fun fires
  (n: int, d: int, t: int): bool =
  if t >= d then ((t - d) mod n) = 0 else false
//
fun slot_rows
  (ts: list0(@(string, int, int, int)), t: int, i: int
  ) : list0 (@(int, int, string)) =
(
  case+ ts of
  | list0_nil () => list0_nil ()
  | list0_cons (tk, ts) =>
      if fires (tk.1, tk.2, t)
        then list0_cons (@(i, tk.3, tk.0), slot_rows (ts, t, i + 1))
        else slot_rows (ts, t, i + 1)
      // end of [if]
) (* end of [slot_rows] *)
//
fun slots (t: int, lim: int): void =
  if t < lim then let
    val rows = slot_rows (tasks, t, 0)
    val () = (
      case+ rows of
      | list0_nil () => ()
      | list0_cons _ => let
          val () = fprint! (out,
            "  val s = slot_begin (", g, ") (* date ", t, " *)\n")
          fun add (rows: list0(@(int, int, string))): void =
            case+ rows of
            | list0_nil () => ()
            | list0_cons (r, rows) => let
                val () = fprint! (out,
                  "  val s = slot_add (s, ", r.0, ", ", r.1,
                  ") (* ", r.2, " *)\n")
              in
                add (rows)
              end
          val () = add (rows)
        in
          fprint! (out,
            "  val b = schedule_slot (b, ", t, ", s)\n")
        end
    ) : void
  in
    slots (t + g, lim)
  end // end of [slots]
//
val () = slots (0, md + hp)
val () = fprint! (out, "in\n  schedule_end (b)\nend\n\n")
//
in
  // nothing
end // end of [emit_schedule_ats]

fn emit_tasktbl_c
  (out: FILEref, ctx: ccctx): void = let
//
val tasks = list0_reverse (!(ctx.x_tasks))
val n = list0_length (tasks)
//
val () = fprint! (out,
  "/* the task table: period, offset, wcet per extern instance.\n",
  " * NOTE: the C backend carries no schedulability certificate;\n",
  " * feasibility is proven only by the ATS backend (or externally). */\n",
  "typedef struct { const char *name; int period; int offset; int wcet; } ov_task;\n",
  "static const ov_task overture_tasks[] = {\n")
fun rows
  (ts: list0(@(string, int, int, int))): void =
  case+ ts of
  | list0_nil () => ()
  | list0_cons (tk, ts) => let
      val () = fprint! (out,
        "  { \"", tk.0, "\", ", tk.1, ", ", tk.2, ", ", tk.3, " },\n")
    in
      rows (ts)
    end
val () = rows (tasks)
val () = fprint! (out, "  { 0, 0, 0, 0 }\n};\n")
val () = fprint! (out,
  "static const int overture_ntasks = ", n, ";\n\n")
//
in
  // nothing
end // end of [emit_tasktbl_c]

fn emit_gen
(
  out: FILEref, ctx: ccctx, base: string
) : void = let
//
val () = fprint! (out,
  "(*\n** generated by overture --codegen (", base, ")\n",
  "** logical-time harness; do not edit\n*)\n\n")
val () = fprint! (out, "#include \"share/atspre_staload.hats\"\n\n")
val () = fprint! (out, "staload \"./overture_sched.sats\"\n\n")
//
val () = emit_externs (out, ctx)
val () = fprint! (out, "\n")
//
// cells and state
//
fun cells (cs: list0(cell)): void =
  case+ cs of
  | list0_nil () => ()
  | list0_cons (c, cs) => let
      val () =
        if c.c_bool
          then fprint! (out, "val c", c.c_id, " = ref<bool> (false)\n")
          else fprint! (out, "val c", c.c_id, " = ref<int> (0)\n")
    in
      cells (cs)
    end
val () = cells (list0_reverse (!(ctx.x_cells)))
val () = cells (list0_reverse (!(ctx.x_states)))
//
fun bufs (bs: list0(buf)): void =
  case+ bs of
  | list0_nil () => ()
  | list0_cons (b, bs) => let
      val ty = (if b.b_bool then "bool" else "int"): string
      val v0 = (if b.b_bool then "false" else "0"): string
      val () = fprint! (out,
        "val b", b.b_id, " = arrszref_make_elt<", ty, "> (i2sz(",
        b.b_depth, "), ", v0, ")\n")
      val () = fprint! (out, "val b", b.b_id, "_r = ref<int> (0)\n")
      val () = fprint! (out, "val b", b.b_id, "_w = ref<int> (0)\n")
    in
      bufs (bs)
    end
val () = bufs (list0_reverse (!(ctx.x_bufs)))
//
val () = fprint! (out, "\n")
val () = fprint! (out,
  "fn fire (t: int, n: int, d: int): bool =\n",
  "  if t >= d then ((t - d) mod n) = 0 else false\n\n")
//
// the step function: pre, strict, post
//
val () = fprint! (out, "fn step (t: int): void = let\n")
val () = emit_actions (out, list0_reverse (!(ctx.x_pre)))
val () = emit_actions (out, list0_reverse (!(ctx.x_strict)))
val () = emit_actions (out, list0_reverse (!(ctx.x_post)))
val () = fprint! (out, "in () end\n\n")
//
val () = emit_schedule_ats (out, ctx)
//
// base tick, hyperperiod, main loop
//
fun basetick
  (cs: list0(cell), g: int): int =
  case+ cs of
  | list0_nil () => g
  | list0_cons (c, cs) =>
      basetick (cs, gcd (gcd (g, c.c_n), c.c_d))
val base = basetick (!(ctx.x_cells), 0)
val base = (if base <= 0 then 1 else base): int
//
fun hyper
  (cs: list0(cell), h: int): int =
  case+ cs of
  | list0_nil () => h
  | list0_cons (c, cs) => hyper (cs, lcm (h, c.c_n))
val hp = hyper (!(ctx.x_cells), 1)
//
fun maxd (cs: list0(cell), m: int): int =
  case+ cs of
  | list0_nil () => m
  | list0_cons (c, cs) =>
      maxd (cs, if c.c_d > m then c.c_d else m)
val md = maxd (!(ctx.x_cells), 0)
//
val dflt = md + 2 * hp
//
val () = fprint! (out,
  "implement main0 (argc, argv) = let\n",
  "  val ticks = (if argc >= 2 then g0string2int (argv[1]) else ",
  dflt, "): int\n",
  "  fun loop (t: int): void =\n",
  "    if t < ticks then (step (t); loop (t + ", base, "))\n",
  "in\n  loop (0)\nend\n")
//
in
  // nothing
end // end of [emit_gen]

(* ****** ****** *)

fn emit_stubs
(
  out: FILEref, ctx: ccctx, base: string
) : void = let
//
val () = fprint! (out,
  "(*\n** generated by overture --codegen (", base, ")\n",
  "** deterministic default stubs; replace for real IO\n*)\n\n")
val () = fprint! (out, "#define ATS_DYNLOADFLAG 0\n\n")
val () = fprint! (out, "#include \"share/atspre_staload.hats\"\n\n")
val () = fprint! (out, "staload \"./overture_sched.sats\"\n\n")
//
val () = emit_externs (out, ctx)
val () = fprint! (out, "\n")
//
fun senses
  (xs: list0(@(string, bool))): void =
  case+ xs of
  | list0_nil () => ()
  | list0_cons (x, xs) => let
      val () =
        if x.1
          then fprint! (out,
            "implement ", x.0, "_sense (t) = ((t mod 3) = 0)\n")
          else fprint! (out, "implement ", x.0, "_sense (t) = t\n")
    in
      senses (xs)
    end
val () = senses (list0_reverse (!(ctx.x_senses)))
//
fun acts
  (xs: list0(@(string, bool))): void =
  case+ xs of
  | list0_nil () => ()
  | list0_cons (x, xs) => let
      val () = fprint! (out,
        "implement ", x.0, "_actuate (t, v) = ",
        "println! (\"[t=\", t, \"] ", x.0, " = \", v)\n")
    in
      acts (xs)
    end
val () = acts (list0_reverse (!(ctx.x_acts)))
//
fun calls
  (xs: list0(@(string, list0(bool), list0(bool)))): void =
  case+ xs of
  | list0_nil () => ()
  | list0_cons (x, xs) => let
      val () = fprint! (out, "implement ", x.0, " (")
      fun args (bs: list0(bool), i: int): void =
        case+ bs of
        | list0_nil () => ()
        | list0_cons (_, bs) => let
            val () = if i > 0 then fprint! (out, ", ")
            val () = fprint! (out, "a", i)
          in
            args (bs, i + 1)
          end
      val () = args (x.1, 0)
      val () = fprint! (out, ") = ")
      (* sum of the inputs, bools counted as 0/1 *)
      fn sum (): void = let
        fun loop (bs: list0(bool), i: int, first: bool): bool =
          case+ bs of
          | list0_nil () => first
          | list0_cons (b, bs) => let
              val () = if not (first) then fprint! (out, " + ")
              val () =
                if b then fprint! (out, "(if a", i, " then 1 else 0)")
                else fprint! (out, "a", i)
            in
              loop (bs, i + 1, false)
            end
        val first = loop (x.1, 0, true)
        val () = if first then fprint! (out, "0")
      in
        // nothing
      end
      val () = (
        case+ x.2 of
        | list0_cons (b, list0_nil ()) =>
            if b then fprint! (out, "true")
            else (fprint! (out, "("); sum (); fprint! (out, " + 0)"))
        | _ (*tuple*) => let
            val () = fprint! (out, "(")
            fun outs (bs: list0(bool), i: int): void =
              case+ bs of
              | list0_nil () => ()
              | list0_cons (b, bs) => let
                  val () = if i > 0 then fprint! (out, ", ")
                  val () =
                    if b then fprint! (out, "true")
                    else (fprint! (out, "("); sum ();
                          fprint! (out, " + ", i, ")"))
                in
                  outs (bs, i + 1)
                end
            val () = outs (x.2, 0)
          in
            fprint! (out, ")")
          end
      ) : void
      val () = fprint! (out, "\n")
    in
      calls (xs)
    end
val () = calls (list0_reverse (!(ctx.x_calls)))
//
in
  // nothing
end // end of [emit_stubs]

(* ****** ****** *)
//
// the C backend: same cells/buffers/actions, emitted as plain C99
//
fn ctystr (b: bool): string = if b then "bool" else "int"

fn emit_fval_c
  (out: FILEref, fv: fval): void =
(
  case+ fv of
  | FVcell (c) => fprint! (out, "c", c.c_id)
  | FVgated (v, _) => fprint! (out, "c", v.c_id)
  | FVint (i) => fprint! (out, i)
  | FVrat (a, b) => fprint! (out, "(", a, " / ", b, ")") (* unused *)
) (* end of [emit_fval_c] *)

fn emit_fire_c
  (out: FILEref, n: int, d: int): void =
  fprint! (out, "if (fire(t, ", n, ", ", d, ")) ")

fn emit_action_c
  (out: FILEref, a: action): void =
(
case+ a of
//
| ACTsense (name, c) => let
    val () = emit_fire_c (out, c.c_n, c.c_d)
  in
    fprint! (out, "c", c.c_id, " = ", name, "_sense(t);")
  end
//
| ACTcall (name, args, outs) => (
    case+ name of
    | "%merge" => (
        case+ (args, outs) of
        | (list0_cons (b, list0_cons (x, list0_cons (y, list0_nil ()))),
           list0_cons (o, list0_nil ())) => let
            val () = emit_fire_c (out, o.c_n, o.c_d)
            val () = fprint! (out, "c", o.c_id, " = (")
            val () = emit_fval_c (out, b)
            val () = fprint! (out, " ? ")
            val () = emit_fval_c (out, x)
            val () = fprint! (out, " : ")
            val () = emit_fval_c (out, y)
          in
            fprint! (out, ");")
          end
        | (_, _) => ()
      )
    | _ (*extern call*) => let
        fun cargs
          (args: list0(fval), first: bool): void =
          case+ args of
          | list0_nil () => ()
          | list0_cons (fv, args) => let
              val () = if not (first) then fprint! (out, ", ")
              val () = emit_fval_c (out, fv)
            in
              cargs (args, false)
            end
      in
        case+ outs of
        | list0_cons (o0, list0_nil ()) => let
            val () = emit_fire_c (out, o0.c_n, o0.c_d)
            val () = fprint! (out, "c", o0.c_id, " = ", name, "(")
            val () = cargs (args, true)
          in
            fprint! (out, ");")
          end
        | list0_cons (o0, _) => let
            val () = emit_fire_c (out, o0.c_n, o0.c_d)
            val () = fprint! (out, name, "(")
            val () = cargs (args, true)
            fun couts
              (outs: list0(cell), first: bool): void =
              case+ outs of
              | list0_nil () => ()
              | list0_cons (o, outs) => let
                  val () = if not (first) then fprint! (out, ", ")
                  val () = fprint! (out, "&c", o.c_id)
                in
                  couts (outs, false)
                end
            val () = (
              case+ args of
              | list0_nil () => couts (outs, true)
              | list0_cons _ => (fprint! (out, ", "); couts (outs, true))
            ) : void
          in
            fprint! (out, ");")
          end
        | list0_nil () => ()
      end
  )
//
| ACTcopy (fv, o) => let
    val () = emit_fire_c (out, o.c_n, o.c_d)
    val () = fprint! (out, "c", o.c_id, " = ")
    val () = emit_fval_c (out, fv)
  in
    fprint! (out, ";")
  end
//
| ACTfbyemit (v0, st, o) => let
    val () = emit_fire_c (out, o.c_n, o.c_d)
  in
    fprint! (out,
      "c", o.c_id, " = (t == ", o.c_d, ") ? ", v0, " : c", st.c_id, ";")
  end
//
| ACTfbystore (i, st) => let
    val () = emit_fire_c (out, i.c_n, i.c_d)
  in
    fprint! (out, "c", st.c_id, " = c", i.c_id, ";")
  end
//
| ACTconsemit (v0, i, o) => let
    val () = emit_fire_c (out, o.c_n, o.c_d)
  in
    fprint! (out,
      "c", o.c_id, " = (t == ", o.c_d, ") ? ", v0, " : c", i.c_id, ";")
  end
//
| ACTbufwrite (i, b) => let
    val () = emit_fire_c (out, i.c_n, i.c_d)
  in
    fprint! (out,
      "{ b", b.b_id, "[b", b.b_id, "_w] = c", i.c_id,
      "; b", b.b_id, "_w = (b", b.b_id, "_w + 1) % ", b.b_depth, "; }")
  end
//
| ACTbufread (b, o) => let
    val () = emit_fire_c (out, o.c_n, o.c_d)
  in
    fprint! (out,
      "{ c", o.c_id, " = b", b.b_id, "[b", b.b_id, "_r]",
      "; b", b.b_id, "_r = (b", b.b_id, "_r + 1) % ", b.b_depth, "; }")
  end
//
| ACTcurrent (v0, v, p, o) => let
    val () = emit_fire_c (out, o.c_n, o.c_d)
  in
    fprint! (out,
      "c", o.c_id, " = c", p.c_id, " ? c", v.c_id,
      " : ((t == ", o.c_d, ") ? ", v0, " : c", o.c_id, ");")
  end
//
| ACTactuate (name, c) => let
    val () = emit_fire_c (out, c.c_n, c.c_d)
  in
    fprint! (out, name, "_actuate(t, c", c.c_id, ");")
  end
//
| ACTactuate_gated (name, v, p) => let
    val () = emit_fire_c (out, v.c_n, v.c_d)
  in
    fprint! (out,
      "{ if (c", p.c_id, ") ", name, "_actuate(t, c", v.c_id, "); }")
  end
//
) (* end of [emit_action_c] *)

fn emit_actions_c
  (out: FILEref, acts: list0(action)): void = let
  fun loop (acts: list0(action)): void =
    case+ acts of
    | list0_nil () => ()
    | list0_cons (a, acts) => let
        val () = fprint! (out, "  ")
        val () = emit_action_c (out, a)
        val () = fprint! (out, "\n")
      in
        loop (acts)
      end
in
  loop (acts)
end // end of [emit_actions_c]

(*
** shared prototype printer: [defn] selects declarations (extern,
** with a trailing semicolon) or definition headers for the stubs
*)
fn emit_protos_c
  (out: FILEref, ctx: ccctx, defn: bool): void = let
//
fn pre (): void =
  if not (defn) then fprint! (out, "extern ")
//
fun senses
  (xs: list0(@(string, bool))): void =
  case+ xs of
  | list0_nil () => ()
  | list0_cons (x, xs) => let
      val () = pre ()
      val () = fprint! (out, ctystr (x.1), " ", x.0, "_sense(int t)")
      val () =
        if defn then (
          if x.1
            then fprint! (out, " { return (t % 3) == 0; }\n")
            else fprint! (out, " { return t; }\n")
        ) else fprint! (out, ";\n")
    in
      senses (xs)
    end
val () = senses (list0_reverse (!(ctx.x_senses)))
//
fun acts
  (xs: list0(@(string, bool))): void =
  case+ xs of
  | list0_nil () => ()
  | list0_cons (x, xs) => let
      val () = pre ()
      val () = fprint! (out,
        "void ", x.0, "_actuate(int t, ", ctystr (x.1), " v)")
      val () =
        if defn then (
          if x.1
            then fprint! (out,
              " { printf(\"[t=%d] ", x.0,
              " = %s\\n\", t, v ? \"true\" : \"false\"); }\n")
            else fprint! (out,
              " { printf(\"[t=%d] ", x.0, " = %d\\n\", t, v); }\n")
        ) else fprint! (out, ";\n")
    in
      acts (xs)
    end
val () = acts (list0_reverse (!(ctx.x_acts)))
//
fun calls
  (xs: list0(@(string, list0(bool), list0(bool)))): void =
  case+ xs of
  | list0_nil () => ()
  | list0_cons (x, xs) => let
      val single = (
        case+ x.2 of
        | list0_cons (_, list0_nil ()) => true | _ => false
      ) : bool
      val () = pre ()
      val () = (
        case+ x.2 of
        | list0_cons (b, list0_nil ()) =>
            fprint! (out, ctystr (b), " ", x.0, "(")
        | _ (*rest*) => fprint! (out, "void ", x.0, "(")
      ) : void
      fun cargs (bs: list0(bool), i: int): int =
        case+ bs of
        | list0_nil () => i
        | list0_cons (b, bs) => let
            val () = if i > 0 then fprint! (out, ", ")
            val () = fprint! (out, ctystr (b), " a", i)
          in
            cargs (bs, i + 1)
          end
      val n = cargs (x.1, 0)
      val () =
        if not (single) then (
          let
            fun couts (bs: list0(bool), i: int, j: int): void =
              case+ bs of
              | list0_nil () => ()
              | list0_cons (b, bs) => let
                  val () = if (i + j) > 0 then fprint! (out, ", ")
                  val () = fprint! (out, ctystr (b), " *r", j)
                in
                  couts (bs, i, j + 1)
                end
          in
            couts (x.2, n, 0)
          end
        )
      val () =
        if (n = 0) andalso single then fprint! (out, "void")
      val () = fprint! (out, ")")
      val () =
        if defn then let
          (* stub body: each output is the sum of the inputs (+idx) *)
          fn sum (): void = let
            fun loop (bs: list0(bool), i: int, first: bool): bool =
              case+ bs of
              | list0_nil () => first
              | list0_cons (b, bs) => let
                  val () = if not (first) then fprint! (out, " + ")
                  val () =
                    if b then fprint! (out, "(a", i, " ? 1 : 0)")
                    else fprint! (out, "a", i)
                in
                  loop (bs, i + 1, false)
                end
            val first = loop (x.1, 0, true)
            val () = if first then fprint! (out, "0")
          in
            // nothing
          end
          val () = fprint! (out, " { ")
          val () = (
            case+ x.2 of
            | list0_cons (b, list0_nil ()) =>
                if b then fprint! (out, "return true; ")
                else (fprint! (out, "return ("); sum ();
                      fprint! (out, " + 0); "))
            | _ (*rest*) => let
                fun bodies (bs: list0(bool), j: int): void =
                  case+ bs of
                  | list0_nil () => ()
                  | list0_cons (b, bs) => let
                      val () =
                        if b then fprint! (out, "*r", j, " = true; ")
                        else (fprint! (out, "*r", j, " = (");
                              sum (); fprint! (out, " + ", j, "); "))
                    in
                      bodies (bs, j + 1)
                    end
              in
                bodies (x.2, 0)
              end
          ) : void
        in
          fprint! (out, "}\n")
        end
        else fprint! (out, ";\n")
    in
      calls (xs)
    end
val () = calls (list0_reverse (!(ctx.x_calls)))
//
in
  // nothing
end // end of [emit_protos_c]

fn emit_gen_c
(
  out: FILEref, ctx: ccctx, base: string
) : void = let
//
val () = fprint! (out,
  "/* generated by overture --codegen=c (", base, ");\n",
  " * logical-time harness; do not edit.\n",
  " * Compile with -DOVERTURE_FREESTANDING for bare-metal targets:\n",
  " * no hosted headers, no main; the runtime paces overture_step. */\n\n",
  "#ifndef OVERTURE_FREESTANDING\n",
  "#include <stdio.h>\n#include <stdlib.h>\n",
  "#endif\n",
  "#include <stdbool.h>\n\n")
//
val () = emit_protos_c (out, ctx, false)
val () = fprint! (out, "\n")
//
fun cells (cs: list0(cell)): void =
  case+ cs of
  | list0_nil () => ()
  | list0_cons (c, cs) => let
      val () =
        if c.c_bool
          then fprint! (out, "static bool c", c.c_id, " = false;\n")
          else fprint! (out, "static int c", c.c_id, " = 0;\n")
    in
      cells (cs)
    end
val () = cells (list0_reverse (!(ctx.x_cells)))
val () = cells (list0_reverse (!(ctx.x_states)))
//
fun bufs (bs: list0(buf)): void =
  case+ bs of
  | list0_nil () => ()
  | list0_cons (b, bs) => let
      val () = fprint! (out,
        "static ", ctystr (b.b_bool), " b", b.b_id, "[", b.b_depth, "];\n")
      val () = fprint! (out, "static int b", b.b_id, "_r = 0;\n")
      val () = fprint! (out, "static int b", b.b_id, "_w = 0;\n")
    in
      bufs (bs)
    end
val () = bufs (list0_reverse (!(ctx.x_bufs)))
//
val () = fprint! (out, "\n")
val () = emit_tasktbl_c (out, ctx)
val () = fprint! (out,
  "static int fire(int t, int n, int d) {\n",
  "  return t >= d && (t - d) % n == 0;\n}\n\n")
//
val () = fprint! (out, "static void step(int t) {\n")
val () = emit_actions_c (out, list0_reverse (!(ctx.x_pre)))
val () = emit_actions_c (out, list0_reverse (!(ctx.x_strict)))
val () = emit_actions_c (out, list0_reverse (!(ctx.x_post)))
val () = fprint! (out, "}\n\n")
//
fun basetick
  (cs: list0(cell), g: int): int =
  case+ cs of
  | list0_nil () => g
  | list0_cons (c, cs) =>
      basetick (cs, gcd (gcd (g, c.c_n), c.c_d))
val base = basetick (!(ctx.x_cells), 0)
val base = (if base <= 0 then 1 else base): int
//
fun hyper
  (cs: list0(cell), h: int): int =
  case+ cs of
  | list0_nil () => h
  | list0_cons (c, cs) => hyper (cs, lcm (h, c.c_n))
val hp = hyper (!(ctx.x_cells), 1)
//
fun maxd (cs: list0(cell), m: int): int =
  case+ cs of
  | list0_nil () => m
  | list0_cons (c, cs) =>
      maxd (cs, if c.c_d > m then c.c_d else m)
val md = maxd (!(ctx.x_cells), 0)
//
val dflt = md + 2 * hp
//
val () = fprint! (out,
  "/* the freestanding interface: the runtime paces logical time,\n",
  " * wrapping past one hyperperiod beyond the max offset so t\n",
  " * never overflows (the firing pattern is periodic there) */\n",
  "const int overture_base_tick = ", base, ";\n",
  "const int overture_hyperperiod = ", hp, ";\n",
  "const int overture_wrap_at = ", md + 2 * hp, ";\n",
  "void overture_step(int t) { step(t); }\n\n",
  "#ifndef OVERTURE_FREESTANDING\n",
  "int main(int argc, char **argv) {\n",
  "  int t;\n",
  "  int ticks = argc >= 2 ? atoi(argv[1]) : ", dflt, ";\n",
  "  for (t = 0; t < ticks; t += ", base, ") step(t);\n",
  "  return 0;\n}\n",
  "#endif\n")
//
in
  // nothing
end // end of [emit_gen_c]

fn emit_stubs_c
(
  out: FILEref, ctx: ccctx, base: string
) : void = let
//
val () = fprint! (out,
  "/* generated by overture --codegen=c (", base, ");\n",
  " * deterministic default stubs; replace for real IO */\n\n",
  "#include <stdio.h>\n#include <stdbool.h>\n\n")
//
val () = emit_protos_c (out, ctx, true)
//
in
  // nothing
end // end of [emit_stubs_c]

(* ****** ****** *)
//
// the bare-metal ATS backend (--codegen=ats-bare): the harness is
// freestanding ATS2 in the kernelats style. Cells and ring buffers
// are statics in a generated CATS floor with typed inline
// accessors -- no refs, no dynload, no allocation -- and the
// schedule certificate is a function whose typechecking (by the
// patsopt run that compiles the harness) proves the slot
// obligations: the artifact that flies is the certified one.
//
fn emit_atsfval
  (out: FILEref, fv: fval): void =
(
  case+ fv of
  | FVcell (c) => fprint! (out, "cell_get_", c.c_id, " ()")
  | FVgated (v, _) => fprint! (out, "cell_get_", v.c_id, " ()")
  | FVint (i) =>
      if i < 0 then fprint! (out, "(~", ~i, ")") else fprint! (out, i)
  | FVrat (a, b) => fprint! (out, "(", a, " / ", b, ")") (* unused *)
) (* end of [emit_atsfval] *)

fn emit_atsint
  (out: FILEref, i: int): void =
  if i < 0 then fprint! (out, "(~", ~i, ")") else fprint! (out, i)

fn emit_action_atsbare
  (out: FILEref, a: action): void = let
//
fn firehdr
  (out: FILEref, n: int, d: int): void =
  fprint! (out, "  val () = if fire (t, ", n, ", ", d, ") then ")
//
in
//
case+ a of
//
| ACTsense (name, c) => let
    val () = firehdr (out, c.c_n, c.c_d)
  in
    fprint! (out, "cell_set_", c.c_id, " (", name, "_sense (t))")
  end
//
| ACTcall (name, args, outs) => (
    case+ name of
    | "%merge" => (
        case+ (args, outs) of
        | (list0_cons (b, list0_cons (x, list0_cons (y, list0_nil ()))),
           list0_cons (o, list0_nil ())) => let
            val bc = fval_cell (b, "merge")
            val () = firehdr (out, o.c_n, o.c_d)
            val () = fprint! (out,
              "cell_set_", o.c_id, " (if cell_get_", bc.c_id, " () then ")
            val () = emit_atsfval (out, x)
            val () = fprint! (out, " else ")
            val () = emit_atsfval (out, y)
          in
            fprint! (out, ")")
          end
        | (_, _) => ()
      )
    | _ (*extern call*) => (
        case+ outs of
        | list0_cons (o0, list0_nil ()) => let
            val () = firehdr (out, o0.c_n, o0.c_d)
            val () = fprint! (out, "cell_set_", o0.c_id, " (", name, " (")
            fun loop (args: list0(fval), first: bool): void =
              case+ args of
              | list0_nil () => ()
              | list0_cons (fv, args) => let
                  val () = if not (first) then fprint! (out, ", ")
                  val () = emit_atsfval (out, fv)
                in
                  loop (args, false)
                end
            val () = loop (args, true)
          in
            fprint! (out, "))")
          end
        | list0_cons _ =>
            cgerr (str3 ("codegen: multi-output extern node [", name,
              "] is not yet supported by ats-bare"))
        | list0_nil () => ()
      )
  )
//
| ACTcopy (fv, o) => let
    val () = firehdr (out, o.c_n, o.c_d)
    val () = fprint! (out, "cell_set_", o.c_id, " (")
    val () = emit_atsfval (out, fv)
  in
    fprint! (out, ")")
  end
//
| ACTfbyemit (v0, st, o) => let
    val () = firehdr (out, o.c_n, o.c_d)
    val () = fprint! (out,
      "cell_set_", o.c_id, " (if t = ", o.c_d, " then ")
    val () = emit_atsint (out, v0)
  in
    fprint! (out, " else cell_get_", st.c_id, " ())")
  end
//
| ACTfbystore (i, st) => let
    val () = firehdr (out, i.c_n, i.c_d)
  in
    fprint! (out, "cell_set_", st.c_id, " (cell_get_", i.c_id, " ())")
  end
//
| ACTconsemit (v0, i, o) => let
    val () = firehdr (out, o.c_n, o.c_d)
    val () = fprint! (out,
      "cell_set_", o.c_id, " (if t = ", o.c_d, " then ")
    val () = emit_atsint (out, v0)
  in
    fprint! (out, " else cell_get_", i.c_id, " ())")
  end
//
| ACTbufwrite (i, b) => let
    val () = firehdr (out, i.c_n, i.c_d)
  in
    fprint! (out, "buf_write_", b.b_id, " (cell_get_", i.c_id, " ())")
  end
//
| ACTbufread (b, o) => let
    val () = firehdr (out, o.c_n, o.c_d)
  in
    fprint! (out, "cell_set_", o.c_id, " (buf_read_", b.b_id, " ())")
  end
//
| ACTcurrent (v0, v, pc, o) => let
    val () = firehdr (out, o.c_n, o.c_d)
    val () = fprint! (out,
      "cell_set_", o.c_id, " (if cell_get_", pc.c_id,
      " () then cell_get_", v.c_id, " () else (if t = ", o.c_d, " then ")
    val () = emit_atsint (out, v0)
  in
    fprint! (out, " else cell_get_", o.c_id, " ()))")
  end
//
| ACTactuate (name, c) => let
    val () = firehdr (out, c.c_n, c.c_d)
  in
    fprint! (out, name, "_actuate (t, cell_get_", c.c_id, " ())")
  end
//
| ACTactuate_gated (name, v, pc) => let
    val () = firehdr (out, v.c_n, v.c_d)
  in
    fprint! (out,
      "(if cell_get_", pc.c_id, " () then ",
      name, "_actuate (t, cell_get_", v.c_id, " ()))")
  end
//
end // end of [emit_action_atsbare]

fn emit_cells_cats
(
  out: FILEref, ctx: ccctx, base: string
) : void = let
//
val tm = ctx_timing (ctx)
//
val () = fprint! (out,
  "/* generated by overture --codegen=ats-bare (", base, ");\n",
  " * the state floor: every cell and ring buffer as a static with\n",
  " * typed inline accessors -- no allocation anywhere */\n\n",
  "#ifndef OVERTURE_CELLS_", base, "_CATS\n",
  "#define OVERTURE_CELLS_", base, "_CATS\n\n")
//
val () = fprint! (out,
  "const int overture_base_tick = ", tm.0, ";\n",
  "const int overture_hyperperiod = ", tm.1, ";\n",
  "const int overture_wrap_at = ", tm.2 + 2 * tm.1, ";\n\n")
//
fun cells (cs: list0(cell)): void =
  case+ cs of
  | list0_nil () => ()
  | list0_cons (c, cs) => let
      val ty = (if c.c_bool then "atstype_bool" else "int"): string
      val v0 = (if c.c_bool then "0" else "0"): string
      val () = fprint! (out,
        "static ", ty, " c", c.c_id, " = ", v0, ";\n",
        "ATSinline() ", ty, " cell_get_", c.c_id,
        " () { return c", c.c_id, "; }\n",
        "ATSinline() atstype_void cell_set_", c.c_id,
        " (", ty, " v) { c", c.c_id, " = v; }\n")
    in
      cells (cs)
    end
val () = cells (list0_reverse (!(ctx.x_cells)))
val () = cells (list0_reverse (!(ctx.x_states)))
//
fun bufs (bs: list0(buf)): void =
  case+ bs of
  | list0_nil () => ()
  | list0_cons (b, bs) => let
      val ty = (if b.b_bool then "atstype_bool" else "int"): string
      val () = fprint! (out,
        "static ", ty, " b", b.b_id, "_arr[", b.b_depth, "];\n",
        "static int b", b.b_id, "_r = 0;\n",
        "static int b", b.b_id, "_w = 0;\n",
        "ATSinline() ", ty, " buf_read_", b.b_id,
        " () { ", ty, " v = b", b.b_id, "_arr[b", b.b_id, "_r]; ",
        "b", b.b_id, "_r = (b", b.b_id, "_r + 1) % ", b.b_depth,
        "; return v; }\n",
        "ATSinline() atstype_void buf_write_", b.b_id,
        " (", ty, " v) { b", b.b_id, "_arr[b", b.b_id, "_w] = v; ",
        "b", b.b_id, "_w = (b", b.b_id, "_w + 1) % ", b.b_depth,
        "; }\n")
    in
      bufs (bs)
    end
val () = bufs (list0_reverse (!(ctx.x_bufs)))
//
val () = fprint! (out, "\n#endif\n")
//
in
  // nothing
end // end of [emit_cells_cats]

fn emit_cells_sats
(
  out: FILEref, ctx: ccctx, base: string
) : void = let
//
val () = fprint! (out,
  "(*\n** generated by overture --codegen=ats-bare (", base, ")\n",
  "** the state floor and stub surface; the %{# block here is\n",
  "** emitted before any client's prototypes, so the static\n",
  "** inline accessors declare first.\n*)\n\n")
val () = fprint! (out,
  "#include \"contrib/kernelats/prelude/staloadall.hats\"\n\n")
val () = fprint! (out,
  "%{#\n",
  "#include \"contrib/kernelats/ccomp/pats_ccomp.h\"\n",
  "#include \"", base, "_cells.cats\"\n",
  "#include \"prelude/CATS/integer.cats\"\n",
  "#include \"prelude/CATS/bool.cats\"\n",
  "%}\n\n")
//
//
// the state floor's accessor surface
//
fun celldecls (cs: list0(cell)): void =
  case+ cs of
  | list0_nil () => ()
  | list0_cons (c, cs) => let
      val ty = (if c.c_bool then "bool" else "int"): string
      val () = fprint! (out,
        "fun cell_get_", c.c_id, " (): ", ty,
        " = \"ext#\"\n",
        "fun cell_set_", c.c_id, " (v: ", ty, "): void",
        " = \"ext#\"\n")
    in
      celldecls (cs)
    end
val () = celldecls (list0_reverse (!(ctx.x_cells)))
val () = celldecls (list0_reverse (!(ctx.x_states)))
fun bufdecls (bs: list0(buf)): void =
  case+ bs of
  | list0_nil () => ()
  | list0_cons (b, bs) => let
      val ty = (if b.b_bool then "bool" else "int"): string
      val () = fprint! (out,
        "fun buf_read_", b.b_id, " (): ", ty,
        " = \"ext#\"\n",
        "fun buf_write_", b.b_id, " (v: ", ty, "): void",
        " = \"ext#\"\n")
    in
      bufdecls (bs)
    end
val () = bufdecls (list0_reverse (!(ctx.x_bufs)))
val () = fprint! (out, "\n")
//
// the program's stub surface
//
fn tystr2 (b: bool): string = if b then "bool" else "int"
fun senses (xs: list0(@(string, bool))): void =
  case+ xs of
  | list0_nil () => ()
  | list0_cons (x, xs) => let
      val () = fprint! (out,
        "fun ", x.0, "_sense (t: int): ", tystr2 (x.1),
        " = \"ext#", x.0, "_sense\"\n")
    in
      senses (xs)
    end
val () = senses (list0_reverse (!(ctx.x_senses)))
fun acts (xs: list0(@(string, bool))): void =
  case+ xs of
  | list0_nil () => ()
  | list0_cons (x, xs) => let
      val () = fprint! (out,
        "fun ", x.0, "_actuate (t: int, v: ", tystr2 (x.1),
        "): void = \"ext#", x.0, "_actuate\"\n")
    in
      acts (xs)
    end
val () = acts (list0_reverse (!(ctx.x_acts)))
fun calls
  (xs: list0(@(string, list0(bool), list0(bool)))): void =
  case+ xs of
  | list0_nil () => ()
  | list0_cons (x, xs) => let
      val () = fprint! (out, "fun ", x.0, " (")
      fun cargs (bs: list0(bool), i: int): void =
        case+ bs of
        | list0_nil () => ()
        | list0_cons (b, bs) => let
            val () = if i > 0 then fprint! (out, ", ")
            val () = fprint! (out, "a", i, ": ", tystr2 (b))
          in
            cargs (bs, i + 1)
          end
      val () = cargs (x.1, 0)
      val () = fprint! (out, "): ")
      val () = (
        case+ x.2 of
        | list0_cons (b, list0_nil ()) => fprint! (out, tystr2 (b))
        | _ (*rest*) => fprint! (out, "void") (* multi: rejected above *)
      ) : void
      val () = fprint! (out, " = \"ext#", x.0, "\"\n")
    in
      calls (xs)
    end
val () = calls (list0_reverse (!(ctx.x_calls)))
//
//
in
  // nothing
end // end of [emit_cells_sats]

fn emit_gen_atsbare
(
  out: FILEref, ctx: ccctx, base: string
) : void = let
//
val () = fprint! (out,
  "(*\n** generated by overture --codegen=ats-bare (", base, ")\n",
  "** freestanding ATS2 harness (kernelats); do not edit.\n",
  "** patsopt typechecking of this file proves the schedule\n",
  "** certificate: the artifact that flies is the certified one.\n*)\n\n")
val () = fprint! (out, "#define ATS_DYNLOADFLAG 0\n\n")
val () = fprint! (out,
  "#include \"contrib/kernelats/prelude/staloadall.hats\"\n\n")
val () = fprint! (out,
  "staload \"./", base, "_cells.sats\"\n",
  "staload \"./overture_sched_bare.sats\"\n\n",
  "staload _ = \"prelude/DATS/integer.dats\"\n",
  "staload _ = \"prelude/DATS/bool.dats\"\n\n")
val () = fprint! (out,
  "\nfn fire (t: int, n: int, d: int): bool =\n",
  "  if t >= d then (t - d) mod n = 0 else false\n\n")
//
// the step function
//
val () = fprint! (out,
  "extern fun overture_step (t: int): void = \"ext#overture_step\"\n\n",
  "implement overture_step (t) = let\n")
fun actions (acts: list0(action)): void =
  case+ acts of
  | list0_nil () => ()
  | list0_cons (a, acts) => let
      val () = emit_action_atsbare (out, a)
      val () = fprint! (out, "\n")
    in
      actions (acts)
    end
val () = actions (list0_reverse (!(ctx.x_pre)))
val () = actions (list0_reverse (!(ctx.x_strict)))
val () = actions (list0_reverse (!(ctx.x_post)))
val () = fprint! (out, "in () end\n\n")
//
// the schedule certificate as a function: its typechecking is the
// proof; the token it returns is unboxed and free to construct
//
val tm = ctx_timing (ctx)
val g = tm.0 and hp = tm.1 and md = tm.2
val tasks = list0_reverse (!(ctx.x_tasks))
val () = fprint! (out,
  "extern fun overture_the_schedule (): schedule",
  " = \"ext#overture_the_schedule\"\n\n",
  "implement overture_the_schedule () = let\n",
  "  val b = schedule_begin (", g, ", ", hp, ")\n")
fun fires2 (n: int, d: int, t: int): bool =
  if t >= d then ((t - d) mod n) = 0 else false
fun slotrows
  (ts: list0(@(string, int, int, int)), t: int, i: int
  ) : list0 (@(int, int, string)) =
(
  case+ ts of
  | list0_nil () => list0_nil ()
  | list0_cons (tk, ts) =>
      if fires2 (tk.1, tk.2, t)
        then list0_cons (@(i, tk.3, tk.0), slotrows (ts, t, i + 1))
        else slotrows (ts, t, i + 1)
      // end of [if]
) (* end of [slotrows] *)
fun slots2 (t: int, lim: int): void =
  if t < lim then let
    val rows = slotrows (tasks, t, 0)
    val () = (
      case+ rows of
      | list0_nil () => ()
      | list0_cons _ => let
          val () = fprint! (out,
            "  val s = slot_begin (", g, ") (* date ", t, " *)\n")
          fun add (rows: list0(@(int, int, string))): void =
            case+ rows of
            | list0_nil () => ()
            | list0_cons (r, rows) => let
                val () = fprint! (out,
                  "  val s = slot_add (s, ", r.0, ", ", r.1,
                  ") (* ", r.2, " *)\n")
              in
                add (rows)
              end
          val () = add (rows)
        in
          fprint! (out, "  val b = schedule_slot (b, ", t, ", s)\n")
        end
    ) : void
  in
    slots2 (t + g, lim)
  end // end of [slots2]
val () = slots2 (0, md + hp)
val () = fprint! (out, "in\n  schedule_end (b)\nend\n")
//
in
  // nothing
end // end of [emit_gen_atsbare]

fn emit_schedbare_sats
  (out: FILEref): void =
(
  fprint! (out,
"(*\n** generated by overture --codegen=ats-bare: the schedule\n** certificate, freestanding. The token is unboxed; the value of a\n** schedule is its typability.\n*)\n\n#include \"contrib/kernelats/prelude/staloadall.hats\"\n\n%{#\n#include \"contrib/kernelats/ccomp/pats_ccomp.h\"\n#include \"prelude/CATS/integer.cats\"\n%}\n\nabst@ype slotbuild (r: int) = int (* remaining capacity *)\nabst@ype schedbuild = int\nabst@ype schedule = int\n\nfun schedule_begin (base: int, hyper: int): schedbuild\nfun slot_begin {g:int | g > 0} (g: int (g)): slotbuild (g)\nfun slot_add {r,c:int | 0 <= c; c <= r}\n  (sl: slotbuild (r), tid: int, wcet: int (c)): slotbuild (r - c)\nfun schedule_slot {r:int | r >= 0}\n  (sb: schedbuild, date: int, sl: slotbuild (r)): schedbuild\nfun schedule_end (sb: schedbuild): schedule\n")
) (* end of [emit_schedbare_sats] *)

fn emit_schedbare_dats
  (out: FILEref): void =
(
  fprint! (out,
"(*\n** generated by overture --codegen=ats-bare: the schedule\n** certificate, freestanding.\n*)\n\n#define ATS_DYNLOADFLAG 0\n\n#include \"contrib/kernelats/prelude/staloadall.hats\"\n\n%{#\n#include \"contrib/kernelats/ccomp/pats_ccomp.h\"\n#include \"prelude/CATS/integer.cats\"\n%}\n\nstaload \"./overture_sched_bare.sats\"\n\nstaload _ = \"prelude/DATS/integer.dats\"\n\nassume slotbuild (r: int) = int\nassume schedbuild = int\nassume schedule = int\n\nimplement schedule_begin (b, h) = 0\nimplement slot_begin {g} (g) = 0\nimplement slot_add {r,c} (sl, tid, w) = sl + 1\nimplement schedule_slot {r} (sb, d, sl) = sb + sl\nimplement schedule_end (sb) = sb\n")
) (* end of [emit_schedbare_dats] *)

(* ****** ****** *)

implement
ccomp_program
  (base, prog, lang) = let
//
val ctx = ctx_new (prog.p2rog_nodes)
//
// sensors: one cell and one pre-phase sense each
//
fun sensors
  (d2vs: list0(d2var), env: fenv): fenv =
(
  case+ d2vs of
  | list0_nil () => env
  | list0_cons (d2v, d2vs) => let
      val name = symbol_get_name (d2var_get_sym (d2v))
      val c = (
        case+ d2var_get_type (d2v) of
        | T2YPErate (_, elt, clk) => let
            val nd = eval_clk (clk)
          in
            cell_new (ctx, t2ype_isbool (elt, list0_nil ()), nd.0, nd.1)
          end
        | _ (*rest*) => cell_new (ctx, false, 1, 0)
      ) : cell
      val () = emit_pre (ctx, ACTsense (name, c))
      val r = ctx.x_senses
      val () = !r := list0_cons (@(name, c.c_bool), !r)
    in
      sensors (d2vs,
        list0_cons (@(d2var_get_stamp (d2v), FVcell (c)), env))
    end
) (* end of [sensors] *)
//
val env = sensors (prog.p2rog_sensors, list0_nil ())
//
// top-level equations drive actuators
//
fun eqns
  (eqs: list0(e2qn)): void =
(
  case+ eqs of
  | list0_nil () => ()
  | list0_cons (eqn, eqs) => let
      val () = (
        case+ eqn.e2qn_lhs of
        | list0_cons (act, list0_nil ()) => let
            val name = symbol_get_name (d2var_get_sym (act))
            val fv = flatten1 (ctx, env, eqn.e2qn_rhs)
            val isb = (
              case+ fv of
              | FVgated (v, _) => (
                  emit_strict (ctx, ACTactuate_gated (name, v, fval_presence (fv)));
                  v.c_bool
                )
              | _ (*rest*) => let
                  val c = fval_cell (fv, "a top-level equation")
                  val () = emit_strict (ctx, ACTactuate (name, c))
                in
                  c.c_bool
                end
            ) : bool
            val r = ctx.x_acts
            val () = !r := list0_cons (@(name, isb), !r)
          in
            // nothing
          end
        | _ (*rest*) => ()
      ) : void
    in
      eqns (eqs)
    end
) (* end of [eqns] *)
//
val () = eqns (prog.p2rog_eqns)
//
val () = abort_if_errors ()
//
// write the two files
//
val isc = (lang = 1): bool
val isbare = (lang = 2): bool
//
val () =
  if isbare then let
    val cellspath = string_append (base, "_cells.cats")
    val () = (
      case+ fileref_open_opt (cellspath, file_mode_w) of
      | ~Some_vt (out) =>
          (emit_cells_cats (out, ctx, base); fileref_close (out))
      | ~None_vt () =>
          errmsg_noloc (str3 ("cannot open [", cellspath, "] for writing"))
    ) : void
    val () = (
      case+ fileref_open_opt ("overture_sched_bare.sats", file_mode_w) of
      | ~Some_vt (out) =>
          (emit_schedbare_sats (out); fileref_close (out))
      | ~None_vt () =>
          errmsg_noloc ("cannot open [overture_sched_bare.sats] for writing")
    ) : void
    val () = (
      case+ fileref_open_opt ("overture_sched_bare.dats", file_mode_w) of
      | ~Some_vt (out) =>
          (emit_schedbare_dats (out); fileref_close (out))
      | ~None_vt () =>
          errmsg_noloc ("cannot open [overture_sched_bare.dats] for writing")
    ) : void
    val cellsats = string_append (base, "_cells.sats")
    val () = (
      case+ fileref_open_opt (cellsats, file_mode_w) of
      | ~Some_vt (out) =>
          (emit_cells_sats (out, ctx, base); fileref_close (out))
      | ~None_vt () =>
          errmsg_noloc (str3 ("cannot open [", cellsats, "] for writing"))
    ) : void
    val barepath = string_append (base, "_bare.dats")
    val () = (
      case+ fileref_open_opt (barepath, file_mode_w) of
      | ~Some_vt (out) =>
          (emit_gen_atsbare (out, ctx, base); fileref_close (out))
      | ~None_vt () =>
          errmsg_noloc (str3 ("cannot open [", barepath, "] for writing"))
    ) : void
  in
    println! ("overture: wrote ", barepath, ", ", cellspath,
      ", overture_sched_bare.sats, overture_sched_bare.dats")
  end // end of [if]
//
val () =
  if (not (isc)) andalso (not (isbare)) then let
    val () = (
      case+ fileref_open_opt ("overture_sched.sats", file_mode_w) of
      | ~Some_vt (out) =>
          (emit_schedlib_sats (out); fileref_close (out))
      | ~None_vt () =>
          errmsg_noloc ("cannot open [overture_sched.sats] for writing")
    ) : void
    val () = (
      case+ fileref_open_opt ("overture_sched.dats", file_mode_w) of
      | ~Some_vt (out) =>
          (emit_schedlib_dats (out); fileref_close (out))
      | ~None_vt () =>
          errmsg_noloc ("cannot open [overture_sched.dats] for writing")
    ) : void
  in
    println! ("overture: wrote overture_sched.sats, overture_sched.dats")
  end // end of [if]
val genpath = string_append
  (base, if isc then "_gen.c" else "_gen.dats")
val stubpath = string_append
  (base, if isc then "_stubs.c" else "_stubs.dats")
//
val () = if isbare then () else (
  case+ fileref_open_opt (genpath, file_mode_w) of
  | ~Some_vt (out) => let
      val () =
        if isc then emit_gen_c (out, ctx, base)
        else emit_gen (out, ctx, base)
      val () = fileref_close (out)
    in
      println! ("overture: wrote ", genpath)
    end
  | ~None_vt () =>
      errmsg_noloc (str3 ("cannot open [", genpath, "] for writing"))
) : void // end of [val]
//
val () = if isbare then () else (
  case+ fileref_open_opt (stubpath, file_mode_w) of
  | ~Some_vt (out) => let
      val () =
        if isc then emit_stubs_c (out, ctx, base)
        else emit_stubs (out, ctx, base)
      val () = fileref_close (out)
    in
      println! ("overture: wrote ", stubpath)
    end
  | ~None_vt () =>
      errmsg_noloc (str3 ("cannot open [", stubpath, "] for writing"))
) : void // end of [val]
//
in
  abort_if_errors ()
end // end of [ccomp_program]

(* ****** ****** *)

(* end of [overture_ccomp.dats] *)
