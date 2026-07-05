(*
** Overture: A Synchronous Dataflow Language with Dependently Typed Clocks
**
** Driver. Stage flags compose, argparse-style: a single invocation
** may typecheck, dump intermediate forms, emit the dataflow graph,
** and generate code; stages run once, in pipeline order. With no
** stage selected, --typecheck is the default.
*)

#include "share/atspre_staload.hats"

staload "libats/ML/SATS/basis.sats"
staload "libats/ML/SATS/list0.sats"
staload "libats/ML/SATS/array0.sats"
staload "libats/ML/SATS/string.sats"
staload _ = "libats/ML/DATS/list0.dats"
staload _ = "libats/ML/DATS/array0.dats"
staload _ = "libats/ML/DATS/string.dats"

staload "./overture_symbol.sats"
staload "./overture_location.sats"
staload "./overture_error.sats"
staload "./overture_lexing.sats"
staload "./overture_fixity.sats"
staload "./overture_syntax.sats"
staload "./overture_parsing.sats"
staload "./overture_staexp.sats"
staload "./overture_trans12.sats"
staload "./overture_constraint.sats"
staload "./overture_trans3.sats"
staload "./overture_solver.sats"
staload "./overture_erasure.sats"
staload "./overture_ccomp.sats"

(* ****** ****** *)

dynload "overture_location.dats"
dynload "overture_symbol.dats"
dynload "overture_error.dats"
dynload "overture_lexing.dats"
dynload "overture_fixity.dats"
dynload "overture_syntax.dats"
dynload "overture_parsing.dats"
dynload "overture_staexp.dats"
dynload "overture_trans12.dats"
dynload "overture_constraint.dats"
dynload "overture_trans3.dats"
dynload "overture_solver.dats"
dynload "overture_erasure.dats"
dynload "overture_ccomp.dats"

(* ****** ****** *)

#define OVERTURE_VERSION "0.2.0"

fun print_version (): void =
  println! ("Overture ", OVERTURE_VERSION)

fun print_usage (cmd: string): void =
(
  println! ("usage: ", cmd, " [options] <file.ovt>");
  println! ("stages (any combination; run in pipeline order):");
  println! ("  --dump-tokens      print the token stream");
  println! ("  --dump-ast         print the parse tree after fixity resolution");
  println! ("  --dump-ast2        print the resolved, sort-checked program");
  println! ("  --typecheck        typecheck (the default when no stage is given)");
  println! ("  --emit-graph       print the clock-erased dataflow graph (JSON)");
  println! ("  --codegen[=c|ats|ats-bare]");
  println! ("                     generate a runnable harness (default: c;");
  println! ("                     ats-bare: freestanding ATS2 for bare metal)");
  println! ("other:");
  println! ("  --version          print the version");
  println! ("  --help, -h         this message")
)

(* ****** ****** *)

fun path_basename
  (path: string): string = let
  val arr = array0_make_list0<char> (string_explode (path))
  val n = array0_get_length (arr)
  fn at (i: int): char = array0_get_at_gint (arr, i)
  fun lastslash (i: int, r: int): int =
    if i >= n then r
    else if at (i) = '/'
      then lastslash (i + 1, i + 1) else lastslash (i + 1, r)
  val st = lastslash (0, 0)
  val hasext = (
    if n - st > 4 then
      (at (n - 4) = '.') andalso (at (n - 3) = 'o') andalso
      (at (n - 2) = 'v') andalso (at (n - 1) = 't')
    else false
  ) : bool
  val len = (if hasext then n - st - 4 else n - st): int
in
  string_make_substring
    (path, g0int2uint_int_size (st), g0int2uint_int_size (len))
end // end of [path_basename]

fun starts_dash
  (s: string): bool = let
  val arr = array0_make_list0<char> (string_explode (s))
in
  if array0_get_length (arr) > 0
    then array0_get_at_gint (arr, 0) = '-' else false
end // end of [starts_dash]

(* ****** ****** *)
//
// options; o_cg: ~1 none, 0 ats, 1 c
//
typedef cmdopts = '{
  o_dumptok= ref (bool)
, o_dumpast= ref (bool)
, o_dumpast2= ref (bool)
, o_tc= ref (bool)
, o_graph= ref (bool)
, o_cg= ref (int)
, o_version= ref (bool)
, o_help= ref (bool)
, o_bad= ref (bool)
, o_file= ref (option0 (string))
} (* end of [cmdopts] *)

fun parse_arg
  (opts: cmdopts, a: string): void =
(
case+ a of
| "--dump-tokens" => !(opts.o_dumptok) := true
| "--dump-ast" => !(opts.o_dumpast) := true
| "--dump-ast2" => !(opts.o_dumpast2) := true
| "--typecheck" => !(opts.o_tc) := true
| "--emit-graph" => !(opts.o_graph) := true
| "--codegen" => !(opts.o_cg) := 1
| "--codegen=c" => !(opts.o_cg) := 1
| "--codegen=ats" => !(opts.o_cg) := 0
| "--codegen=ats-bare" => !(opts.o_cg) := 2
| "--version" => !(opts.o_version) := true
| _ when (a = "--help") orelse (a = "-h") => !(opts.o_help) := true
| _ when starts_dash (a) => let
    val () = prerrln! ("overture: unknown option: ", a)
  in
    !(opts.o_bad) := true
  end
| _ (*input file*) => (
    case+ !(opts.o_file) of
    | None0 () => !(opts.o_file) := Some0 (a)
    | Some0 _ => let
        val () = prerrln! ("overture: more than one input file: ", a)
      in
        !(opts.o_bad) := true
      end
  )
) (* end of [parse_arg] *)

(* ****** ****** *)
//
// the pipeline: each stage runs at most once, as deep as requested
//
fun pipeline
  (opts: cmdopts, path: string): void = let
//
val () = fixity_initialize ()
val () = staexp_initialize ()
//
val toks = lexing_tokenize_file (path)
//
val () =
  if !(opts.o_dumptok) then let
    fun loop (toks: list0(token)): void =
      case+ toks of
      | list0_nil () => ()
      | list0_cons (tok, toks) => let
          val () = fprint_token (stdout_ref, tok)
          val () = fprint! (stdout_ref, "\n")
        in
          loop (toks)
        end
  in
    loop (toks)
  end // end of [if]
//
val cg = !(opts.o_cg)
val need_parse =
  !(opts.o_dumpast) orelse !(opts.o_dumpast2) orelse
  !(opts.o_tc) orelse !(opts.o_graph) orelse (cg >= 0)
//
in
//
if need_parse then let
  val decs = parsing_program (toks)
  val () =
    if !(opts.o_dumpast) then fprint_d0eclst (stdout_ref, decs)
  val need_resolve =
    !(opts.o_dumpast2) orelse !(opts.o_tc) orelse
    !(opts.o_graph) orelse (cg >= 0)
in
  if need_resolve then let
    val prog = trans12_program (decs)
    val () = abort_if_errors ()
    val () =
      if !(opts.o_dumpast2) then fprint_p2rog (stdout_ref, prog)
    val need_check =
      !(opts.o_tc) orelse !(opts.o_graph) orelse (cg >= 0)
  in
    if need_check then let
      val cs = trans3_program (prog)
      val () = abort_if_errors ()
      val () = solver_solve (cs)
      val () = abort_if_errors ()
      val () =
        if !(opts.o_tc) then println! ("typechecking succeeded")
      val () =
        if !(opts.o_graph) then erasure_emit (stdout_ref, prog)
      val () =
        if cg >= 0 then
          ccomp_program (path_basename (path), prog, cg)
        // end of [if]
    in
      // nothing
    end // end of [if need_check]
  end // end of [if need_resolve]
end // end of [if need_parse]
//
end // end of [pipeline]

(* ****** ****** *)

implement
main0 (argc, argv) = let
//
val opts = '{
  o_dumptok= ref<bool> (false)
, o_dumpast= ref<bool> (false)
, o_dumpast2= ref<bool> (false)
, o_tc= ref<bool> (false)
, o_graph= ref<bool> (false)
, o_cg= ref<int> (~1)
, o_version= ref<bool> (false)
, o_help= ref<bool> (false)
, o_bad= ref<bool> (false)
, o_file= ref<option0(string)> (None0 ())
} : cmdopts // end of [val]
//
fun loop
  {n:int}{i:nat | i <= n}
  (argc: int n, argv: !argv(n), i: int i, opts: cmdopts): void =
  if i < argc then let
    val () = parse_arg (opts, argv[i])
  in
    loop (argc, argv, i + 1, opts)
  end // end of [loop]
val () = loop (argc, argv, 1, opts)
//
val cmd = (if argc >= 1 then argv[0] else "overture"): string
//
in
//
case+ 0 of
| _ when !(opts.o_bad) => let
    val () = print_usage (cmd)
  in
    exit (1)
  end
| _ when !(opts.o_help) => print_usage (cmd)
| _ (*rest*) => let
    val () =
      if !(opts.o_version) then print_version ()
  in
    case+ !(opts.o_file) of
    | Some0 (path) => let
        (* default to typechecking when no stage was selected *)
        val anystage =
          !(opts.o_dumptok) orelse !(opts.o_dumpast) orelse
          !(opts.o_dumpast2) orelse !(opts.o_tc) orelse
          !(opts.o_graph) orelse (!(opts.o_cg) >= 0)
        val () =
          if not (anystage) then !(opts.o_tc) := true
      in
        pipeline (opts, path)
      end
    | None0 () =>
        if !(opts.o_version) then ()
        else let
          val () = print_usage (cmd)
        in
          exit (1)
        end
  end
//
end // end of [main0]

(* ****** ****** *)

(* end of [overture_main.dats] *)
