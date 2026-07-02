(*
** Overture: parsing
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
staload "./overture_lexing.sats"
staload "./overture_fixity.sats"
staload "./overture_syntax.sats"
staload "./overture_parsing.sats"

(* ****** ****** *)

typedef pstate = '{ ps_toks= ref(list0(token)) }

(* ****** ****** *)

fn token_eof_dummy (): token =
  '{ tok_loc= location_dummy (), tok_node= TOKeof () }

fn pstate_peek
  (ps: pstate): token = let
  val toks = ps.ps_toks
in
  case+ !toks of
  | list0_cons (tok, _) => tok
  | list0_nil () => token_eof_dummy ()
end // end of [pstate_peek]

fn pstate_peek2
  (ps: pstate): token = let
  val toks = ps.ps_toks
in
  case+ !toks of
  | list0_cons (_, list0_cons (tok, _)) => tok
  | _ (*rest*) => token_eof_dummy ()
end // end of [pstate_peek2]

fn pstate_advance
  (ps: pstate): void = let
  val toks = ps.ps_toks
in
  case+ !toks of
  | list0_cons (_, rest) => !toks := rest
  | list0_nil () => ()
end // end of [pstate_advance]

fn pstate_next
  (ps: pstate): token = let
  val tok = pstate_peek (ps)
  val () = pstate_advance (ps)
in
  tok
end // end of [pstate_next]

(* ****** ****** *)

fn parse_fail
  (tok: token, msg: string): void = let
  val () = fprint! (stderr_ref, "  at token: ")
  val () = fprint_tnode (stderr_ref, tok.tok_node)
  val () = fprint! (stderr_ref, "\n")
in
  fatal_error (tok.tok_loc, msg)
end // end of [parse_fail]

(* ****** ****** *)

fn expect_lparen
  (ps: pstate, msg: string): void = let
  val tok = pstate_next (ps)
in
  case+ tok.tok_node of
  | TOKlparen () => () | _ => parse_fail (tok, msg)
end

fn expect_rparen
  (ps: pstate, msg: string): void = let
  val tok = pstate_next (ps)
in
  case+ tok.tok_node of
  | TOKrparen () => () | _ => parse_fail (tok, msg)
end

fn expect_colon
  (ps: pstate, msg: string): void = let
  val tok = pstate_next (ps)
in
  case+ tok.tok_node of
  | TOKcolon () => () | _ => parse_fail (tok, msg)
end

fn expect_semi
  (ps: pstate, msg: string): void = let
  val tok = pstate_next (ps)
in
  case+ tok.tok_node of
  | TOKsemi () => () | _ => parse_fail (tok, msg)
end

fn expect_eq
  (ps: pstate, msg: string): void = let
  val tok = pstate_next (ps)
in
  case+ tok.tok_node of
  | TOKeq () => () | _ => parse_fail (tok, msg)
end

fn expect_ident
  (ps: pstate, msg: string): i0de = let
  val tok = pstate_next (ps)
in
  case+ tok.tok_node of
  | TOKident (sym) => i0de_make (tok.tok_loc, sym)
  | _ (*rest*) => let
      val () = parse_fail (tok, msg) in exit (1)
    end
end // end of [expect_ident]

(* ****** ****** *)
//
// static expressions: item collection + precedence climbing
//
datatype s0item =
| S0ITMatm of s0exp
| S0ITMopr of (location, symbol, fxty)

(*
** an operator usable in expressions: a symbolic operator, the bar,
** or an identifier with a declared fixity (e.g. fby)
*)
fn token_operator
  (tok: token): option0 (@(symbol, fxty)) =
(
case+ tok.tok_node of
| TOKoper (sym) => (
    case+ the_fxtytbl_search (sym) of
    | Some0 (fxty) => Some0 (@(sym, fxty))
    | None0 () => None0 ()
  )
| TOKbar () => let
    val sym = symbol_make ("|")
  in
    case+ the_fxtytbl_search (sym) of
    | Some0 (fxty) => Some0 (@(sym, fxty))
    | None0 () => None0 ()
  end
| TOKident (sym) => (
    case+ the_fxtytbl_search (sym) of
    | Some0 (fxty) => Some0 (@(sym, fxty))
    | None0 () => None0 ()
  )
| _ (*rest*) => None0 ()
) (* end of [token_operator] *)

(* ****** ****** *)

extern fun parse_s0exp (ps: pstate): s0exp
extern fun parse_d0exp (ps: pstate): d0exp
extern fun parse_q0ua_brace (ps: pstate): q0ua (* after consuming '{' *)
extern fun parse_q0ua_forall (ps: pstate): q0ua (* after consuming forall *)

(* ****** ****** *)
//
// comma-separated static expressions until ')'
//
fun
parse_s0explst_rparen
  (ps: pstate): s0explst = let
  val s0e = parse_s0exp (ps)
  val tok = pstate_next (ps)
in
  case+ tok.tok_node of
  | TOKrparen () => list0_cons (s0e, list0_nil ())
  | TOKcomma () =>
      list0_cons (s0e, parse_s0explst_rparen (ps))
  | _ (*rest*) => let
      val () = parse_fail (tok, "expected ',' or ')'") in exit (1)
    end
end // end of [parse_s0explst_rparen]

fun
parse_s0atom
  (ps: pstate): s0exp = let
  val tok = pstate_next (ps)
in
//
case+ tok.tok_node of
//
| TOKint (i) =>
    s0exp_make (tok.tok_loc, S0Eint (i))
//
| TOKident (sym) => let
    val tok2 = pstate_peek (ps)
  in
    case+ tok2.tok_node of
    | TOKlparen () => let
        val () = pstate_advance (ps)
        val args = parse_s0explst_rparen (ps)
        val f = i0de_make (tok.tok_loc, sym)
      in
        s0exp_make (tok.tok_loc, S0Eapp (f, args))
      end
    | _ (*rest*) =>
        s0exp_make (tok.tok_loc, S0Eide (sym))
  end
//
| TOKlparen () => let
    val args = parse_s0explst_rparen (ps)
  in
    case+ args of
    | list0_cons (s0e, list0_nil ()) => s0e
    | _ (*rest*) => s0exp_make (tok.tok_loc, S0Etup (args))
  end
//
| _ (*rest*) => let
    val () = parse_fail (tok, "expected a static expression") in exit (1)
  end
//
end // end of [parse_s0atom]

(*
** does the current token start a static atom?
*)
fn s0atom_starts
  (ps: pstate): bool = let
  val tok = pstate_peek (ps)
in
  case+ tok.tok_node of
  | TOKint _ => true
  | TOKident _ => true
  | TOKlparen () => true
  | _ (*rest*) => false
end // end of [s0atom_starts]

fun
parse_s0items
  (ps: pstate, items: list0(s0item)): list0(s0item) = let
  val tok = pstate_peek (ps)
  val opr = token_operator (tok)
in
//
case+ opr of
| Some0 (sf) => let
    val () = pstate_advance (ps)
    val itm = S0ITMopr (tok.tok_loc, sf.0, sf.1)
  in
    parse_s0items (ps, list0_cons (itm, items))
  end
| None0 () =>
    if s0atom_starts (ps) then let
      val atm = parse_s0atom (ps)
      val itm = S0ITMatm (atm)
    in
      parse_s0items (ps, list0_cons (itm, items))
    end else list0_reverse (items)
  // end of [None0]
//
end // end of [parse_s0items]

(* ****** ****** *)
//
// precedence climbing over an item cursor
//
fun
s0items_climb
(
  cur: ref(list0(s0item)), minprec: int
) : s0exp = let
//
val lhs = (
  case+ !cur of
  | list0_cons (S0ITMatm (atm), rest) =>
      let val () = !cur := rest in atm end
  | list0_cons (S0ITMopr (loc, sym, _), _) => let
      val () = fatal_error (loc,
        string_append ("operator used where an operand is expected: ",
          symbol_get_name (sym)))
    in
      exit (1)
    end
  | list0_nil () => let
      val () = errmsg_noloc ("expected an operand")
      val () = abort_if_errors ()
    in
      exit (1)
    end
) : s0exp // end of [val]
//
fun loop
  (cur: ref(list0(s0item)), lhs: s0exp, minprec: int): s0exp =
(
  case+ !cur of
  | list0_cons (S0ITMopr (loc, sym, FXTYinfix (prec, assoc)), rest) =>
      if prec >= minprec then let
        val () = !cur := rest
        val nextmin = (
          case+ assoc of
          | ASSOCright () => prec
          | _ (*left/none*) => prec + 1
        ) : int
        val rhs = s0items_climb (cur, nextmin)
        val f = i0de_make (loc, sym)
        val loc12 = location_combine (lhs.s0e_loc, rhs.s0e_loc)
        val app = s0exp_make
          (loc12, S0Eopapp (f, list0_cons (lhs, list0_cons (rhs, list0_nil ()))))
      in
        loop (cur, app, minprec)
      end else lhs
  | _ (*rest*) => lhs
) (* end of [loop] *)
//
in
  loop (cur, lhs, minprec)
end // end of [s0items_climb]

implement
parse_s0exp (ps) = let
  val tok = pstate_peek (ps)
in
//
case+ tok.tok_node of
//
// existential quantifier: [qargs | guard] t  /  exists-form
//
| TOKlbracket () => let
    val () = pstate_advance (ps)
    val qua = parse_q0ua_brace (ps) (* shared: ends at '}' or ']' *)
    val body = parse_s0exp (ps)
    val loc = location_combine (tok.tok_loc, body.s0e_loc)
  in
    s0exp_make (loc, S0Eexi (qua, body))
  end
| TOKexists () => let
    val () = pstate_advance (ps)
    val qua = parse_q0ua_forall (ps) (* ends at '.' *)
    val body = parse_s0exp (ps)
    val loc = location_combine (tok.tok_loc, body.s0e_loc)
  in
    s0exp_make (loc, S0Eexi (qua, body))
  end
//
| _ (*rest*) => let
    val items = parse_s0items (ps, list0_nil ())
    val cur = ref<list0(s0item)> (items)
    val res = s0items_climb (cur, 0)
    val () = (
      case+ !cur of
      | list0_nil () => ()
      | list0_cons (S0ITMopr (loc, sym, _), _) =>
          fatal_error (loc,
            string_append ("dangling operator: ", symbol_get_name (sym)))
      | list0_cons (S0ITMatm (atm), _) =>
          fatal_error (atm.s0e_loc, "expected an operator between operands")
    ) : void // end of [val]
  in
    res
  end
//
end // end of [parse_s0exp]

(* ****** ****** *)
//
// quantifiers
//
// qargs ::= ID (':' ID)? ((';'|',') qargs)?
//
fun
parse_q0args
  (ps: pstate): list0(q0arg) = let
//
val ide = expect_ident (ps, "expected a static variable name")
//
val tok = pstate_peek (ps)
val srt = (
  case+ tok.tok_node of
  | TOKcolon () => let
      val () = pstate_advance (ps)
      val srtide = expect_ident (ps, "expected a sort name")
    in
      Some0 (S0RTide (srtide.i0de_loc, srtide.i0de_sym))
    end
  | _ (*rest*) => None0 ()
) : option0 (s0rt) // end of [val]
//
val arg = q0arg_make (ide.i0de_loc, ide.i0de_sym, srt)
//
val tok = pstate_peek (ps)
//
in
//
case+ tok.tok_node of
| TOKsemi () => let
    val () = pstate_advance (ps)
  in
    list0_cons (arg, parse_q0args (ps))
  end
| TOKcomma () => let
    val () = pstate_advance (ps)
  in
    list0_cons (arg, parse_q0args (ps))
  end
| _ (*rest*) => list0_cons (arg, list0_nil ())
//
end // end of [parse_q0args]

(*
** after '{' or '[': qargs (BAR guard)? ('}' or ']')
*)
implement
parse_q0ua_brace
  (ps) = let
//
val tok0 = pstate_peek (ps)
val args = parse_q0args (ps)
//
val tok = pstate_next (ps)
//
in
//
case+ tok.tok_node of
| TOKrbrace () =>
    q0ua_make (tok0.tok_loc, args, None0 ())
| TOKrbracket () =>
    q0ua_make (tok0.tok_loc, args, None0 ())
| TOKbar () => let
    val gua = parse_s0exp (ps)
    val tok = pstate_next (ps)
  in
    case+ tok.tok_node of
    | TOKrbrace () =>
        q0ua_make (tok0.tok_loc, args, Some0 (gua))
    | TOKrbracket () =>
        q0ua_make (tok0.tok_loc, args, Some0 (gua))
    | _ (*rest*) => let
        val () = parse_fail (tok, "expected '}' or ']'") in exit (1)
      end
  end
| _ (*rest*) => let
    val () = parse_fail
      (tok, "expected '}', ']', or a guard separator") in exit (1)
  end
//
end // end of [parse_q0ua_brace]

(*
** after forall/exists: qargs (BAR guard)? '.'
**
** note: the guard itself may contain bars (divides); the guard ends
** at the '.' terminator, and the FIRST bar after the qargs is the
** separator. Inside the guard, bars resolve as the divides operator.
*)
implement
parse_q0ua_forall
  (ps) = let
//
val tok0 = pstate_peek (ps)
val args = parse_q0args (ps)
//
val tok = pstate_next (ps)
//
in
//
case+ tok.tok_node of
| TOKdot () =>
    q0ua_make (tok0.tok_loc, args, None0 ())
| TOKbar () => let
    val gua = parse_s0exp (ps)
    val tok = pstate_next (ps)
  in
    case+ tok.tok_node of
    | TOKdot () =>
        q0ua_make (tok0.tok_loc, args, Some0 (gua))
    | _ (*rest*) => let
        val () = parse_fail (tok, "expected '.'") in exit (1)
      end
  end
| _ (*rest*) => let
    val () = parse_fail
      (tok, "expected '.' or a guard separator") in exit (1)
  end
//
end // end of [parse_q0ua_forall]

(* ****** ****** *)
//
// dynamic expressions
//
datatype d0item =
| D0ITMatm of d0exp
| D0ITMopr of (location, symbol, fxty)

fun
parse_d0explst_rparen
  (ps: pstate): d0explst = let
  val d0e = parse_d0exp (ps)
  val tok = pstate_next (ps)
in
  case+ tok.tok_node of
  | TOKrparen () => list0_cons (d0e, list0_nil ())
  | TOKcomma () =>
      list0_cons (d0e, parse_d0explst_rparen (ps))
  | _ (*rest*) => let
      val () = parse_fail (tok, "expected ',' or ')'") in exit (1)
    end
end // end of [parse_d0explst_rparen]

fun
parse_d0atom
  (ps: pstate): d0exp = let
  val tok = pstate_next (ps)
in
//
case+ tok.tok_node of
//
| TOKint (i) =>
    d0exp_make (tok.tok_loc, D0Eint (i))
//
| TOKident (sym) => let
    val tok2 = pstate_peek (ps)
  in
    case+ tok2.tok_node of
    | TOKlparen () => let
        val () = pstate_advance (ps)
        val f = i0de_make (tok.tok_loc, sym)
        val tok3 = pstate_peek (ps)
      in
        case+ tok3.tok_node of
        | TOKrparen () => let
            val () = pstate_advance (ps)
          in
            d0exp_make (tok.tok_loc, D0Eapp (f, list0_nil ()))
          end
        | _ (*rest*) => let
            val args = parse_d0explst_rparen (ps)
          in
            d0exp_make (tok.tok_loc, D0Eapp (f, args))
          end
      end
    | _ (*rest*) =>
        d0exp_make (tok.tok_loc, D0Eide (sym))
  end
//
| TOKlparen () => let
    val args = parse_d0explst_rparen (ps)
  in
    case+ args of
    | list0_cons (d0e, list0_nil ()) => d0e
    | _ (*rest*) => d0exp_make (tok.tok_loc, D0Etup (args))
  end
//
| _ (*rest*) => let
    val () = parse_fail (tok, "expected an expression") in exit (1)
  end
//
end // end of [parse_d0atom]

fn d0atom_starts
  (ps: pstate): bool = let
  val tok = pstate_peek (ps)
in
  case+ tok.tok_node of
  | TOKint _ => true
  | TOKident _ => true
  | TOKlparen () => true
  | _ (*rest*) => false
end // end of [d0atom_starts]

fun
parse_d0items
  (ps: pstate, items: list0(d0item)): list0(d0item) = let
  val tok = pstate_peek (ps)
  val opr = token_operator (tok)
in
//
case+ opr of
| Some0 (sf) => let
    val () = pstate_advance (ps)
    val itm = D0ITMopr (tok.tok_loc, sf.0, sf.1)
  in
    parse_d0items (ps, list0_cons (itm, items))
  end
| None0 () =>
    if d0atom_starts (ps) then let
      val atm = parse_d0atom (ps)
      val itm = D0ITMatm (atm)
    in
      parse_d0items (ps, list0_cons (itm, items))
    end else list0_reverse (items)
  // end of [None0]
//
end // end of [parse_d0items]

fun
d0items_climb
(
  cur: ref(list0(d0item)), minprec: int
) : d0exp = let
//
val lhs = (
  case+ !cur of
  | list0_cons (D0ITMatm (atm), rest) =>
      let val () = !cur := rest in atm end
  | list0_cons (D0ITMopr (loc, sym, _), _) => let
      val () = fatal_error (loc,
        string_append ("operator used where an operand is expected: ",
          symbol_get_name (sym)))
    in
      exit (1)
    end
  | list0_nil () => let
      val () = errmsg_noloc ("expected an operand")
      val () = abort_if_errors ()
    in
      exit (1)
    end
) : d0exp // end of [val]
//
fun loop
  (cur: ref(list0(d0item)), lhs: d0exp, minprec: int): d0exp =
(
  case+ !cur of
  | list0_cons (D0ITMopr (loc, sym, FXTYinfix (prec, assoc)), rest) =>
      if prec >= minprec then let
        val () = !cur := rest
        val nextmin = (
          case+ assoc of
          | ASSOCright () => prec
          | _ (*left/none*) => prec + 1
        ) : int
        val rhs = d0items_climb (cur, nextmin)
        val f = i0de_make (loc, sym)
        val loc12 = location_combine (lhs.d0e_loc, rhs.d0e_loc)
        val app = d0exp_make
          (loc12, D0Eopapp (f, list0_cons (lhs, list0_cons (rhs, list0_nil ()))))
      in
        loop (cur, app, minprec)
      end else lhs
  | _ (*rest*) => lhs
) (* end of [loop] *)
//
in
  loop (cur, lhs, minprec)
end // end of [d0items_climb]

implement
parse_d0exp (ps) = let
  val items = parse_d0items (ps, list0_nil ())
  val cur = ref<list0(d0item)> (items)
  val res = d0items_climb (cur, 0)
  val () = (
    case+ !cur of
    | list0_nil () => ()
    | list0_cons (D0ITMopr (loc, sym, _), _) =>
        fatal_error (loc,
          string_append ("dangling operator: ", symbol_get_name (sym)))
    | list0_cons (D0ITMatm (atm), _) =>
        fatal_error (atm.d0e_loc, "expected an operator between operands")
  ) : void // end of [val]
in
  res
end // end of [parse_d0exp]

(* ****** ****** *)
//
// declarations
//
fun
parse_p0aramlst
  (ps: pstate): list0(p0aram) = let
//
// params ::= eps | param (',' param)*  -- up to and including ')'
//
val tok = pstate_peek (ps)
//
in
//
case+ tok.tok_node of
| TOKrparen () =>
    let val () = pstate_advance (ps) in list0_nil () end
| _ (*rest*) => let
    fun loop (ps: pstate): list0(p0aram) = let
      val ide = expect_ident (ps, "expected a parameter name")
      val () = expect_colon (ps, "expected ':' after parameter name")
      val typ = parse_s0exp (ps)
      val par = '{
        p0aram_loc= ide.i0de_loc
      , p0aram_sym= ide.i0de_sym
      , p0aram_typ= typ
      } : p0aram
      val tok = pstate_next (ps)
    in
      case+ tok.tok_node of
      | TOKrparen () => list0_cons (par, list0_nil ())
      | TOKcomma () => list0_cons (par, loop (ps))
      | _ (*rest*) => let
          val () = parse_fail (tok, "expected ',' or ')'") in exit (1)
        end
    end // end of [loop]
  in
    loop (ps)
  end
//
end // end of [parse_p0aramlst]

(* ****** ****** *)

fun
parse_e0qn
  (ps: pstate): e0qn = let
//
val tok = pstate_peek (ps)
//
val lhs = (
  case+ tok.tok_node of
  | TOKident (sym) => let
      val () = pstate_advance (ps)
    in
      list0_cons (i0de_make (tok.tok_loc, sym), list0_nil ())
    end
  | TOKlparen () => let
      val () = pstate_advance (ps)
      fun loop (ps: pstate): list0(i0de) = let
        val ide = expect_ident (ps, "expected a flow name")
        val tok = pstate_next (ps)
      in
        case+ tok.tok_node of
        | TOKrparen () => list0_cons (ide, list0_nil ())
        | TOKcomma () => list0_cons (ide, loop (ps))
        | _ (*rest*) => let
            val () = parse_fail (tok, "expected ',' or ')'") in exit (1)
          end
      end // end of [loop]
    in
      loop (ps)
    end
  | _ (*rest*) => let
      val () = parse_fail
        (tok, "expected an equation left-hand side") in exit (1)
    end
) : list0 (i0de) // end of [val]
//
val () = expect_eq (ps, "expected '=' in equation")
val rhs = parse_d0exp (ps)
val () = expect_semi (ps, "expected ';' after equation")
//
in '{
  e0qn_loc= location_combine (tok.tok_loc, rhs.d0e_loc)
, e0qn_lhs= lhs
, e0qn_rhs= rhs
} end // end of [parse_e0qn]

(* ****** ****** *)

fun
parse_node
  (ps: pstate, isext: bool, loc0: location): d0ec = let
//
// quantifier groups may appear on both sides of the name; the
// position is stylistic (template-flavored groups on the left, DML
// statics on the right) -- variables are classified by sort
//
fun prequas
  (ps: pstate): list0 (q0ua) = let
  val tok = pstate_peek (ps)
in
  case+ tok.tok_node of
  | TOKlbrace () => let
      val () = pstate_advance (ps)
      val qua = parse_q0ua_brace (ps)
    in
      list0_cons (qua, prequas (ps))
    end
  | _ (*rest*) => list0_nil ()
end // end of [prequas]
//
fun postquas
  (ps: pstate): list0 (q0ua) = let
  val tok = pstate_peek (ps)
in
  case+ tok.tok_node of
  | TOKlbrace () => let
      val () = pstate_advance (ps)
      val qua = parse_q0ua_brace (ps)
    in
      list0_cons (qua, postquas (ps))
    end
  | TOKforall () => let
      val () = pstate_advance (ps)
      val qua = parse_q0ua_forall (ps)
    in
      list0_cons (qua, postquas (ps))
    end
  | _ (*rest*) => list0_nil ()
end // end of [postquas]
//
val prequa = prequas (ps)
val name = expect_ident (ps, "expected a node name")
val postqua = postquas (ps)
//
val () = expect_lparen (ps, "expected '(' to begin node parameters")
val params = parse_p0aramlst (ps)
//
val tok = pstate_next (ps)
val () = (
  case+ tok.tok_node of
  | TOKkw_returns () => ()
  | _ (*rest*) => parse_fail (tok, "expected 'returns'")
) : void // end of [val]
//
val () = expect_lparen (ps, "expected '(' to begin node returns")
val returns = parse_p0aramlst (ps)
//
// optional wcet clause: a static integer expression
//
val tok = pstate_peek (ps)
val wcet = (
  case+ tok.tok_node of
  | TOKkw_wcet () => let
      val () = pstate_advance (ps)
    in
      Some0 (parse_s0exp (ps))
    end
  | _ (*rest*) => None0 ()
) : option0 (s0exp) // end of [val]
//
in
//
if isext then let
  val () = expect_semi (ps, "expected ';' after extern node declaration")
in
  d0ec_make (loc0, D0Cnode ('{
    n0de_loc= loc0
  , n0de_extern= true
  , n0de_name= name
  , n0de_prequa= prequa
  , n0de_postqua= postqua
  , n0de_params= params
  , n0de_returns= returns
  , n0de_wcet= wcet
  , n0de_vars= list0_nil ()
  , n0de_eqns= None0 ()
  }))
end else let
//
// var declarations
//
  fun vars (ps: pstate): list0(p0aram) = let
    val tok = pstate_peek (ps)
  in
    case+ tok.tok_node of
    | TOKkw_var () => let
        val () = pstate_advance (ps)
        val ide = expect_ident (ps, "expected a var name")
        val () = expect_colon (ps, "expected ':' after var name")
        val typ = parse_s0exp (ps)
        val () = expect_semi (ps, "expected ';' after var declaration")
        val par = '{
          p0aram_loc= ide.i0de_loc
        , p0aram_sym= ide.i0de_sym
        , p0aram_typ= typ
        } : p0aram
      in
        list0_cons (par, vars (ps))
      end
    | _ (*rest*) => list0_nil ()
  end // end of [vars]
//
  val vardecls = vars (ps)
//
  val tok = pstate_next (ps)
  val () = (
    case+ tok.tok_node of
    | TOKkw_let () => ()
    | _ (*rest*) => parse_fail (tok, "expected 'let'")
  ) : void // end of [val]
//
  fun eqns (ps: pstate): list0(e0qn) = let
    val tok = pstate_peek (ps)
  in
    case+ tok.tok_node of
    | TOKkw_tel () =>
        let val () = pstate_advance (ps) in list0_nil () end
    | _ (*rest*) =>
        list0_cons (parse_e0qn (ps), eqns (ps))
  end // end of [eqns]
//
  val body = eqns (ps)
//
in
  d0ec_make (loc0, D0Cnode ('{
    n0de_loc= loc0
  , n0de_extern= false
  , n0de_name= name
  , n0de_prequa= prequa
  , n0de_postqua= postqua
  , n0de_params= params
  , n0de_returns= returns
  , n0de_wcet= wcet
  , n0de_vars= vardecls
  , n0de_eqns= Some0 (body)
  }))
end // end of [if]
//
end // end of [parse_node]

(* ****** ****** *)

fun
parse_fixity
  (ps: pstate, knd: fixkind, loc0: location): d0ec = let
//
val tok = pstate_next (ps)
val prec = (
  case+ tok.tok_node of
  | TOKint (i) => i
  | _ (*rest*) => let
      val () = parse_fail
        (tok, "expected a precedence integer") in exit (1)
    end
) : int // end of [val]
//
fun opers
  (ps: pstate): list0(i0de) = let
  val tok = pstate_peek (ps)
in
  case+ tok.tok_node of
  | TOKoper (sym) => let
      val () = pstate_advance (ps)
      val ide = i0de_make (tok.tok_loc, sym)
    in
      list0_cons (ide, opers (ps))
    end
  | TOKident (sym) => let
      val () = pstate_advance (ps)
      val ide = i0de_make (tok.tok_loc, sym)
    in
      list0_cons (ide, opers (ps))
    end
  | _ (*rest*) => list0_nil ()
end // end of [opers]
//
val ides = opers (ps)
val () = (
  case+ ides of
  | list0_nil () =>
      parse_fail (pstate_peek (ps), "expected at least one operator")
  | list0_cons _ => ()
) : void // end of [val]
val () = expect_semi (ps, "expected ';' after fixity declaration")
//
// take effect immediately
//
val fxty = (
  case+ knd of
  | FIXleft () => FXTYinfix (prec, ASSOCleft ())
  | FIXright () => FXTYinfix (prec, ASSOCright ())
  | FIXpre () => FXTYprefix (prec)
) : fxty // end of [val]
//
fun register
  (ides: list0(i0de), fxty: fxty): void =
(
  case+ ides of
  | list0_nil () => ()
  | list0_cons (ide, ides) => let
      val () = the_fxtytbl_insert (ide.i0de_sym, fxty)
    in
      register (ides, fxty)
    end
) (* end of [register] *)
val () = register (ides, fxty)
//
in
  d0ec_make (loc0, D0Cfixity (knd, prec, ides))
end // end of [parse_fixity]

(* ****** ****** *)

fun
parse_d0ec
  (ps: pstate): d0ec = let
  val tok = pstate_next (ps)
  val loc0 = tok.tok_loc
in
//
case+ tok.tok_node of
//
| TOKkw_sensor () => let
    val ide = expect_ident (ps, "expected a sensor name")
    val () = expect_colon (ps, "expected ':' after sensor name")
    val typ = parse_s0exp (ps)
    val () = expect_semi (ps, "expected ';' after sensor declaration")
  in
    d0ec_make (loc0, D0Csensor (ide, typ))
  end
//
| TOKkw_actuator () => let
    val ide = expect_ident (ps, "expected an actuator name")
    val () = expect_colon (ps, "expected ':' after actuator name")
    val typ = parse_s0exp (ps)
    val () = expect_semi (ps, "expected ';' after actuator declaration")
  in
    d0ec_make (loc0, D0Cactuator (ide, typ))
  end
//
| TOKkw_infixl () => parse_fixity (ps, FIXleft (), loc0)
| TOKkw_infixr () => parse_fixity (ps, FIXright (), loc0)
| TOKkw_prefix () => parse_fixity (ps, FIXpre (), loc0)
//
| TOKkw_extern () => let
    val tok = pstate_next (ps)
  in
    case+ tok.tok_node of
    | TOKkw_node () => parse_node (ps, true, loc0)
    | _ (*rest*) => let
        val () = parse_fail (tok, "expected 'node' after 'extern'")
      in
        exit (1)
      end
  end
//
| TOKkw_node () => parse_node (ps, false, loc0)
//
| TOKident (sym) => let
    (* top-level equation: ID = d0exp ; *)
    val () = expect_eq (ps, "expected '=' in top-level equation")
    val rhs = parse_d0exp (ps)
    val () = expect_semi (ps, "expected ';' after equation")
    val lhs = list0_cons (i0de_make (loc0, sym), list0_nil ())
    val eqn = '{
      e0qn_loc= location_combine (loc0, rhs.d0e_loc)
    , e0qn_lhs= lhs
    , e0qn_rhs= rhs
    } : e0qn
  in
    d0ec_make (loc0, D0Ceqn (eqn))
  end
//
| _ (*rest*) => let
    val () = parse_fail (tok, "expected a declaration") in exit (1)
  end
//
end // end of [parse_d0ec]

(* ****** ****** *)

implement
parsing_program
  (toks) = let
//
val ps = '{ ps_toks= ref<list0(token)> (toks) } : pstate
//
fun loop (ps: pstate): d0eclst = let
  val tok = pstate_peek (ps)
in
  case+ tok.tok_node of
  | TOKeof () => list0_nil ()
  | _ (*rest*) =>
      list0_cons (parse_d0ec (ps), loop (ps))
end // end of [loop]
//
in
  loop (ps)
end // end of [parsing_program]

implement
parsing_program_file
  (path) =
  parsing_program (lexing_tokenize_file (path))
// end of [parsing_program_file]

(* ****** ****** *)

(* end of [overture_parsing.dats] *)
