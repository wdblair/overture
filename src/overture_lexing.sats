(*
** Overture: lexing
**
** Byte-oriented lexer in the style of pats_lexing.sats. The only
** multibyte sequences recognized are the UTF-8 quantifier characters:
**
**   U+2200 FOR ALL     (E2 88 80) -> TOKforall
**   U+2203 THERE EXISTS(E2 88 83) -> TOKexists
**   U+2223 DIVIDES     (E2 88 A3) -> TOKbar
**
** Symbolic operators are lexed by maximal munch over the character
** class [+ - * / ^ < > = ! & | ~ %]; fixity is resolved later. The
** runs "=" and "|" are given their own token constructors since they
** double as structural syntax (equation sign, guard separator).
**
** Comments: // to end of line, and nested block comments delimited
** by lparen-star and star-rparen, as in ML. Note that lparen-star
** always opens a comment; since no expression may begin with a star,
** this steals nothing from the operator grammar.
*)

staload "libats/ML/SATS/basis.sats"
staload "libats/ML/SATS/list0.sats"

staload "./overture_symbol.sats"
staload "./overture_location.sats"

(* ****** ****** *)

datatype tnode =
//
| TOKeof of ()
//
| TOKint of int
| TOKident of symbol (* alphanumeric identifier *)
| TOKoper of symbol  (* symbolic operator: */ ^/ + == <= ... *)
//
| TOKkw_sensor of ()
| TOKkw_actuator of ()
| TOKkw_node of ()
| TOKkw_extern of ()
| TOKkw_returns of ()
| TOKkw_var of ()
| TOKkw_let of ()
| TOKkw_tel of ()
| TOKkw_infixl of ()
| TOKkw_infixr of ()
| TOKkw_prefix of ()
| TOKkw_wcet of ()
| TOKkw_abstype of ()
| TOKkw_typedef of ()
//
| TOKlparen of () | TOKrparen of ()
| TOKlbrace of () | TOKrbrace of ()
| TOKlbracket of () | TOKrbracket of ()
//
| TOKcomma of () | TOKsemi of ()
| TOKcolon of () | TOKdot of ()
//
| TOKbar of ()    (* '|' or U+2223: guard separator / divides *)
| TOKeq of ()     (* '=': equation sign *)
//
| TOKforall of () (* U+2200 *)
| TOKexists of () (* U+2203 *)
//
(* end of [tnode] *)

typedef token = '{ tok_loc= location, tok_node= tnode }

(* ****** ****** *)

fun fprint_tnode (out: FILEref, node: tnode): void
fun fprint_token (out: FILEref, tok: token): void

(* ****** ****** *)

(*
** tokenize a whole source file; the returned list always ends with a
** TOKeof token; lexical errors are fatal
*)
fun lexing_tokenize_file (path: string): list0(token)

fun lexing_tokenize_string
  (path: string, src: string): list0(token)
// end of [lexing_tokenize_string]

(* ****** ****** *)

(* end of [overture_lexing.sats] *)
