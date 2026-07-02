(*
** Overture: lexing
*)

#include "share/atspre_staload.hats"

staload UNSAFE = "prelude/SATS/unsafe.sats"

staload _ = "libats/libc/SATS/string.sats"

staload "libats/ML/SATS/basis.sats"
staload "libats/ML/SATS/list0.sats"
staload "libats/ML/SATS/array0.sats"
staload "libats/ML/SATS/string.sats"
staload "libats/ML/SATS/filebas.sats"

staload _ = "libats/ML/DATS/list0.dats"
staload _ = "libats/ML/DATS/array0.dats"
staload _ = "libats/ML/DATS/string.dats"
staload _ = "libats/ML/DATS/filebas.dats"

staload "./overture_symbol.sats"
staload "./overture_location.sats"
staload "./overture_error.sats"
staload "./overture_lexing.sats"

staload _ = "prelude/DATS/filebas.dats"

(* ****** ****** *)

typedef
lexbuf = '{
  lb_file= string
, lb_src= string
, lb_arr= array0(char)
, lb_len= int
, lb_pos= ref(int)
, lb_line= ref(int)
, lb_col= ref(int)
} (* end of [lexbuf] *)

(* ****** ****** *)

fn lexbuf_make
  (path: string, src: string): lexbuf = let
  val arr = array0_make_list0<char> (string_explode (src))
  val len = array0_get_length (arr)
in '{
  lb_file= path
, lb_src= src
, lb_arr= arr
, lb_len= len
, lb_pos= ref<int> (0)
, lb_line= ref<int> (1)
, lb_col= ref<int> (1)
} end // end of [lexbuf_make]

(* ****** ****** *)
//
// byte access: returns 0..255, or ~1 at/past the end
//
fn lexbuf_byte_at
  (buf: lexbuf, i: int): int =
(
  if i >= buf.lb_len
    then ~1
    else let
      val c = array0_get_at_gint (buf.lb_arr, i)
      val b = char2int0 (c)
    in
      if b < 0 then b + 256 else b
    end
  // end of [if]
) (* end of [lexbuf_byte_at] *)

fn lexbuf_peek
  (buf: lexbuf, k: int): int = let
  val p = buf.lb_pos
in
  lexbuf_byte_at (buf, !p + k)
end // end of [lexbuf_peek]

fn lexbuf_step
  (buf: lexbuf): void = let
  val p = buf.lb_pos
  and l = buf.lb_line
  and c = buf.lb_col
  val i = !p
  val b = lexbuf_byte_at (buf, i)
  val () = !p := i + 1
in
  if b = 10 (*newline*)
    then (!l := !l + 1; !c := 1)
    else !c := !c + 1
  // end of [if]
end // end of [lexbuf_step]

(* ****** ****** *)

fn lexbuf_getloc
(
  buf: lexbuf
, bline: int, bcol: int, bofs: int
) : location = let
  val l = buf.lb_line
  and c = buf.lb_col
  and p = buf.lb_pos
in
  location_make
    (buf.lb_file, bline, bcol, bofs, !l, !c, !p)
end // end of [lexbuf_getloc]

fn lexbuf_getloc_here
  (buf: lexbuf): location = let
  val l = buf.lb_line
  and c = buf.lb_col
  and p = buf.lb_pos
in
  lexbuf_getloc (buf, !l, !c, !p)
end // end of [lexbuf_getloc_here]

(* ****** ****** *)

fn byte_is_space (b: int): bool =
  (b = 32) orelse (b = 9) orelse (b = 10) orelse (b = 13)
fn byte_is_digit (b: int): bool =
  (b >= 48) andalso (b <= 57)
fn byte_is_identfst (b: int): bool =
  ((b >= 65) andalso (b <= 90)) orelse
  ((b >= 97) andalso (b <= 122)) orelse (b = 95(*'_'*))
fn byte_is_identrst (b: int): bool =
  byte_is_identfst (b) orelse byte_is_digit (b) orelse (b = 39(*'\''*))

(*
** the symbolic-operator character class:
** + - * / ^ < > = ! & | ~ %
*)
fn byte_is_symbolic (b: int): bool =
(
  case+ b of
  | 43 (*+*) => true | 45 (*-*) => true
  | 42 (***) => true | 47 (*/*) => true
  | 94 (*^*) => true
  | 60 (*<*) => true | 62 (*>*) => true
  | 61 (*=*) => true | 33 (*!*) => true
  | 38 (*&*) => true | 124 (*|*) => true
  | 126 (*~*) => true | 37 (*%*) => true
  | _ (*rest*) => false
) (* end of [byte_is_symbolic] *)

(* ****** ****** *)
//
// skipping whitespace and comments
//
fun lexbuf_skip
  (buf: lexbuf): void = let
  val b0 = lexbuf_peek (buf, 0)
in
//
if byte_is_space (b0) then
  (lexbuf_step (buf); lexbuf_skip (buf))
else if (b0 = 47(*'/'*)) andalso (lexbuf_peek (buf, 1) = 47) then let
  fun toeol (buf: lexbuf): void = let
    val b = lexbuf_peek (buf, 0)
  in
    if (b = ~1) orelse (b = 10)
      then () else (lexbuf_step (buf); toeol (buf))
    // end of [if]
  end // end of [toeol]
  val () = toeol (buf)
in
  lexbuf_skip (buf)
end
else if (b0 = 40(*'('*)) andalso (lexbuf_peek (buf, 1) = 42(*'*'*)) then let
  val loc0 = lexbuf_getloc_here (buf)
  val () = lexbuf_step (buf) and () = lexbuf_step (buf)
  fun incomment
    (buf: lexbuf, depth: int, loc0: location): void = let
    val b = lexbuf_peek (buf, 0)
  in
    case+ 0 of
    | _ when b = ~1 =>
        fatal_error (loc0, "unterminated block comment")
    | _ when (b = 42) andalso (lexbuf_peek (buf, 1) = 41(*')'*)) => let
        val () = lexbuf_step (buf) and () = lexbuf_step (buf)
      in
        if depth > 1 then incomment (buf, depth - 1, loc0)
      end
    | _ when (b = 40) andalso (lexbuf_peek (buf, 1) = 42) => let
        val () = lexbuf_step (buf) and () = lexbuf_step (buf)
      in
        incomment (buf, depth + 1, loc0)
      end
    | _ (*rest*) =>
        (lexbuf_step (buf); incomment (buf, depth, loc0))
  end // end of [incomment]
  val () = incomment (buf, 1, loc0)
in
  lexbuf_skip (buf)
end
else () // end of [if]
//
end // end of [lexbuf_skip]

(* ****** ****** *)

fn lexbuf_substr
  (buf: lexbuf, st: int, ln: int): string =
  string_make_substring
    (buf.lb_src, g0int2uint_int_size (st), g0int2uint_int_size (ln))
// end of [lexbuf_substr]

(* ****** ****** *)

fn ident_tnode
  (name: string): tnode =
(
  case+ name of
  | "sensor" => TOKkw_sensor ()
  | "actuator" => TOKkw_actuator ()
  | "node" => TOKkw_node ()
  | "extern" => TOKkw_extern ()
  | "returns" => TOKkw_returns ()
  | "var" => TOKkw_var ()
  | "let" => TOKkw_let ()
  | "tel" => TOKkw_tel ()
  | "infixl" => TOKkw_infixl ()
  | "infixr" => TOKkw_infixr ()
  | "prefix" => TOKkw_prefix ()
  | "wcet" => TOKkw_wcet ()
  | _ (*rest*) => TOKident (symbol_make (name))
) (* end of [ident_tnode] *)

fn oper_tnode
  (name: string): tnode =
(
  case+ name of
  | "=" => TOKeq ()
  | "|" => TOKbar ()
  | _ (*rest*) => TOKoper (symbol_make (name))
) (* end of [oper_tnode] *)

(* ****** ****** *)

fun lexbuf_get_token
  (buf: lexbuf): token = let
//
val () = lexbuf_skip (buf)
//
val bline = !(buf.lb_line)
val bcol = !(buf.lb_col)
val bofs = !(buf.lb_pos)
//
fn mktok
  (buf: lexbuf, node: tnode): token = '{
  tok_loc= lexbuf_getloc (buf, bline, bcol, bofs), tok_node= node
} (* end of [mktok] *)
//
fn mktok1
  (buf: lexbuf, node: tnode): token =
  (lexbuf_step (buf); mktok (buf, node))
//
val b0 = lexbuf_peek (buf, 0)
//
in
//
case+ 0 of
//
| _ when b0 = ~1 => mktok (buf, TOKeof ())
//
| _ when byte_is_digit (b0) => let
    fun loop (buf: lexbuf, v: int): int = let
      val b = lexbuf_peek (buf, 0)
    in
      if byte_is_digit (b)
        then (lexbuf_step (buf); loop (buf, 10 * v + (b - 48)))
        else v
      // end of [if]
    end // end of [loop]
    val v = loop (buf, 0)
  in
    mktok (buf, TOKint (v))
  end // end of [digit]
//
| _ when byte_is_identfst (b0) => let
    fun loop (buf: lexbuf): void = let
      val b = lexbuf_peek (buf, 0)
    in
      if byte_is_identrst (b)
        then (lexbuf_step (buf); loop (buf)) else ()
      // end of [if]
    end // end of [loop]
    val () = loop (buf)
    val name = lexbuf_substr (buf, bofs, !(buf.lb_pos) - bofs)
  in
    mktok (buf, ident_tnode (name))
  end // end of [ident]
//
| _ when b0 = 40 (*'('*) => mktok1 (buf, TOKlparen ())
| _ when b0 = 41 (*')'*) => mktok1 (buf, TOKrparen ())
| _ when b0 = 123 (*'{'*) => mktok1 (buf, TOKlbrace ())
| _ when b0 = 125 (*'}'*) => mktok1 (buf, TOKrbrace ())
| _ when b0 = 91 (*'['*) => mktok1 (buf, TOKlbracket ())
| _ when b0 = 93 (*']'*) => mktok1 (buf, TOKrbracket ())
| _ when b0 = 44 (*','*) => mktok1 (buf, TOKcomma ())
| _ when b0 = 59 (*';'*) => mktok1 (buf, TOKsemi ())
| _ when b0 = 58 (*':'*) => mktok1 (buf, TOKcolon ())
| _ when b0 = 46 (*'.'*) => mktok1 (buf, TOKdot ())
//
| _ when b0 = 226 (*0xE2*) => let
    val b1 = lexbuf_peek (buf, 1)
    val b2 = lexbuf_peek (buf, 2)
    fn mktok3
      (buf: lexbuf, node: tnode): token = let
      val () = lexbuf_step (buf)
      val () = lexbuf_step (buf)
      val () = lexbuf_step (buf)
    in
      mktok (buf, node)
    end // end of [mktok3]
  in
    if b1 = 136 (*0x88*) then (
      case+ 0 of
      | _ when b2 = 128 (*0x80*) => mktok3 (buf, TOKforall ())
      | _ when b2 = 131 (*0x83*) => mktok3 (buf, TOKexists ())
      | _ when b2 = 163 (*0xA3*) => mktok3 (buf, TOKbar ())
      | _ (*rest*) => let
          val () = fatal_error
            (lexbuf_getloc_here (buf), "unsupported non-ASCII character")
        in
          exit (1)
        end
    ) else let
      val () = fatal_error
        (lexbuf_getloc_here (buf), "unsupported non-ASCII character")
    in
      exit (1)
    end (* end of [if] *)
  end // end of [0xE2]
//
| _ when byte_is_symbolic (b0) => let
    fun loop (buf: lexbuf): void = let
      val b = lexbuf_peek (buf, 0)
    in
      if byte_is_symbolic (b)
        then (lexbuf_step (buf); loop (buf)) else ()
      // end of [if]
    end // end of [loop]
    val () = loop (buf)
    val name = lexbuf_substr (buf, bofs, !(buf.lb_pos) - bofs)
  in
    mktok (buf, oper_tnode (name))
  end // end of [symbolic]
//
| _ when b0 >= 128 => let
    val () = fatal_error
      (lexbuf_getloc_here (buf), "unsupported non-ASCII character")
  in
    exit (1)
  end
//
| _ (*rest*) => let
    val () = fatal_error
      (lexbuf_getloc_here (buf), "illegal character")
  in
    exit (1)
  end
//
end // end of [lexbuf_get_token]

(* ****** ****** *)

implement
lexing_tokenize_string
  (path, src) = let
//
val buf = lexbuf_make (path, src)
//
fun loop
  (buf: lexbuf): list0(token) = let
  val tok = lexbuf_get_token (buf)
in
  case+ tok.tok_node of
  | TOKeof () => list0_cons (tok, list0_nil ())
  | _ (*rest*) => list0_cons (tok, loop (buf))
end // end of [loop]
//
in
  loop (buf)
end // end of [lexing_tokenize_string]

(* ****** ****** *)

implement
lexing_tokenize_file
  (path) = let
  val opt = fileref_open_opt (path, file_mode_r)
in
//
case+ opt of
| ~Some_vt (inp) => let
    val str = fileref_get_file_string (inp)
    val () = fileref_close (inp)
    val src = $UNSAFE.castvwtp0{string} (str)
  in
    lexing_tokenize_string (path, src)
  end // end of [Some_vt]
| ~None_vt ((*void*)) => let
    val () = errmsg_noloc
      (string_append ("cannot open file: ", path))
    val () = abort_if_errors ()
  in
    list0_nil ()
  end // end of [None_vt]
//
end // end of [lexing_tokenize_file]

(* ****** ****** *)

implement
fprint_tnode
  (out, node) =
(
case+ node of
//
| TOKeof () => fprint! (out, "EOF")
//
| TOKint (i) => fprint! (out, "INT(", i, ")")
| TOKident (s) => fprint! (out, "IDENT(", symbol_get_name (s), ")")
| TOKoper (s) => fprint! (out, "OPER(", symbol_get_name (s), ")")
//
| TOKkw_sensor () => fprint! (out, "SENSOR")
| TOKkw_actuator () => fprint! (out, "ACTUATOR")
| TOKkw_node () => fprint! (out, "NODE")
| TOKkw_extern () => fprint! (out, "EXTERN")
| TOKkw_returns () => fprint! (out, "RETURNS")
| TOKkw_var () => fprint! (out, "VAR")
| TOKkw_let () => fprint! (out, "LET")
| TOKkw_tel () => fprint! (out, "TEL")
| TOKkw_infixl () => fprint! (out, "INFIXL")
| TOKkw_infixr () => fprint! (out, "INFIXR")
| TOKkw_prefix () => fprint! (out, "PREFIX")
| TOKkw_wcet () => fprint! (out, "WCET")
//
| TOKlparen () => fprint! (out, "LPAREN")
| TOKrparen () => fprint! (out, "RPAREN")
| TOKlbrace () => fprint! (out, "LBRACE")
| TOKrbrace () => fprint! (out, "RBRACE")
| TOKlbracket () => fprint! (out, "LBRACKET")
| TOKrbracket () => fprint! (out, "RBRACKET")
//
| TOKcomma () => fprint! (out, "COMMA")
| TOKsemi () => fprint! (out, "SEMI")
| TOKcolon () => fprint! (out, "COLON")
| TOKdot () => fprint! (out, "DOT")
//
| TOKbar () => fprint! (out, "BAR")
| TOKeq () => fprint! (out, "EQ")
//
| TOKforall () => fprint! (out, "FORALL")
| TOKexists () => fprint! (out, "EXISTS")
//
) (* end of [fprint_tnode] *)

implement
fprint_token
  (out, tok) = let
  val () = fprint_location (out, tok.tok_loc)
  val () = fprint! (out, ": ")
  val () = fprint_tnode (out, tok.tok_node)
in
  // nothing
end // end of [fprint_token]

(* ****** ****** *)

(* end of [overture_lexing.dats] *)
