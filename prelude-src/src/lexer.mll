(* ----------------------------------------------------------------------------
 * SchedMCore - A MultiCore Scheduling Framework
 * Copyright (C) 2009-2011, ONERA, Toulouse, FRANCE - LIFL, Lille, FRANCE
 *
 * This file is part of Prelude
 *
 * Prelude is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation ; either version 2 of
 * the License, or (at your option) any later version.
 *
 * Prelude is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY ; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program ; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
 * USA
 *---------------------------------------------------------------------------- *)

{
open Parser
open Utils

exception Error of Location.t

(* As advised by Caml documentation. This way a single lexer rule is
   used to handle all the possible keywords. *)
let keyword_table =
  create_hashtable 20 [
  "true", TRUE;
  "false", FALSE;
  "if", IF;
  "then", THEN;
  "else", ELSE;
  "merge", MERGE;
  "fby", FBY;
  "when", WHEN;
  "whennot", WHENNOT;
  "node", NODE;
  "sensor", SENSOR;
  "actuator", ACTUATOR;
  "let", LET;
  "tel", TEL;
  "returns", RETURNS;
  "var", VAR;
  "imported", IMPORTED;
  "wcet", WCET;
  "int", TINT;
  "bool", TBOOL;
  "float", TFLOAT;
  "real", TREAL;
  "clock", TCLOCK;
  "rate", RATE;
  "due", DUE;
  "not", NOT;
  "type", TYPE;
  "tail", TAIL
]

(* Update line number for location info *)
let incr_line lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  lexbuf.Lexing.lex_curr_p <- { pos with
    Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
    Lexing.pos_bol = pos.Lexing.pos_cnum;
  }
}

let newline = ('\010' | '\013' | "\013\010")
let blank = [' ' '\009' '\012']

rule token = parse
| "--" [^ '\n']* ['\n']
    { incr_line lexbuf;
      token lexbuf }
| "--" [^ '\n']* eof
    { token lexbuf }
| newline
    { incr_line lexbuf;
      token lexbuf }
| blank +
    {token lexbuf}
| ['0'-'9'] ['0'-'9']* '.' ['0'-'9']*
    {FLOAT (float_of_string (Lexing.lexeme lexbuf))}
| ['0'-'9']+ 
    {INT (int_of_string (Lexing.lexeme lexbuf)) }
| ['A'-'Z' 'a'-'z'] ['A'-'Z' 'a'-'z' '_' '0'-'9'] *
    {let s = Lexing.lexeme lexbuf in
    try
      Hashtbl.find keyword_table s
    with Not_found ->
      IDENT s}
| ['<' '>' '<' '>' '!'] ['=']?
    {INFIX0 (Lexing.lexeme lexbuf)}
| ['+' '-']
    {INFIX1 (Lexing.lexeme lexbuf)}
| '*'
    {INFIX2 (Lexing.lexeme lexbuf)}
| '=' {EQ}
| '(' {LPAR}
| ')' {RPAR}
| ';' {SCOL}
| ':' {COL}
| ',' {COMMA}
| '=' {EQ}
| '/' {DIV}
| "&&" {AMPERAMPER}
| "||" {BARBAR}
| "*^" {UCLOCK}
| "/^" {DCLOCK}
| "~>" {PHCLOCK}
| "::" {COLCOL}
| eof { EOF }
| _ { raise (Error (Location.curr lexbuf)) }
