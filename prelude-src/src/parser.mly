/* ----------------------------------------------------------------------------
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
 *---------------------------------------------------------------------------- */

%{
open Corelang
open Utils
let type_table =
  create_hashtable 20 [
    "int",Tydec_int;
    "bool",Tydec_bool;
    "float",Tydec_float;
    "real",Tydec_float
  ]
let mktyp d =
  { ty_dec_desc = d; ty_dec_loc = Location.symbol_rloc () }
let mkclock d =
  { ck_dec_desc = d; ck_dec_loc = Location.symbol_rloc () }
let mkvar_decl (id, ty_dec, ck_dec, dd_dec) =
  { var_id= id;
    var_dec_type = ty_dec;
    var_dec_clock = ck_dec;
    var_dec_deadline = dd_dec;
    var_type = Types.new_var ();
    var_clock = Clocks.new_var true;
    var_loc = Location.symbol_rloc () }
let mkexpr d =
  { expr_tag = new_tag ();
    expr_desc = d;
    expr_type = Types.new_var ();
    expr_clock = Clocks.new_var true;
    expr_loc = Location.symbol_rloc () }
let mkeq (lhs, rhs) =
  { eq_lhs = lhs;
    eq_rhs = rhs;
    eq_loc = Location.symbol_rloc () }
let mktop_decl d =
  { top_decl_desc = d; top_decl_loc = Location.symbol_rloc () }
%}

%token <int> INT
%token <float> FLOAT
%token TRUE FALSE
%token <string> IDENT
%token LPAR RPAR SCOL COL COMMA COLCOL
%token AMPERAMPER BARBAR NOT
%token IF THEN ELSE
%token UCLOCK DCLOCK PHCLOCK TAIL
%token MERGE FBY WHEN WHENNOT
%token NODE LET TEL RETURNS VAR IMPORTED SENSOR ACTUATOR WCET TYPE
%token TINT TFLOAT TREAL TBOOL TCLOCK
%token RATE DUE
%token EQ DIV
%token <string> INFIX0
%token <string> INFIX1
%token <string> INFIX2

%token EOF

%nonassoc COMMA
%left MERGE IF
%left WHEN WHENNOT UCLOCK DCLOCK PHCLOCK
%right COLCOL
%right BARBAR
%right AMPERAMPER
%right NOT
%left INFIX0 EQ
%left INFIX1
%left INFIX2 DIV
%left FBY

%start prog
%type <Corelang.top_decl list> prog

%%

prog:
    typ_def_list top_decl_list EOF {$1;(List.rev $2)}

top_decl_list:
    top_decl {[$1]}
| top_decl_list top_decl {$2::$1}

top_decl:
| NODE IDENT LPAR vdecl_list RPAR RETURNS LPAR vdecl_list RPAR locals LET eq_list TEL 
    {let nd = mktop_decl (Node
                            {node_id = $2;
                             node_type = Types.new_var ();
                             node_clock = Clocks.new_var true;
                             node_inputs = List.rev $4;
                             node_outputs = List.rev $8;
                             node_locals = List.rev $10;
                             node_eqs = List.rev $12})
    in
    Hashtbl.add node_table $2 nd; nd}
| IMPORTED NODE IDENT LPAR vdecl_list RPAR RETURNS LPAR vdecl_list RPAR WCET INT SCOL
    {let nd = mktop_decl (ImportedNode
                            {nodei_id = $3;
                             nodei_type = Types.new_var ();
                             nodei_clock = Clocks.new_var true;
                             nodei_inputs = List.rev $5;
                             nodei_outputs = List.rev $9;
                             nodei_wcet = $12})
    in
    Hashtbl.add node_table $3 nd; nd}
| SENSOR IDENT WCET INT SCOL
    {let sens = mktop_decl (SensorDecl {sensor_id = $2; sensor_wcet = $4}) in
    Hashtbl.add node_table $2 sens; sens}
| ACTUATOR IDENT WCET INT SCOL
    {let act = mktop_decl (ActuatorDecl {actuator_id = $2; actuator_wcet = $4}) in
    Hashtbl.add node_table $2 act; act}

typ_def_list:
    /* empty */ {}
| typ_def SCOL typ_def_list {$1;$3}
typ_def:
    TYPE IDENT EQ IDENT 
        {
          try 
            let ty = Hashtbl.find type_table $4 in
            Hashtbl.add type_table $2 ty
          with Not_found -> raise( Corelang.Unbound_type ($4,Location.symbol_rloc()))
        }
| TYPE IDENT EQ TINT {Hashtbl.add type_table $2 Tydec_int} 
| TYPE IDENT EQ TBOOL {Hashtbl.add type_table $2 Tydec_bool} 
| TYPE IDENT EQ TREAL {Hashtbl.add type_table $2 Tydec_float} 
| TYPE IDENT EQ TFLOAT {Hashtbl.add type_table $2 Tydec_float} 
eq_list:
    eq {[$1]}
| eq_list eq {$2::$1}

eq:
    ident_list  EQ expr SCOL {mkeq ($1,$3)}
| LPAR ident_list RPAR EQ expr SCOL {mkeq (List.rev $2,$5)}

tuple_expr:
    expr COMMA expr {[$3;$1]}
| tuple_expr COMMA expr {$3::$1}

expr:
 const
    {mkexpr (Expr_const $1)}
| IDENT 
    {mkexpr (Expr_ident $1)}
| LPAR expr RPAR
    {$2}
| LPAR tuple_expr RPAR
    {mkexpr (Expr_tuple (List.rev $2))}
| const FBY expr 
    {mkexpr (Expr_fby ($1,$3))}
| const COLCOL expr
    {mkexpr (Expr_concat ($1,$3))}
| TAIL LPAR expr RPAR
    {mkexpr (Expr_tail $3)}
| expr WHEN IDENT 
    {mkexpr (Expr_when ($1,$3))}
| expr WHENNOT IDENT
    {mkexpr (Expr_whennot ($1,$3))}
| MERGE LPAR IDENT COMMA expr COMMA expr RPAR
    {mkexpr (Expr_merge ($3,$5,$7))}
| IDENT LPAR expr RPAR
    {mkexpr (Expr_appl ($1, $3))}
| IDENT LPAR tuple_expr RPAR
    {mkexpr (Expr_appl ($1, mkexpr (Expr_tuple (List.rev $3))))}
| expr UCLOCK INT
    {mkexpr (Expr_uclock ($1,$3))}
| expr DCLOCK INT
    {mkexpr (Expr_dclock ($1,$3))}
| expr PHCLOCK rat
    {mkexpr (Expr_phclock ($1,$3))}

const:
    INT {Const_int $1}
| FLOAT {Const_float $1}
| TRUE {Const_bool true}
| FALSE {Const_bool false}

locals:
  {[]}
| VAR vdecl_list SCOL {List.rev $2}

vdecl_list:
    vdecl {$1}
| vdecl_list SCOL vdecl {$3 @ $1}

vdecl:
    ident_list
    {List.map mkvar_decl 
        (List.map (fun id -> (id, mktyp Tydec_any, mkclock Ckdec_any, None)) $1)}
| ident_list COL typ clock deadline
    {List.map mkvar_decl (List.map (fun id -> (id, $3, $4, $5)) $1)}

clock:
    {mkclock Ckdec_any}
| when_list
    {mkclock (Ckdec_bool (List.rev $1))}
| RATE LPAR INT COMMA rat RPAR
    {mkclock (Ckdec_pclock ($3,$5))}

when_cond:
    WHEN IDENT {Wtrue $2}
| WHENNOT IDENT {Wfalse $2}

    when_list:
    when_cond {[$1]}
| when_list when_cond {$2::$1}
    
deadline:
    {None}
| DUE INT {Some $2}

typ:
    {mktyp Tydec_any}
| TINT {mktyp Tydec_int}
| IDENT {
  try 
    mktyp (Hashtbl.find type_table $1)
  with Not_found -> raise (Corelang.Unbound_type ($1,Location.symbol_rloc()))
}
| TFLOAT {mktyp Tydec_float}
| TREAL {mktyp Tydec_float}
| TBOOL {mktyp Tydec_bool}
| TCLOCK {mktyp Tydec_clock}

ident_list:
  IDENT {[$1]}
| ident_list COMMA IDENT {$3::$1}

rat:
    INT {($1,1)}
| INT DIV INT {($1,$3)}
| LPAR INT DIV INT RPAR {($2,$4)}
