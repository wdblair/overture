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
open Format

(** The core language and its ast. Every element of the ast contains its
    location in the program text. The type and clock of an ast element
    is mutable (and initialized to dummy values). This avoids to have to
    duplicate ast structures (e.g. ast, typed_ast, clocked_ast). *)

type ident = Utils.ident
type rat = Utils.rat
type tag = Utils.tag

type constant =
  | Const_int of int
  | Const_float of float
  | Const_bool of bool

type type_dec =
    {ty_dec_desc: type_dec_desc;
     ty_dec_loc: Location.t}

and type_dec_desc =
  | Tydec_any
  | Tydec_int
  | Tydec_float
  | Tydec_bool
  | Tydec_clock

type deadline_dec = int option

type clock_dec =
    {ck_dec_desc: clock_dec_desc;
     ck_dec_loc: Location.t}

and when_cond =
    Wtrue of ident
  | Wfalse of ident

and clock_dec_desc =
  | Ckdec_any
  | Ckdec_bool of when_cond list 
  | Ckdec_pclock of int * rat

type var_decl =
    {var_id: ident;
     var_dec_type: type_dec;
     var_dec_clock: clock_dec;
     var_dec_deadline: deadline_dec;
     mutable var_type: Types.type_expr;
     mutable var_clock: Clocks.clock_expr;
     var_loc: Location.t}

(* The tag of an expression is a unique identifier used to distinguish
   different instances of the same node *)
type expr =
    {expr_tag: tag;
     expr_desc: expr_desc;
     mutable expr_type: Types.type_expr;
     mutable expr_clock: Clocks.clock_expr;
     expr_loc: Location.t}

and expr_desc =
  | Expr_const of constant
  | Expr_ident of ident
  | Expr_tuple of expr list
  | Expr_fby of constant * expr
  | Expr_concat of constant * expr
  | Expr_tail of expr
  | Expr_when of expr * ident
  | Expr_whennot of expr * ident
  | Expr_merge of ident * expr * expr
  | Expr_appl of ident * expr
  | Expr_uclock of expr * int
  | Expr_dclock of expr * int
  | Expr_phclock of expr * rat

type eq =
    {eq_lhs: ident list;
     eq_rhs: expr;
     eq_loc: Location.t}

type node_desc =
    {node_id: ident;
     mutable node_type: Types.type_expr;
     mutable node_clock: Clocks.clock_expr;
     node_inputs: var_decl list;
     node_outputs: var_decl list;
     node_locals: var_decl list;
     node_eqs: eq list}

type imported_node_desc =
    {nodei_id: ident;
     mutable nodei_type: Types.type_expr;
     mutable nodei_clock: Clocks.clock_expr;
     nodei_inputs: var_decl list;
     nodei_outputs: var_decl list;
     nodei_wcet: int}

type sensor_desc =
    {sensor_id: ident;
     sensor_wcet: int}

type actuator_desc =
    {actuator_id: ident;
     actuator_wcet: int}

type top_decl_desc =
  | Node of node_desc
  | ImportedNode of imported_node_desc
  | SensorDecl of sensor_desc
  | ActuatorDecl of actuator_desc

type top_decl =
    {top_decl_desc: top_decl_desc;
     top_decl_loc: Location.t}

type program = top_decl list

type error =
    Main_not_found
  | Main_wrong_kind
  | No_main_specified

exception Error of error
exception Unbound_type of ident*Location.t
(* Fast access to nodes, by name *)
let node_table = Hashtbl.create 30

(* *)
let last_tag = ref (-1)

let new_tag () =
  incr last_tag; !last_tag

(* Caution, returns an untyped and unclocked expression *)
let expr_of_ident id loc =
  {expr_tag = new_tag ();
   expr_desc = Expr_ident id;
   expr_type = Types.new_var ();
   expr_clock = Clocks.new_var true;
   expr_loc = loc}

let expr_list_of_expr expr =
  match expr.expr_desc with
  | Expr_tuple elist ->
      elist
  | _ -> [expr]

let pp_decl_type tdecl =
  match tdecl.top_decl_desc with
  | Node nd ->
      print_string nd.node_id;
      print_string ": ";
      Utils.reset_names ();
      Types.print_ty nd.node_type;
      print_newline ()
  | ImportedNode ind ->
      print_string ind.nodei_id;
      print_string ": ";
      Utils.reset_names ();
      Types.print_ty ind.nodei_type;
      print_newline ()
 | SensorDecl _ | ActuatorDecl _ -> ()

let pp_decl_ats tdecl = 
  match tdecl.top_decl_desc with
  | Node nd ->
     print_string nd.node_id;
     print_string ": ";
     Utils.reset_names ();
     Types.print_ty nd.node_type;
     print_newline ();
     (** Print all the inputs *)
     Utils.pp_list nd.node_inputs (fun x -> print_string x.var_id) "("  ")" ", ";
     print_string ": ";
     Utils.pp_list nd.node_outputs (fun x -> print_string x.var_id) "(" ")" ", ";
     print_newline ();
 | ImportedNode ind ->
     print_string ind.nodei_id;
     print_string ": ";
     Utils.reset_names ();
     Types.print_ty ind.nodei_type;
     print_newline ();
     Utils.pp_list ind.nodei_inputs (fun x -> print_string x.var_id) "("  ")" ", ";
     print_string ": ";
     Utils.pp_list ind.nodei_outputs (fun x -> print_string x.var_id) "(" ")" ", ";
     print_newline ();	
  | SensorDecl _ | ActuatorDecl _ -> ()

let pp_prog_type tdecl_list =
  Utils.pp_list tdecl_list pp_decl_type "" "" ""

let pp_prog_ats tdecl_list =
  Utils.pp_list tdecl_list pp_decl_ats "" "" ""

let pp_decl_clock cdecl =
  match cdecl.top_decl_desc with
  | Node nd ->
      print_string nd.node_id;
      print_string ": ";
      Utils.reset_names ();
      Clocks.print_ck nd.node_clock;
      print_newline ()
  | ImportedNode ind ->
      print_string ind.nodei_id;
      print_string ": ";
      Utils.reset_names ();
      Clocks.print_ck ind.nodei_clock;
      print_newline ()
  | SensorDecl _ | ActuatorDecl _ -> ()

let pp_prog_clock prog =
  Utils.pp_list prog pp_decl_clock "" "" ""

let pp_error = function
    Main_not_found ->
      print_string "Cannot compile node ";
      print_string !Options.main_node;
      print_string ": could not find the node definition.";
      print_newline ()
  | Main_wrong_kind ->
      print_string "Name ";
      print_string !Options.main_node;
      print_string " does not correspond to a (non-imported) node definition.";
      print_newline ()
  | No_main_specified ->
      print_string "No main node specified";
      print_newline ()
