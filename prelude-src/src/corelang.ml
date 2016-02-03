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
  | Too_general_type

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
                                       
let pp_strict_flow_ats ty clock_dec =
  print_string "strict_flow (";
  Types.print_ty ty;
  print_string ", ";
  match clock_dec with
  | Ckdec_pclock (n, p) ->
     print_int n;
     print_string ", ";
     let (q, r) = p in
     print_string "RationalDiv(";
     print_int q;
     print_string ", ";
     print_int r;
     print_string "))";
  | _ -> raise (Error Too_general_type)
               
let pp_vars_decls_ats var_decls with_id =
  Utils.pp_list var_decls 
                (fun v ->
                 if with_id then
                   begin
                     print_string v.var_id;
                     print_string ": "
                   end;
                 let ty = v.var_type in
                 let ck_dec = v.var_dec_clock in
                 pp_strict_flow_ats ty ck_dec.ck_dec_desc
                ) "(" ")" ", "

let pp_const_ats cst =
  match cst with
  | Const_int num -> 
     print_int num
  | _ -> 
     raise (Error Too_general_type)
               
let rec pp_expr_ats expr =
  let expr_desc = expr.expr_desc in
  match expr_desc with
  | Expr_const cst ->
     pp_const_ats cst
  | Expr_ident id -> 
     print_string id
  | Expr_tuple exprs ->
     Utils.pp_list exprs (fun exp -> pp_expr_ats exp) "(" ")" ", "
  | Expr_fby (cst, expr)  ->
     print_string "(flow_fby (";
     pp_const_ats cst;
     print_string ", ";
     pp_expr_ats expr;
     print_string "))"
  | Expr_concat (cst, expr) ->
     print_string "(flow_cons (";
     pp_const_ats cst;
     print_string ", ";
     pp_expr_ats expr;
     print_string "))"
  | Expr_tail expr ->
     print_string "(flow_tail (";
     pp_expr_ats expr;
     print_string "))"
  | Expr_when (expr, id) ->
     print_string "(flow_when (";
     pp_expr_ats expr;
     print_string ", ";
     print_string id;
     print_string "))"
  | Expr_whennot (expr, id) ->
     print_string "(flow_whennot (";
     pp_expr_ats expr;
     print_string ", ";
     print_string id;
     print_string "))"
  | Expr_merge (id, on, off) ->
     print_string "(flow_merge (";
     print_string id;
     print_string ", ";
     pp_expr_ats on;
     print_string ", ";
     pp_expr_ats off;
     print_string "))";
  | Expr_appl (id, expr) ->
     print_string id;
     print_string " ";
     pp_expr_ats expr;
  | Expr_uclock (expr, k) ->
     print_string "(flow_multiply_clock (";
     pp_expr_ats expr;
     print_string ", ";
     print_int k;
     print_string "))"
  | Expr_dclock (expr, k) ->
     print_string "(flow_divide_clock (";
     pp_expr_ats expr;
     print_string ", ";
     print_int k;
     print_string "))"
  | Expr_phclock (expr, r) ->
     let (p, q) = r in
     print_string "(flow_shift_phase (";
     pp_expr_ats expr;
     print_string ", ";
     print_string "rational_make (";
     print_int p;
     print_string ", ";
     print_int q;
     print_string ")))"

let pp_node_eqs_ats nd =
  (** Get the names of all local variables. *)
  let locals = List.map (fun v -> v.var_id) nd.node_locals in
  Utils.pp_list nd.node_eqs
                (fun eq ->
                 Utils.pp_list eq.eq_lhs
                               (fun id ->
                                if List.exists 
                                     (fun vid -> id = vid) locals then
                                  begin
                                    print_string id;
                                    print_string "'";
                                  end
                                else
                                  print_string id) "val (" ") = " ", ";
                 pp_expr_ats eq.eq_rhs;
                 print_newline ()) "" "" ""

(**
Declare any local flows
 *)
let pp_local_vars_decls_ats var_decls = 
  Utils.pp_list var_decls
                (fun v ->
                 print_string "var ";
                 print_string v.var_id;
                 print_string " : ";
                 let ty = v.var_type in
                 let ck_dec = v.var_dec_clock in
                 pp_strict_flow_ats ty ck_dec.ck_dec_desc;
                 print_newline ();
                 print_string "prval pf";
                 print_string v.var_id;
                 print_string " = flow_future_make (";
                 print_string v.var_id;
                 print_string ")";
                 print_newline ()) "" "" ""

(**
"Sync up" the local flows
 *)
let pp_sync_local_vars_ats var_decls = 
  Utils.pp_list var_decls
                (fun v ->
                 print_string 
                   "prval () = flow_future_elim (pf";
                 print_string v.var_id;
                 print_string ", ";
                 print_string v.var_id;
                 print_string ", ";
                 print_string v.var_id;
                 print_string "')";
                 print_newline ()) "" "" ""

(**
Return the functions flows
 *)
let pp_output_vars nd =
  Utils.pp_list nd.node_outputs 
                (fun v -> print_string v.var_id) "(" ")" ", "

(** print the type signature of a function *)
let pp_node_sig_ats nd =
  print_string "fun ";
  print_string nd.node_id;
  pp_vars_decls_ats nd.node_inputs true;
  print_string ": ";
  pp_vars_decls_ats nd.node_outputs false

(** Print the body of a node *)
let pp_node_body nd =
  print_string " = let";
  print_newline ();
  pp_local_vars_decls_ats nd.node_locals;
  pp_node_eqs_ats nd;
  print_newline ();
  pp_sync_local_vars_ats nd.node_locals;
  print_newline ();
  print_string "in";
  print_newline ();
  pp_output_vars nd;
  print_newline ();
  print_string "end";
  print_newline ()

(** print the declaration and body of a node *)
let pp_node_ats nd =
  pp_node_sig_ats nd;
  pp_node_body nd

let pp_nodei_sig_ats nd =
  print_string "extern fun ";
  print_string nd.nodei_id;
  pp_vars_decls_ats nd.nodei_inputs true;
  print_string ": ";
  pp_vars_decls_ats nd.nodei_outputs false

let pp_nodei_ats nd = 
  pp_nodei_sig_ats nd


let pp_decl_ats tdecl =
  match tdecl.top_decl_desc with
  | Node nd ->
     pp_node_ats nd;
     print_newline ();
     print_newline ()
  | ImportedNode ind ->
     pp_nodei_ats ind;
     print_newline ();
     print_newline ()
  | SensorDecl _ | ActuatorDecl _ -> ()

let pp_prog_type tdecl_list =
  Utils.pp_list tdecl_list pp_decl_type "" "" ""

let pp_prog_ats tdecl_list =
  print_string "staload \"rat.sats\"";
  print_newline ();
  print_string "staload \"flow.sats\"";
  print_newline ();
  print_newline ();
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
  | Too_general_type ->
     print_string "Too general type encountered";
     print_newline ()
