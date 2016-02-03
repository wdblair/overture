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

(** Build the task graph (dependent task set) corresponding to a node. *)
open Format
open Corelang

(* Predefined operators, ie rate transitions and all that *)
type predef_op =
  | Gfby of constant
  | Guclock of int
  | Gdclock of int
  | Gphclock of rat
  | Gtail
  | Gconcat of constant

type prec_annot = predef_op list

type vertex_id =
    Var of ident
  | NodeVar of ident * (ident * tag)
  | PredefOp of predef_op * tag

(* Code the graph with fast access adjacency lists (hashtables). Store
   both the predecessor and successors of a vertex. As we consider
   precedences between variables, each vertex can only have one
   predecessor (single assignement). *)
type vertex =
    {vertex_id: vertex_id;
     mutable vertex_pred: (vertex_id*prec_annot) option;
     mutable vertex_succs: (vertex_id, prec_annot) Hashtbl.t}

type t =
    {graph: (vertex_id, vertex) Hashtbl.t;
     nodeinst_clocks: (ident*tag, Clocks.clock_expr) Hashtbl.t}

type error =
    Main_input_unused of ident

exception Error of Location.t * error

let new_graph () =
  {graph = Hashtbl.create 30; nodeinst_clocks = Hashtbl.create 30}

let new_vertex vid =
  {vertex_id = vid;
   vertex_pred = None;
   vertex_succs = Hashtbl.create 10}

(* Returns the variable declaration corresponding to a vertex *)
let vdecl_of_vertex v =
  match v with
  | Var vid ->
      let nd = match (Hashtbl.find node_table !Options.main_node).top_decl_desc with
      | Node nd -> nd
      | ImportedNode _ | SensorDecl _ | ActuatorDecl _ ->
          failwith "Internal error vdecl_of_vertex"
      in
      List.find
        (fun vdecl -> vdecl.var_id = vid)
        (nd.node_inputs@nd.node_outputs@nd.node_locals)
  | NodeVar (vid,(nid,_)) ->
      let nd = match (Hashtbl.find node_table nid).top_decl_desc with
      | Node _ | SensorDecl _ | ActuatorDecl _ ->
          failwith "Internal error vdecl_of_vertex"
      | ImportedNode ind -> ind
      in
      List.find
        (fun vdecl -> vdecl.var_id = vid)
        (nd.nodei_inputs@nd.nodei_outputs)
  | _ -> failwith "Internal error"

(* The list of all precedences of a graph *)
let prec_list g =
  Hashtbl.fold
    (fun vid v precs ->
      let more_precs =
        Hashtbl.fold
          (fun succ_id annot l -> (vid,annot,succ_id)::l)
          v.vertex_succs [] in
      precs@more_precs)
    g.graph []

(* The numbre of precedences of a graph *)
let nb_precs g =
  Hashtbl.fold (fun _ v cpt -> cpt+(Hashtbl.length v.vertex_succs)) g.graph 0

(* ----- Graph construction, starting from a single (main) node *)

(* [add_prec g from_v to_v] adds a precedence [from_v]->[to_v]. Adds the
   vertices if they are not already elements of the graph*)
let add_prec g annot from_vid to_vid =
  let from_v = 
    try
      Hashtbl.find g.graph from_vid
    with Not_found ->
      let v = new_vertex from_vid in
      Hashtbl.add g.graph from_vid v;
      v
  in
  Hashtbl.add from_v.vertex_succs to_vid annot;
  let to_v =
    try
      Hashtbl.find g.graph to_vid
    with Not_found ->
      let v = new_vertex to_vid in
      Hashtbl.add g.graph to_vid v;
      v
  in
  to_v.vertex_pred <- (Some (from_vid,annot))

let is_delayed_prec prec_annot =
  List.exists (fun a -> match a with Gfby _ -> true | _ -> false) prec_annot

let contains_init prec_annot =
  List.exists (fun a -> match a with Gfby _ | Gconcat _ -> true |_ -> false) prec_annot

(* [no_succs expr] returns the vertices of [expr] that have no
   successors *)
let rec no_succs expr =
  match expr.expr_desc with
  | Expr_const _ -> []
  | Expr_ident id -> [Var id]
  | Expr_tuple elist -> List.flatten (List.map no_succs elist)
  | Expr_fby (cst,_) ->
      [PredefOp (Gfby cst, expr.expr_tag)]
  | Expr_uclock (_,k) ->
      [PredefOp (Guclock k, expr.expr_tag)]
  | Expr_dclock (_,k) ->
      [PredefOp (Gdclock k, expr.expr_tag)]
  | Expr_phclock (_,q) ->
      [PredefOp (Gphclock q, expr.expr_tag)]
  | Expr_tail _ ->
      [PredefOp (Gtail, expr.expr_tag)]
  | Expr_concat (cst,_) ->
      [PredefOp (Gconcat cst, expr.expr_tag)]
  | Expr_appl (id,_) ->
      let nd = Hashtbl.find node_table id in
      let outputs =
        match nd.top_decl_desc with
        | Node nd -> nd.node_outputs
        | ImportedNode ind -> ind.nodei_outputs
        | SensorDecl _ | ActuatorDecl _ -> failwith "Internal error no_succs"
      in
      List.map
        (fun v -> NodeVar (v.var_id, (id,expr.expr_tag)))
        outputs
  | Expr_when (_,_) | Expr_whennot (_,_) | Expr_merge (_,_,_) ->
      failwith "not supported yet"

let join_precs g from_vs to_vs = 
 match from_vs with
  | [] -> ()
  | _ -> List.iter2 (add_prec g []) from_vs to_vs

(** [of_expr g expr] adds the vertices and edges corresponding to [expr]
in graph [g] *)
let rec of_expr g expr =
  match expr.expr_desc with
  | Expr_const _ -> ()
  | Expr_ident id -> () (* Vertex added by precedences related to it *)
  | Expr_tuple elist -> List.iter (of_expr g) elist
  | Expr_fby (cst,e) ->
      let last = List.hd (no_succs e) in
      let v = PredefOp (Gfby cst, expr.expr_tag) in
      add_prec g [] last v;
      of_expr g e
  | Expr_uclock (e,k) ->
      let last = List.hd (no_succs e) in
      let v = PredefOp (Guclock k, expr.expr_tag) in
      add_prec g [] last v;
      of_expr g e
  | Expr_dclock (e,k) ->
      let last = List.hd (no_succs e) in
      let v = PredefOp (Gdclock k, expr.expr_tag) in
      add_prec g [] last v;
      of_expr g e
  | Expr_phclock (e,q) ->
      let last = List.hd (no_succs e) in
      let v = PredefOp (Gphclock q, expr.expr_tag) in
      add_prec g [] last v;
      of_expr g e
  | Expr_tail e ->
      let last = List.hd (no_succs e) in
      let v = PredefOp (Gtail, expr.expr_tag) in
      add_prec g [] last v;
      of_expr g e
  | Expr_concat (cst,e) ->
      let last = List.hd (no_succs e) in
      let v = PredefOp (Gconcat cst, expr.expr_tag) in
      add_prec g [] last v;
      of_expr g e
  | Expr_appl (id,e) ->
      let lasts = no_succs e in
      let nd = Hashtbl.find node_table id in
      let inputs =
        match nd.top_decl_desc with
        | Node nd -> nd.node_inputs
        | ImportedNode ind -> ind.nodei_inputs
        | SensorDecl _ | ActuatorDecl _ -> failwith "Internal error of_expr"
      in
      let nlid = (id,expr.expr_tag) in 
      let vins =
        List.map
          (fun v -> NodeVar (v.var_id, nlid))
          inputs in
      Hashtbl.add g.nodeinst_clocks nlid expr.expr_clock;
      join_precs g lasts vins;
      of_expr g e
  | Expr_when (_,_) | Expr_whennot (_,_) | Expr_merge (_,_,_) ->
      failwith "not supported yet"

(** [of_eq g eq] adds the vertices and edges corresponding to [eq]
in graph [g] *)
let of_eq g eq =
  of_expr g eq.eq_rhs;
  let lasts = no_succs eq.eq_rhs in
  let vdecls = List.map (fun vid -> Var vid) eq.eq_lhs in
  join_precs g lasts vdecls

(** [of_node nd] returns the task graph corresponding to node [nd] *)
let of_node nd =
  let g = new_graph () in
  List.iter (of_eq g) nd.node_eqs;
  g

(* ----- Graph reduction. Removes local variables and transforms
   predefined operators into precedence annotations *)

(* [transfer_succs g v] maps the predecessor of variable [v] to its
   successors. To do before reducing annotations. *)
let transfer_succs g vid =
  let v = Hashtbl.find g.graph vid in
  begin
    match v.vertex_pred with
    | None -> ()
    | Some (pred,_) ->
        let succs_pred = (Hashtbl.find g.graph pred).vertex_succs in
        Hashtbl.remove succs_pred vid;
        Hashtbl.iter
          (fun succid _ ->
            let succ = Hashtbl.find g.graph succid in
            succ.vertex_pred <- Some (pred,[]);
            Hashtbl.add succs_pred succid [])
          v.vertex_succs
  end

(* [remove_vertex g v] removes vertex [v] from graph [g].  *)
let remove_vertex g v =
  transfer_succs g v;
  Hashtbl.remove g.graph v 

(* [remove_local_vars p] removes all the local variables of graph [g]. *)
let remove_local_vars g nd =
  List.iter (fun v -> remove_vertex g (Var v.var_id)) nd.node_locals

let transfer_outputs_succs g nd =
  List.iter (fun v -> transfer_succs g (Var v.var_id)) nd.node_outputs

(* Could be optimized (made tail-recursive) *)
let rec reduce_pred g p =
  match p with
  | None -> (None, []) 
  | Some (v,_) ->
      match v with
      | Var _ | NodeVar _ -> Some v, []
      | PredefOp (op,_) ->
          let first_pred = (Hashtbl.find g.graph v).vertex_pred in
          let pred_rec, precannot = reduce_pred g first_pred in
          (* Concatenation should not be expensive as we do not expect
             very long annotation lists *)
          pred_rec, precannot@[op]
                                 
let reduce g exp_main =
  let add_prec_and_vertex g prec_annot from_vid to_vid =
    match from_vid with
    | None ->
        if not (Hashtbl.mem g.graph to_vid) then
          let to_v = new_vertex to_vid in
          Hashtbl.add g.graph to_vid to_v
    | Some from_vid ->
        add_prec g prec_annot from_vid to_vid
  in
  remove_local_vars g exp_main;
  transfer_outputs_succs g exp_main;
  let reduced =
    {graph = Hashtbl.create 30;
     nodeinst_clocks = g.nodeinst_clocks}
  in
  Hashtbl.iter
    (fun vid v ->
      match vid with
      | Var _ | NodeVar (_,_) ->
          begin
            let new_pred, predannot = reduce_pred g v.vertex_pred in
            add_prec_and_vertex reduced predannot new_pred vid
          end
      | PredefOp (_,_) -> ())
    g.graph;
  reduced

(* ----- Check graph topology. For now, only checks that no main node input is unused *)
let check_sensors_unused g exp_main =
  List.iter (fun vin ->
    if not (Hashtbl.mem g.graph (Var vin.var_id))
    then raise (Error (vin.var_loc, Main_input_unused vin.var_id)))
    exp_main.node_inputs

let reduced_task_graph exp_main =
  let g = of_node exp_main in
  check_sensors_unused g exp_main;
  reduce g exp_main

(* ----- Pretty printing *)
let pp_predef op =
  match op with
  | Gfby init ->
      print_string "fby"
  | Guclock k ->
      print_string "*^";
      print_int k
  | Gdclock k ->
      print_string "/^";
      print_int k
  | Gphclock q ->
      print_string "~>";
      Utils.print_rat q
  | Gtail ->
      print_string "tail"
  | Gconcat _ ->
      print_string "concat"

let pp_inst_id (ident,tag) =
  print_string ident;
  print_string "(";
  print_int tag;
  print_string ")"

let pp_vertex_id v =
  match v with
  | Var vid ->
      print_string vid
  | NodeVar (vid,(nid,tag)) ->
      pp_inst_id (nid,tag);
      print_string ".";
      print_string vid
  | PredefOp (op,tag) ->
      pp_predef op;
      print_string "(";
      print_int tag;
      print_string ")"
    
let pp_annot alist =
  Utils.pp_list alist pp_predef " -" "-> " "."
      
let pp_graph g =
  Hashtbl.iter
    (fun vid v ->
      Hashtbl.iter
        (fun s annot ->
          pp_vertex_id vid;
          pp_annot annot;
          pp_vertex_id s;
          print_newline ())
        v.vertex_succs)
    g.graph

let pp_error = function
    Main_input_unused id ->
      print_string ("Main node input "^id^" is unused");
      print_newline ()
