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


(** Simple modular syntactic causality analysis. Can reject correct
    programs, especially if the program is not flattened first. *)
open Utils
open Corelang

(* var |-> successor vars*)
type dependence_graph = (ident,(ident,unit) Hashtbl.t) Hashtbl.t

exception Cycle of ident list

open Format
let pp_graph g =
  Hashtbl.iter
    (fun x succs ->
      print_string x;
      print_string " has succs: ";
        Hashtbl.iter (fun y _ -> print_string y; print_string " ") succs;
      print_newline ())
    g

(* ---- Dependencies computation *)
(* Returns the variables [e] depends on *)
let rec depends_on e =
  match e.expr_desc with
  | Expr_const _ -> []
  | Expr_ident x -> [x]
  | Expr_tuple t ->
      List.fold_left
        (fun acc e -> Utils.list_union acc (depends_on e))
        [] t
  | Expr_fby (_,_) -> []
  | Expr_concat (_,e) -> depends_on e
  | Expr_tail e -> depends_on e
  | Expr_when (e,c) -> c::(depends_on e)
  | Expr_whennot (e,c) -> c::(depends_on e)
  | Expr_merge (c,e1,e2) -> c::((depends_on e1)@(depends_on e2))
  | Expr_appl (_,e) -> depends_on e
  | Expr_uclock (e,_) -> depends_on e
  | Expr_dclock (e,_) -> depends_on e
  | Expr_phclock (e,_) -> depends_on e

(* [add_succ g from_id to_id] adds dependence [from_id]->[to_id] to
   dependence graph [g] *)
let add_succ g from_id to_id =
  try
    let succs = Hashtbl.find g from_id in
    Hashtbl.add succs to_id ()
  with Not_found ->
    let succs = Hashtbl.create 20 in
    Hashtbl.add succs to_id ();
    Hashtbl.add g from_id succs

(* Adds dependencies of equation [eq] to graph [g] *)
let add_eq_dependences g eq =
  let pat_depends_on = depends_on eq.eq_rhs in
  List.iter (fun x -> List.iter (add_succ g x) pat_depends_on) eq.eq_lhs

(* Returns the dependence graph for node [n] *)
let dependence_graph n =
  let g = Hashtbl.create 20 in
  List.iter (add_eq_dependences g) n.node_eqs;
  g

(* ---- Look for cycles in a dependence graph *)

type color = Non_cyclic | Unprocessed | Visited

(* Returns the shortest path in [trace] that forms a cycle. [trace] is a
   list of variables ordered by dependencies. [cyclic_var] is the
   variable for which the cyclic dependency was detected. *)
let rec get_cycle trace cyclic_var =
  let aux l =
    match l with
    | [] -> failwith "Internal error"
    | x::rest ->
        if x = cyclic_var then cyclic_var::(rest@[cyclic_var])
        else get_cycle rest cyclic_var
  in
  aux trace
    
(* Recursively visits the successors of [x]. Each successor is marked as
   [Visited]. If we revisit a vertex already visited, then there is a
   cycle. If no cycle is detected from [x], then it is marked
   [Non_cyclic] and future searches will ignore it and its
   successors. *)
let rec visit g colors trace x =
  Hashtbl.replace colors x Visited;
  let succs = Hashtbl.find g x in
  Hashtbl.iter
    (fun y _ ->
      try
        match (Hashtbl.find colors y) with
        | Visited ->
            let cycle = get_cycle trace y in
            raise (Cycle cycle)
        | Unprocessed -> visit g colors (trace@[y]) y
        | Non_cyclic -> ()
      with Not_found -> () (* no dependence, ie input *))
    succs;
  Hashtbl.replace colors x Non_cyclic

(* Checks that the dependency graph [g] does not contain a cycle. Raises
   [Cycle trace] if the succession of dependencies [trace] forms a cycle *)
let check_cycles g =
  let l = Hashtbl.length g in
  let colors = Hashtbl.create l in
  Hashtbl.iter (fun x _ -> Hashtbl.add colors x Unprocessed) g;
  
  Hashtbl.iter
    (fun x succs ->
      if (Hashtbl.find colors x) = Unprocessed then
        visit g colors [x] x
      else ())
    g

(* ----- Check causality *)
let check_causal_node n =
  let g = dependence_graph n in
  check_cycles g

let check_causal_decl d =
  match d.top_decl_desc with
  | Node n -> check_causal_node n
  | ImportedNode _ -> ()
  | SensorDecl _ -> ()
  | ActuatorDecl _ -> ()

let check_causal_prog p =
  List.iter check_causal_decl p

let pp_error trace =
  print_string "Causality error, cyclic data dependencies: ";
  pp_list trace print_string "" "" "->";
  print_newline ()
