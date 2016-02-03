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

open Corelang
open Task_graph
open Task_set

(** Buil a task set from a task graph *)

(* Adds a task for the imported node vertex [v] belongs to *)
let add_task task_set inst_clocks v =
  match v with
  | Var vid ->
      () (* Main var, added previously *)
  | NodeVar (vid,(nid,ntag)) ->
      let tid = taskid_of_vertex v in
      if not (Hashtbl.mem task_set tid) then
        let ck_nd = Hashtbl.find inst_clocks (nid,ntag) in
        let nd =
          match (Hashtbl.find node_table nid).top_decl_desc with
          | ImportedNode ind -> ind
          | Node _ | SensorDecl _  | ActuatorDecl _ ->
              failwith "internal error new_task"
        in
        let ck = Clocks.pclock_parent (Clocks.clock_of_impnode_clock ck_nd) in
        let period = Clocks.period ck in
        let (release,_) = Clocks.phase ck in
        let dd = Deadlines.dword_create period in
        let ins = List.map (fun i -> i,dummy_vref,dummy_read_pattern) nd.nodei_inputs in
        let outs = List.map (fun o -> o,[]) nd.nodei_outputs in
        let t =
        {task_id = (nid,ntag);
         task_kind = StdTask;
         task_inputs = ins;
         task_outputs = outs;
         task_wcet = nd.nodei_wcet;
         task_deadline = dd;
         task_period = period;
         task_release = release}
        in
        Hashtbl.add task_set t.task_id t
  | PredefOp _ -> failwith "Internal error"   
 
(* Adds the sensor corresponding to main node input [v]  *)
let add_sensor task_set v =
  let sdecl = try
    Hashtbl.find node_table v.var_id
  with Not_found -> 
    raise (Error (v.var_loc, No_sensor_wcet v.var_id))
  in
  let wcet = match sdecl.top_decl_desc with
  | SensorDecl s -> s.sensor_wcet
  | _ -> raise (Error (v.var_loc, Not_sensor v.var_id))
  in
  let ck = Clocks.pclock_parent v.var_clock in
  let period = Clocks.period ck in
  let (release,_) = Clocks.phase ck in
  let dd = Deadlines.dword_create period in
  let t =
    {task_id = (v.var_id,0);
     task_kind = Sensor;
     task_inputs = [];
     task_outputs = [v,[]];
     task_wcet = wcet;
     task_deadline = dd;
     task_period = period;
     task_release = release}
  in
  Hashtbl.add task_set t.task_id t

(* Adds the actuator corresponding to main node output [v]  *)
let add_actuator task_set v =
  let adecl = try
    Hashtbl.find node_table v.var_id
  with Not_found -> 
    raise (Error (v.var_loc, No_actuator_wcet v.var_id))
  in
  let wcet = match adecl.top_decl_desc with
  | ActuatorDecl a -> a.actuator_wcet
  | _ -> raise (Error (v.var_loc, Not_actuator v.var_id))
  in
  let ck = Clocks.pclock_parent v.var_clock in
  let period = Clocks.period ck in
  let (release,_) = Clocks.phase ck in
  let dd =
    match v.var_dec_deadline with
    | None -> Deadlines.dword_create period
    | Some d -> Deadlines.dword_create (min period d)
  in
  let t =
    {task_id = (v.var_id,0);
     task_kind = Actuator;
     task_inputs = [v,dummy_vref,dummy_read_pattern];
     task_outputs = [];
     task_wcet = wcet;
     task_deadline = dd;
     task_period = period;
     task_release = release}
  in
  Hashtbl.add task_set t.task_id t

(* Build a task set from a task graph *)
let of_task_graph g exp_main =
  let task_set = Hashtbl.create 30 in
  List.iter (add_sensor task_set) exp_main.node_inputs;
  List.iter (add_actuator task_set) exp_main.node_outputs;
  (* Create tasks *)
  Hashtbl.iter
    (fun vid v -> add_task task_set g.nodeinst_clocks vid)
    g.graph;
  (* Deadline calculus*)
  if (not !Options.no_encoding) then
    Deadline_calculus.dd_prog g task_set exp_main;
  (* Communication protocol *)
  Com_protocol.proto_prog g task_set;
  task_set
