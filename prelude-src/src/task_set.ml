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

(** Independent task set structure. *)
open Corelang
open Task_graph

type error =
    No_sensor_wcet of ident
  | No_actuator_wcet of ident
  | Not_sensor of ident
  | Not_actuator of ident

exception Error of Location.t * error

(* Communication protocol, on the producer side *)
type write_proto = {wbuf_size: int;
                    write_pref: bool array;
                    write_pat: bool array;
                    wbuf_init: constant option}

(* Communication protocol, on the consumer side *)
type read_proto = {rbuf_size: int;
                   change_pref: bool array;
                   change_pat: bool array;
                   rbuf_init: constant option}

type vref =
    MainVar of ident (* Input/output of the main node *)
  | TaskVar of ident*(ident*tag) (* Input/output of a node instance *)

type task_kind = Sensor | Actuator | StdTask

type task =
    {task_id: ident*tag;
     task_kind: task_kind;
     (* Input variables, with pred vertex and read protocole *)
     mutable task_inputs: (var_decl*vref*read_proto) list;
     (* Output variables with the list of their succ vertices *)
     mutable task_outputs: (var_decl*(vref*write_proto) list) list;
     task_wcet: int;
     mutable task_deadline: Deadlines.deadline_word;
     task_period: int;
     task_release: int}

type task_set = (ident*tag, task) Hashtbl.t

let dummy_vref = MainVar ""

(* Various utility functions *)

let vref_of_vertex_id vid =
  match vid with
  | Var id -> MainVar id
  | NodeVar (v,n) -> TaskVar (v,n)
  | _ -> failwith "Internal error"

let abs_release t n =
  t.task_release + (n*t.task_period)

let dummy_write_pattern = {wbuf_size = 0;
                           write_pref = Array.make 0 false;
                           write_pat = Array.make 0 false;
                           wbuf_init = None}

let dummy_read_pattern = {rbuf_size = 0;
                          change_pref = Array.make 0 false;
                          change_pat = Array.make 0 false;
                          rbuf_init = None}

let taskid_of_vertex v =
  match v with
  | Var v ->
      (v,0)
  | NodeVar (vid,nid) ->
      nid
  | PredefOp _ -> failwith "Internal error"

let task_of_vertex task_set v =
  Hashtbl.find task_set (taskid_of_vertex v)

let inputs_of_task t =
  match t.task_kind with
  | Sensor | Actuator ->
      let (vid,_) = t.task_id in
      [Var vid]
  | StdTask ->
      let (nid,_) = t.task_id in
      let nd = match (Hashtbl.find node_table nid).top_decl_desc with
      | ImportedNode ind -> ind
      | Node _ | SensorDecl _ | ActuatorDecl _ ->
          failwith "internal error inputs_of_task"
      in
      let ins =
        List.map
          (fun v -> NodeVar (v.var_id, t.task_id))
          nd.nodei_inputs in
      ins

open Format

(* ----- Pretty printing *)

let pp_task_id (nid,tag) =
  if !Options.cheddar_format then
    begin
      print_string nid
    end
  else
    begin
      print_string nid;
      print_string "(";
      print_int tag;
      print_string ")"
    end

let pp_task t =
  if !Options.cheddar_format then
    begin
      pp_task_id t.task_id;
      print_string " ";
      print_int t.task_period;
      print_string " ";
      print_int t.task_wcet;
      print_string " ";
      print_int t.task_release;
      print_string " ";
      Deadlines.print_dw t.task_deadline
    end
  else
    begin
      pp_task_id t.task_id;
      print_string ": ";
      print_string "T: ";
      print_int t.task_period;
      print_string ", C: ";
      print_int t.task_wcet;
      print_string ", r: ";
      print_int t.task_release;
      print_string ", d: ";
      Deadlines.print_dw t.task_deadline
    end

let pp_read_proto proto =
  print_string ", read protocol: buffer size is ";
  print_int proto.rbuf_size;
  Utils.pp_array proto.change_pref print_bool ", pattern: " "." ".";
  Utils.pp_array proto.change_pat print_bool "(" ")" "."

let pp_write_proto proto =
  print_string ", write protocol: buffer size is ";
  print_int proto.wbuf_size;
  Utils.pp_array proto.write_pref print_bool ", pattern: " "." ".";
  Utils.pp_array proto.write_pat print_bool "(" ")" "."

let pp_vref vref =
  match vref with
  | MainVar vid ->
      print_string vid
  | TaskVar (vid,(nid,tag)) ->
      pp_inst_id (nid,tag);
      print_string ".";
      print_string vid

let pp_task_protos t =
  List.iter
    (fun (v,_,proto) ->
      pp_inst_id t.task_id;
      print_string ".";
      print_string v.var_id;
      pp_read_proto proto;
      print_newline ())
    t.task_inputs;
  List.iter
    (fun (v,protos) ->
      Utils.pp_list protos
        (fun (succ,proto) ->
          pp_inst_id t.task_id;
          print_string ".";
          print_string v.var_id;
          print_string " to ";
          pp_vref succ;
          pp_write_proto proto;
          print_newline ())
        "" "" "\n")
    t.task_outputs

let pp_task_set_protos s =
  Hashtbl.iter (fun _ t -> pp_task_protos t) s

let pp_task_set s =
  Hashtbl.iter (fun _ t -> pp_task t;print_newline ()) s

let pp_error = function
  | No_sensor_wcet id ->
      print_string ("No wcet provided for sensor "^id);
      print_newline ()
  | No_actuator_wcet id ->
      print_string ("No wcet provided for actuator "^id);
      print_newline ()
  | Not_sensor id ->
      print_string ("Main node parameter "^id^" was not declared as a sensor");
      print_newline ()
  | Not_actuator id ->
      print_string ("Main node parameter "^id^" was not declared as a sensor");
      print_newline ()
