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

(** Print task set as C code. Completely OS independent. Not much
    difficulty, but quite verbose. Probably needs some refactoring. *)

open Types
open Corelang
open Task_graph
open Task_set
open Format

(* Tuple types for tasks with multiple outputs. We need to know if this
   type is already declared in the case where the same node is instanciated
   several times in the same program. *)
let declared_tuple_types = Hashtbl.create 30

let vref_of_task_var task v =
  match task.task_kind with
  | Sensor | Actuator -> MainVar v.var_id
  | StdTask -> TaskVar (v.var_id, task.task_id)

let string_of_constant c =
  match c with
  | Const_int i -> string_of_int i
  | Const_float f -> string_of_float f
  | Const_bool b ->
      if b then "1" else "0"

let pp_bool out_f b =
  if b then
    fprintf out_f "1"
  else
    fprintf out_f "0"

let task_struct_name () =
  if !Options.no_encoding
  then "nonencoded_task_params"
  else "encoded_task_params"

let prec_struct_name = "multirate_precedence"

let job_prec_struct_name = "job_prec"

let task_name (nid,ntag) =
  nid^(string_of_int ntag)

let thread_name (nid,ntag) =
  "t"^nid^(string_of_int ntag)

let fun_name (nid,ntag) =
  nid^(string_of_int ntag)^"_fun"

let external_name (nid,ntag) =
  nid

let actuator_name tid =
  let (nid,_) = tid in
  "output_"^nid

let sensor_name tid =
  let (nid,_) = tid in
  "input_"^nid

let outstruct_name (nid,ntag) =
  (fun_name (nid,ntag))^"_outs"

let outstruct_type (nid,ntag) =
  "struct "^nid^"_outs_t"

let load_name (nid,ntag) =
  "load_"^nid^(string_of_int ntag)

let inst_cpt_name = "instance"

let trace_ins_buf_name = "trace_ins_buf"
let trace_outs_buf_name = "trace_outs_buf"
let trace_buf_maxsize = "1024"

let extname_of_vref v =
  match v with
  | MainVar vid -> vid
  | TaskVar (vid,(nid,ntag)) -> nid

let vname_of_vref v =
  match v with
  | MainVar vid -> vid
  | TaskVar (vid,(nid,ntag)) ->
      nid^(string_of_int ntag)^"_"^vid

let tystring_of_type ty =
  match (repr ty).tdesc with
  | Tint -> "int"
  | Tfloat -> "float"
  | Tbool | Tclock -> "int"
  | _ -> failwith "Internal error tystring_of_type"

let fmtstring_of_type ty =
  match (repr ty).tdesc with
  | Tint -> "%d"
  | Tfloat -> "%f"
  | Tbool | Tclock -> "%d"
  | _ -> failwith "Internal error fmtstring_of_type"

let combuffer_name from_v to_v =
  (vname_of_vref from_v)^"_"^(vname_of_vref to_v)

let outvar_name task vout =
  if List.length task.task_outputs > 1 then
    (outstruct_name task.task_id)^"."^vout.var_id
  else
    vout.var_id

let read_proto_name vin =
  vin.var_id^"_change"

let read_cell_pointer_name vin =
  vin.var_id^"_rcell"

let write_proto_name vout vref_succ =
  vout.var_id^"_"^(vname_of_vref vref_succ)^"_write"

let write_cell_pointer_name vout vref_succ =
    vout.var_id^"_"^(vname_of_vref vref_succ)^"_wcell"

let types_filename nodename =
  nodename

let nb_tasks_name = "PLUD_TASK_NUMBER"

let static_task_set_name = "static_task_set"

let dword_pref_name tname = tname^"_dwpref"

let dword_pat_name tname = tname^"_dwpat"

let nb_precs_name = "PLUD_PREC_NUMBER"

let static_prec_set_name = "static_prec_set"

let task_name_of_vertex src =
  task_name (taskid_of_vertex src)

let prec_pref_name (src,dst) =
  (task_name_of_vertex src)^"_"^
  (task_name_of_vertex dst)^"_pcpref"
                               
let prec_pat_name (srcid,dstid) =
  (task_name_of_vertex  srcid)^"_"^
  (task_name_of_vertex dstid)^"_pcpat"

let externals_filename basename =
  basename^"_includes"

let time_to_timespec i =
  let sec = i/1000 in
  let nsec = (i mod 1000)*1000000 in
  (sec,nsec)

let pp_header out_f basename =
  fprintf out_f "@[<v>";
  fprintf out_f "#include <stdlib.h>@ ";
  if Options.trace_printf () then
    fprintf out_f "#include <stdio.h>@ ";
  if Options.trace_lttng () then
    begin
      fprintf out_f "#define TRACEPOINT_CREATE_PROBES@ ";
      fprintf out_f "#define TRACEPOINT_DEFINE@ ";
      fprintf out_f "#include \"prelude_lttng.h\"@ "
    end;
  fprintf out_f "#include \"%s.h\"@ " (types_filename !Options.main_node);
  fprintf out_f "#include \"%s.h\"@ @ " (externals_filename basename)

let pp_types_header types_f =
  fprintf types_f "@[<v>";
  fprintf types_f "#ifndef _%s_H@ " !Options.main_node;
  fprintf types_f "#define _%s_H@ " !Options.main_node;
  
  fprintf types_f "#include \"%s.h\"@ " (task_struct_name ());
  if !Options.no_encoding then
    fprintf types_f "#include \"%s.h\"@ " prec_struct_name;
  fprintf types_f "#include \"com_patterns.h\"@ @ "

let pp_types_trailer types_f =
  fprintf types_f "void get_task_set(int* task_number, struct %s** task_set);@ @ "
    (task_struct_name ());
  if !Options.no_encoding then
    fprintf types_f "void get_precedence_set(int* prec_number, struct %s** presc);@ @ "
      prec_struct_name;
  fprintf types_f "#endif";
  fprintf types_f "@."

let pp_tuple_type types_f task =
  let task_name,_ = task.task_id in
  if not (Hashtbl.mem declared_tuple_types task_name) then
    begin
      Hashtbl.add declared_tuple_types task_name ();
      fprintf types_f "@[<v 2>%s {@ " (outstruct_type task.task_id);
      Print.list_printer_from_printer "" "@ " "@]@ };@ @ "
        (fun types_f (outv,_) ->
          let ty = tystring_of_type outv.var_type in
          fprintf types_f "%s %s;" ty outv.var_id)
        types_f task.task_outputs
    end

let pp_buffer_alloc out_f task ty read_proto vref_from vref_to =
  let bname = combuffer_name vref_from vref_to in
  let ty = tystring_of_type ty in
  let buf_size = read_proto.rbuf_size in
  if Options.trace_instances () then
    if buf_size > 1 then
        begin
          fprintf out_f "struct { %s value; int instance; } %s[%i]" ty bname buf_size;
          match read_proto.rbuf_init with
          | Some cst ->
              fprintf out_f "={";
              for i=0 to buf_size -2 do
                fprintf out_f "{%s, -1}, " (string_of_constant cst)
              done;
              fprintf out_f "{%s, -1}};@ " (string_of_constant cst)
          | None -> fprintf out_f ";@ "
        end
    else
      fprintf out_f "struct { %s value; int instance; } %s;@ " ty bname
  else
    if buf_size > 1 then
        begin
          fprintf out_f "%s %s[%i]" ty bname buf_size;
          match read_proto.rbuf_init with
          | Some cst ->
              fprintf out_f "={";
              for i=0 to buf_size -2 do
                fprintf out_f "%s, " (string_of_constant cst)
              done;
              fprintf out_f "%s};@ " (string_of_constant cst)
          | None -> fprintf out_f ";@ "
        end
    else
      fprintf out_f "%s %s;@ " ty bname
      
let pp_buffer_allocs_task out_f task =
  List.iter
    (fun (vin,vref_from,read_proto) ->
      let vref_to = vref_of_task_var task vin in
      pp_buffer_alloc out_f task vin.var_type read_proto vref_from vref_to)
    task.task_inputs

let pp_buffer_allocs out_f task_set =
  Hashtbl.iter (fun _ task -> pp_buffer_allocs_task out_f task) task_set;
  fprintf out_f "@ "

let pp_outbuffer_alloc out_f types_f task =
  if (List.length task.task_outputs >1) then
    begin
      pp_tuple_type types_f task;
      fprintf out_f "%s %s;@ " (outstruct_type task.task_id)
        (outstruct_name task.task_id)
    end
  else if List.length task.task_outputs = 1 then
    let vout,_ = List.hd task.task_outputs in
    let ty = tystring_of_type vout.var_type in
    fprintf out_f "%s %s;@ " ty vout.var_id

let pp_read_proto out_f (vin,vref_pred,proto) =
  if proto.rbuf_size > 1 then
    let pref_length, pat_length =
      (Array.length proto.change_pref),
      (Array.length proto.change_pat) in
    if (pref_length > 1) || (pat_length > 1) then
      begin
        let proto_name = read_proto_name vin in
        fprintf out_f "const struct read_proto_t %s =@ { " proto_name;
        (* Prefix *)
        if pref_length > 0 then
          begin
            fprintf out_f "(int []){ %a " pp_bool proto.change_pref.(0);
            for i = 1 to pref_length - 1 do
              fprintf out_f ", %a " pp_bool proto.change_pref.(i)
            done;
            fprintf out_f "}, "
          end
        else
          fprintf out_f "NULL, ";
        fprintf out_f "%i, " pref_length;
        (* Pattern *)
        if pat_length > 0 then
          begin
            fprintf out_f "(int []){ %a " pp_bool proto.change_pat.(0);
            for i = 1 to pat_length - 1 do
              fprintf out_f ", %a " pp_bool proto.change_pat.(i)
            done;
            fprintf out_f "}, "
          end
        else
          fprintf out_f "NULL, ";
        fprintf out_f "%i };@ " pat_length
      end;
    fprintf out_f "static int %s=0;@ " (read_cell_pointer_name vin)
 
let pp_write_proto out_f vout vref_succ proto =
  if proto.wbuf_size > 1 then
    let pref_length, pat_length =
      (Array.length proto.write_pref),
      (Array.length proto.write_pat) in
    if  (pref_length > 1) || (pat_length > 1) then
      begin
        let proto_name = write_proto_name vout vref_succ in
        fprintf out_f "const struct write_proto_t %s =@ { " proto_name;
        (* Prefix *)
        if pref_length > 0 then
          begin
            fprintf out_f "(int []){ %a " pp_bool proto.write_pref.(0);
            for i = 1 to pref_length - 1 do
              fprintf out_f ", %a " pp_bool proto.write_pref.(i)
            done;
            fprintf out_f "}, "
          end
        else
          fprintf out_f "NULL, ";
        fprintf out_f "%i, " pref_length;
        (* Pattern *)
        if pat_length > 0 then
          begin
            fprintf out_f "(int []){ %a " pp_bool proto.write_pat.(0);
            for i = 1 to pat_length - 1 do
              fprintf out_f ", %a " pp_bool proto.write_pat.(i)
            done;
            fprintf out_f "}, "
          end
        else
          fprintf out_f "NULL, ";
        fprintf out_f "%i };@ " pat_length
      end;
    let init_cell = match proto.wbuf_init with | Some _ -> 1 | None -> 0 in
    fprintf out_f "static int %s=%i;@ " (write_cell_pointer_name vout vref_succ)
      init_cell

let pp_write_protos out_f (vout,readers) =
  List.iter
    (fun (vref_succ,proto) -> pp_write_proto out_f vout vref_succ proto)
    readers

let pp_fun_input out_f task vin vref_pred proto =
  let vref_in = vref_of_task_var task vin in
  let buf_name = combuffer_name vref_pred vref_in in
  fprintf out_f "%s" buf_name;
  if proto.rbuf_size > 1 then
    fprintf out_f "[%s]" (read_cell_pointer_name vin);
  if Options.trace_instances () then
      fprintf out_f ".value"

let pp_read_cell_update out_f (vin,vref_pred,proto) =
  let pointer_name = read_cell_pointer_name vin in
  let proto_name = read_proto_name vin in
  let uses_protocol = (Array.length proto.change_pref > 1) ||
  (Array.length proto.change_pat > 1) in
  if proto.rbuf_size > 1 then
    begin
      if uses_protocol then
        begin
          fprintf out_f "@[<v 2>if(must_change(%s,%s))@ " proto_name inst_cpt_name
        end;
      fprintf out_f "%s=(%s+1)%%%i;" pointer_name pointer_name proto.rbuf_size;
      if uses_protocol then
        fprintf out_f "@]";
      fprintf out_f "@ "
    end

let pp_write_copy out_f task vout vref_succ proto =
  let pointer_name = write_cell_pointer_name vout vref_succ in
  let proto_name = write_proto_name vout vref_succ in
  let uses_protocol = (Array.length proto.write_pref > 1) ||
  (Array.length proto.write_pat > 1) in
  if uses_protocol then
    begin
      fprintf out_f "@[<v 2>if(must_write(%s,%s)) {@ " proto_name inst_cpt_name
    end;
  let com_buf = combuffer_name (vref_of_task_var task vout) vref_succ in
  let out_var = outvar_name task vout in
  if Options.trace_instances () then
    if proto.wbuf_size > 1 then
      begin
        fprintf out_f "%s[%s].value=%s;@ " com_buf pointer_name out_var;
        fprintf out_f "%s[%s].instance=%s;@ " com_buf pointer_name inst_cpt_name;
        fprintf out_f "%s=(%s+1)%%%i;" pointer_name pointer_name proto.wbuf_size
      end
    else
      begin
        fprintf out_f "%s.value=%s;@ " com_buf out_var;
        fprintf out_f "%s.instance=%s;" com_buf inst_cpt_name
      end
  else
    if proto.wbuf_size > 1 then
      begin
        fprintf out_f "%s[%s]=%s;@ " com_buf pointer_name out_var;
        fprintf out_f "%s=(%s+1)%%%i;" pointer_name pointer_name proto.wbuf_size
      end
    else
      fprintf out_f "%s=%s;" com_buf out_var;
  if uses_protocol then
    fprintf out_f "@]@ }@ "
  else
    fprintf out_f "@ "

let pp_write_copies out_f task (vout,readers) =
  List.iter
    (fun (vref_succ,proto) -> pp_write_copy out_f task vout vref_succ proto)
    readers

let pp_trace_buf_alloc out_f task =
  if Options.trace_lttng () then
    begin
      match task.task_kind with
      | StdTask ->
          fprintf out_f "static char %s [%s];@ " trace_ins_buf_name trace_buf_maxsize;
          fprintf out_f "static char %s [%s];@ @ " trace_outs_buf_name trace_buf_maxsize
      | Sensor ->
          fprintf out_f "static char %s [%s];@ @ " trace_outs_buf_name trace_buf_maxsize
      | Actuator ->
          fprintf out_f "static char %s [%s];@ @ " trace_ins_buf_name trace_buf_maxsize
    end

let pp_trace_inputs_pat_printer out_f task (decl, ref, proto) =
  let valfmt = fmtstring_of_type decl.var_type in
  if Options.trace_instances () then
    fprintf out_f "%s=<%s,%s@@%%d>" decl.var_id valfmt (extname_of_vref ref)
  else
    fprintf out_f "%s=%s" decl.var_id valfmt

let pp_trace_inputs_pat out_f task =
  Print.list_printer_from_printer "\"@,\"" " \"@,\"" "\"@,\""
    (fun out_f t -> (pp_trace_inputs_pat_printer out_f task t))
    out_f task.task_inputs

let pp_trace_output_val_pat out_f task =
  let vout,_ = List.hd task.task_outputs in
  let valfmt = fmtstring_of_type vout.var_type in
  if Options.trace_printf () then
    fprintf out_f "=> %s=%s" vout.var_id valfmt
  else
    fprintf out_f "%s=%s" vout.var_id valfmt

let pp_trace_outputs_struct_pat_printer out_f task (decl, list) =
  let valfmt = fmtstring_of_type decl.var_type in
  fprintf out_f "%s=%s" decl.var_id valfmt
let pp_trace_output_struct_pat out_f task =
  if Options.trace_printf () then
    fprintf out_f "=> ";
  Print.list_printer_from_printer "\"@,\"" " \"@,\"" "\"@,\""
    (fun out_f t -> (pp_trace_outputs_struct_pat_printer out_f task t))
    out_f task.task_outputs

let pp_trace_input_name out_f task vin vref_pred proto =
  let vref_in = vref_of_task_var task vin in
  let buf_name = combuffer_name vref_pred vref_in in
  fprintf out_f "%s" buf_name;
  if proto.rbuf_size > 1 then
    fprintf out_f "[%s]" (read_cell_pointer_name vin);
  if Options.trace_instances () then
    begin
      fprintf out_f ".value, ";
      fprintf out_f "%s" buf_name;
      if proto.rbuf_size > 1 then
        fprintf out_f "[%s]" (read_cell_pointer_name vin);
      fprintf out_f ".instance"
    end

let pp_trace_inputs_val_printer out_f task (decl, ref, proto) =
  fprintf out_f ",@,";
  pp_trace_input_name out_f task decl ref proto
let pp_trace_inputs_val out_f task =
  List.iter (pp_trace_inputs_val_printer out_f task) task.task_inputs

let pp_trace_output_val_val out_f task =
  let vout,_ = List.hd task.task_outputs in
  fprintf out_f ",@,%s" vout.var_id

let pp_trace_outputs_struct_val_printer out_f task (decl, list) =
  fprintf out_f ",@,%s.%s " (outstruct_name task.task_id) decl.var_id
let pp_trace_output_struct_val out_f task =
  List.iter (pp_trace_outputs_struct_val_printer out_f task) task.task_outputs

let pp_trace_node out_f task =
  if Options.trace_printf () then
    begin
      fprintf out_f "fflush(NULL);@ fprintf(stderr, \"";
      begin
        match task.task_kind with
        | StdTask ->
            if Options.trace_instances () then
              fprintf out_f "NODE %s@@%%d " (external_name task.task_id)
            else
              fprintf out_f "NODE %s " (external_name task.task_id);
            pp_trace_inputs_pat out_f task;
            fprintf out_f " ";
            if List.length task.task_outputs = 1 then
              pp_trace_output_val_pat out_f task
            else
              pp_trace_output_struct_pat out_f task;
            fprintf out_f "\\n\"";
            if Options.trace_instances () then
              fprintf out_f ",@,%s" inst_cpt_name;
            pp_trace_inputs_val out_f task;
            if List.length task.task_outputs = 1 then
              pp_trace_output_val_val out_f task
            else
              pp_trace_output_struct_val out_f task
        | Sensor ->
            if Options.trace_instances () then
              fprintf out_f "SENS %s@@%%d " (external_name task.task_id)
            else
              fprintf out_f "SENS %s " (external_name task.task_id);
            pp_trace_output_val_pat out_f task;
            fprintf out_f "\\n\"";
            if Options.trace_instances () then
              fprintf out_f ",@,%s" inst_cpt_name;
            pp_trace_output_val_val out_f task
        | Actuator ->
            if Options.trace_instances () then
              fprintf out_f "ACTU %s@@%%d " (external_name task.task_id)
            else
              fprintf out_f "ACTU %s " (external_name task.task_id);
            pp_trace_inputs_pat out_f task;
            fprintf out_f "\\n\"";
            if Options.trace_instances () then
              fprintf out_f ",@,%s" inst_cpt_name;
            pp_trace_inputs_val out_f task
      end;
      fprintf out_f ");@ "
    end
  else if Options.trace_lttng () then
    begin
      match task.task_kind with
        | StdTask ->
            fprintf out_f "snprintf(%s, %s, \"" trace_ins_buf_name trace_buf_maxsize;
            pp_trace_inputs_pat out_f task;
            fprintf out_f "\"";
            pp_trace_inputs_val out_f task;
            fprintf out_f ");@ ";
            fprintf out_f "snprintf(%s, %s, \"" trace_outs_buf_name trace_buf_maxsize;
            if List.length task.task_outputs = 1 then
              begin
                pp_trace_output_val_pat out_f task;
                fprintf out_f "\"";
                pp_trace_output_val_val out_f task
              end
            else
              begin
                pp_trace_output_struct_pat out_f task;
                fprintf out_f "\"";
                pp_trace_output_struct_val out_f task
              end;
            fprintf out_f ");@ ";
            fprintf out_f "tracepoint(prelude, task, \"%s\", %s, %s, %s);@ "
              (external_name task.task_id) inst_cpt_name trace_ins_buf_name trace_outs_buf_name
        | Sensor ->
            fprintf out_f "snprintf(%s, %s, \"" trace_outs_buf_name trace_buf_maxsize;
            if List.length task.task_outputs = 1 then
              begin
                pp_trace_output_val_pat out_f task;
                fprintf out_f "\"";
                pp_trace_output_val_val out_f task
              end
            else
              begin
                pp_trace_output_struct_pat out_f task;
                fprintf out_f "\"";
                pp_trace_output_struct_val out_f task
              end;
            fprintf out_f ");@ ";
            fprintf out_f "tracepoint(prelude, sensor, \"%s\", %s, %s);@ "
              (external_name task.task_id) inst_cpt_name trace_outs_buf_name
        | Actuator ->
            fprintf out_f "snprintf(%s, %s, \"" trace_ins_buf_name trace_buf_maxsize;
            pp_trace_inputs_pat out_f task;
            fprintf out_f "\"";
            pp_trace_inputs_val out_f task;
            fprintf out_f ");@ ";
            fprintf out_f "tracepoint(prelude, actuator, \"%s\", %s, %s);@ "
              (external_name task.task_id) inst_cpt_name trace_ins_buf_name
    end

let pp_function_call out_f task =
  match task.task_kind with
  | StdTask ->
      let fun_name = external_name task.task_id in
      if List.length task.task_outputs = 1 then
        begin
          let vout,_ = List.hd task.task_outputs in
          fprintf out_f "%s=%s" vout.var_id fun_name;
          Print.list_printer_from_printer "(" "," ")"
            (fun out (vin,vref_pred,proto) ->
              pp_fun_input out task vin vref_pred proto)
            out_f task.task_inputs;
          fprintf out_f ";@ "
        end
      else
        begin
          fprintf out_f "%s(" fun_name;
          Print.list_printer_from_printer "" "," ""
            (fun out (vin,vref_pred,proto) ->
              pp_fun_input out task vin vref_pred proto)
            out_f task.task_inputs;
          fprintf out_f ",&%s" (outstruct_name task.task_id);
          fprintf out_f ");@ "
        end
  | Sensor ->
      let vout,_ = List.hd task.task_outputs in
      fprintf out_f "%s=%s();@ " vout.var_id (sensor_name task.task_id)
  | Actuator ->
      fprintf out_f "%s" (actuator_name task.task_id);
      Print.list_printer_from_printer "(" "," ")"
        (fun out (vin,vref_pred,proto) ->
          pp_fun_input out task vin vref_pred proto)
        out_f task.task_inputs;
      fprintf out_f ";@ "

let pp_task_header out_f task =
  fprintf out_f "int %s(void* args)@ "(fun_name task.task_id);
  fprintf out_f "@[<v 2>{@ "

let pp_task_body out_f types_f task =
  pp_task_header out_f task;
  
  (* Variables *)
(*      List.iter (pp_delay_init out_f) precs; *) (* TO DO *)
  pp_outbuffer_alloc out_f types_f task;
  List.iter (pp_read_proto out_f) task.task_inputs;
  List.iter (pp_write_protos out_f) task.task_outputs;
  fprintf out_f "static int %s=0;@ @ " inst_cpt_name;
  pp_trace_buf_alloc out_f task;
  
  (* Function call *)
  pp_function_call out_f task;
  
  (* Insert code for tracing *)
  pp_trace_node out_f task;

  (* Updates for next iteration *)
  List.iter (pp_read_cell_update out_f) task.task_inputs;
  List.iter (pp_write_copies out_f task) task.task_outputs;
  fprintf out_f "%s++;@ @ " inst_cpt_name;
  fprintf out_f "return 0;";
  
  fprintf out_f "@]@ }@ " (* close function *)

let pp_task_bodies out_f types_f task_set =
  Hashtbl.iter
    (fun _ t -> pp_task_body out_f types_f t; fprintf out_f "@ ")
    task_set

let pp_task_dword_arrays out_f task =
  let (pref,pat) = task.task_deadline in
  let dw_pref_length = Array.length pref in
  let dw_pat_length = Array.length pat in
  let tname = task_name task.task_id in
  if dw_pref_length <> 0 then
    begin
      fprintf out_f "static int %s[%i] = "
        (dword_pref_name tname) dw_pref_length;
      Print.array_printer_from_printer "{@ " ",@ " "@ };" 
        (fun out_f (_,v) -> fprintf out_f "%d" v)
        out_f pref
    end;
    if dw_pat_length <> 0 then
    begin
      fprintf out_f "static int %s[%i] = "
        (dword_pat_name tname) dw_pat_length;
      Print.array_printer_from_printer "{@ " ",@ " "@ };" 
        (fun out_f (_,v) -> fprintf out_f "%d" v)
        out_f pat
    end

let pp_dword out_f task =
  let (pref,pat) = task.task_deadline in
  let dw_pref_length = Array.length pref in
  let dw_pat_length = Array.length pat in
  let tname = task_name task.task_id in

  fprintf out_f "{@ ";
  if dw_pref_length = 0 then
    fprintf out_f "NULL,@ "
  else
    fprintf out_f "%s,@ " (dword_pref_name tname);
  fprintf out_f "%i,@ " dw_pref_length;
  if dw_pat_length = 0 then
    fprintf out_f "NULL,@ "
  else
    fprintf out_f "%s,@ " (dword_pat_name tname);
  fprintf out_f "%i@ }" dw_pat_length

let pp_deadline out_f task =
  if !Options.no_encoding
  then fprintf out_f "%d" (Deadlines.nth_ud task.task_deadline 0)
  else pp_dword out_f task
      
let pp_task_params out_f task =
  fprintf out_f "{@ \"%s\",@ %i,@ %i,@ %i,@ %a,@ %s@ }"
    (task_name task.task_id)
    task.task_period task.task_release task.task_wcet
    pp_deadline task
    (fun_name task.task_id)

let pp_tasks_params out_f task_set =
  (* Task params *)
  if (not !Options.no_encoding) then
    Print.hashtbl_printer_from_printer "" "@ " "@ @ "
      (fun out_f (_, t) -> pp_task_dword_arrays out_f t)
      out_f task_set;

  let nb_tasks = Hashtbl.length task_set in
  fprintf out_f "#define %s %i@\n" nb_tasks_name nb_tasks;
  fprintf out_f "@[<v 2>static struct %s %s[%s] = "
    (task_struct_name ()) static_task_set_name nb_tasks_name; 
  Print.hashtbl_printer_from_printer "{@ " ",@ " "@]@ };@ @ "
    (fun out_f (_, t) -> pp_task_params out_f t)
    out_f task_set;

  (* header*)
  fprintf out_f "void get_task_set (int* task_number, struct %s** task_set)@ "
    (task_struct_name ());
  fprintf out_f "@[<v 2>{@ ";

  (* Return values *)
  fprintf out_f "*task_number = %s;@ " nb_tasks_name;
  fprintf out_f "*task_set=%s;" static_task_set_name;  

  (* trailer *)
  fprintf out_f "@]@ }@ @ "

let pp_job_prec out_f (n,n') =
  fprintf out_f "{%d,%d}" n n'

let pp_prec_arrays out_f (src,annot,dst) =
  let (pref,pat) = Precedence_functions.prec_relation annot in
  let pref_length = Hashtbl.length pref in
  let pat_length = Hashtbl.length pat in
  if pref_length <> 0 then
    begin
      fprintf out_f "static struct %s %s[%i] = "
        job_prec_struct_name (prec_pref_name (src,dst)) pref_length;
      Print.hashtbl_printer_from_printer "{@ " ",@ " "@ };"
        pp_job_prec out_f pref
    end;
    if pat_length <> 0 then
    begin
      fprintf out_f "static struct %s %s[%i] = "
        job_prec_struct_name (prec_pat_name (src,dst)) pat_length;
      Print.hashtbl_printer_from_printer "{@ " ",@ " "@ };"
        pp_job_prec out_f pat
    end

let pp_prec_pref_ptr out_f size src dst =
  if size = 0 then
    fprintf out_f "NULL"
  else
    fprintf out_f "%s" (prec_pref_name (src,dst))

let pp_prec_pat_ptr out_f size src dst =
  if size = 0 then
    fprintf out_f "NULL"
  else
    fprintf out_f "%s" (prec_pat_name (src,dst))

let pp_precedence out_f src annot dst =
  let (pref,pat) = Precedence_functions.prec_relation annot in
  let pref_length = Hashtbl.length pref in
  let pat_length = Hashtbl.length pat in
  fprintf out_f "{ \"%s\",@ \"%s\",@ %d,@ %d,@ "
    (task_name_of_vertex src) (task_name_of_vertex dst)
    pref_length pat_length;
  pp_prec_pref_ptr out_f pref_length src dst;
  fprintf out_f ",@ ";
  pp_prec_pat_ptr out_f pat_length src dst;
  fprintf out_f "@ }"
    
let pp_precedences out_f task_graph =
  (* precs declaration *)
  let precs = prec_list task_graph in
  Print.list_printer_from_printer "" "@ " "@ @ "
    (fun out_f prec -> pp_prec_arrays out_f prec)
    out_f precs;

  let nb_precs = nb_precs task_graph in
  fprintf out_f "#define %s %i@\n" nb_precs_name nb_precs;
  fprintf out_f "@[<v 2>static struct %s %s[%s] = "
    prec_struct_name static_prec_set_name nb_precs_name; 
  Print.list_printer_from_printer "{@ " ",@ " "@]@ };@ @ "
    (fun out_f (src,annot,dst) -> pp_precedence out_f src annot dst)
    out_f precs;

  (* header*)
  fprintf out_f "void get_precedence_set (int* prec_number, struct %s** prec_set)@ "
    prec_struct_name;
  fprintf out_f "@[<v 2>{@ ";

  (* Return values *)
  fprintf out_f "*prec_number = %s;@ " nb_precs_name;
  fprintf out_f "*prec_set=%s;" static_prec_set_name;  

  (* trailer *)
  fprintf out_f "@]@ }"

let make_dir dir =
  if not (Sys.file_exists dir) then
    ignore(Sys.command ("mkdir " ^ dir))

let pp_prog task_set task_graph basename =
  let dir =
    if !Options.dest_dir = "" then
      basename^"_c"
    else !Options.dest_dir in
  ignore(make_dir (dir));
  let types_file = dir ^"/"^ !Options.main_node ^".h" in
  let types_ch = open_out types_file in
  let types_f = formatter_of_out_channel types_ch in
  let target_file = dir ^"/"^ !Options.main_node ^".c" in
  let out_ch = open_out target_file in
  let out_f = formatter_of_out_channel out_ch in
  pp_types_header types_f;
  pp_header out_f basename;
  pp_buffer_allocs out_f task_set;
  pp_task_bodies out_f types_f task_set;
  pp_tasks_params out_f task_set;
  if !Options.no_encoding then
    pp_precedences out_f task_graph;
  fprintf out_f "@.";
  pp_types_trailer types_f;
  close_out types_ch;
  close_out out_ch
