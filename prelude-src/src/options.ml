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

let version = "1.3-svn"
let main_node = ref ""
let print_types = ref false
let print_clocks = ref false
let print_ats = ref false
let print_deadlines = ref false
let print_protocols = ref false
let cheddar_format = ref false
let to_lustre = ref false
let no_encoding = ref false
let dest_dir = ref ""
let tracing = ref "no"

let options =
  [ "-cheddar", Arg.Set cheddar_format,
    "simple formatting of reduced task graph for Cheddar tests";
    "-d", Arg.Set_string dest_dir,
    "produces code in the specified directory";
    "-node", Arg.Set_string main_node, "specifies the main node";
    "-no_encoding", Arg.Set no_encoding,
    "produces C code without RT attributes adjustment";
    "-print_types", Arg.Set print_types, "print node types";
    "-print_clocks", Arg.Set print_clocks, "print node clocks";
    "-print_ats", Arg.Set print_ats, "print ats code";
    "-print_deadlines", Arg.Set print_deadlines, "print node deadlines";
    "-print_protocols", Arg.Set print_protocols, "print communication protocols";
    "-to_lustre", Arg.Set to_lustre, "translate to a lustre program";
    "-tracing", Arg.Set_string tracing, "output trace format (allowed values: no, values, instances, lttng-values, lttng-instances)";
    "-version", Arg.Unit (fun () -> print_endline version), " Display the version";]

type trace_fmt_type =
    No
  | Values
  | Instances
  | LTTngValues
  | LTTngInstances

let trace_fmt () =
  match !tracing with
  | "values" -> Values
  | "instances" -> Instances
  | "lttng-values" -> LTTngValues
  | "lttng-instances" -> LTTngInstances
  | "no" -> No
  | _ -> failwith ("Unknown trace format " ^ !tracing)

let trace_enabled () =
  trace_fmt () <> No
let trace_values () =
  trace_fmt () = Values || trace_fmt () = LTTngValues
let trace_instances () =
  trace_fmt () = Instances || trace_fmt () = LTTngInstances
let trace_printf () =
  trace_fmt () = Values || trace_fmt () = Instances
let trace_lttng () =
  trace_fmt () = LTTngValues || trace_fmt () = LTTngInstances
