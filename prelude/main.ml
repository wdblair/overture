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

let usage = "Usage: preludec [options] <source-file>"

let extension = ".plu"

let compile basename =
  let source_name = basename^extension in
  Location.input_name := source_name;
  let lexbuf = Lexing.from_channel (open_in source_name) in
  Location.init lexbuf source_name;
  (* Parsing *)
  let prog =
    try
      Parse.prog lexbuf
    with (Lexer.Error loc) | (Parse.Syntax_err loc) as exc ->
        Parse.report_error loc;
        raise exc
  in
  (* Typing *)
  begin
    try
      Typing.type_prog Type_predef.env prog
    with (Types.Error (loc,err)) as exc ->
      Location.print loc;
      Types.pp_error err;
      raise exc
  end;
  if !Options.print_types then
    Corelang.pp_prog_type prog;
  if !Options.print_ats then
    Corelang.pp_prog_ats prog;
  (* Clock calculus *)
  begin
    try
      Clock_calculus.clock_prog Clock_predef.env prog
    with (Clocks.Error (loc,err)) as exc ->
      Location.print loc;
      Clocks.pp_error err;
      raise exc
  end;
  if !Options.print_clocks then
    Corelang.pp_prog_clock prog
  (** 
    We're not really worried about compilation now, just the clocks.
  *)


let _ =
  try
    Arg.parse Options.options anonymous usage
  with
  | Parse.Syntax_err _ | Lexer.Error _ | Types.Error (_,_) | Clocks.Error (_,_)
  | Corelang.Error _ | Task_set.Error _ | Causality_analysis.Cycle _
  | Task_graph.Error _ -> ()
  | Corelang.Unbound_type (ty,loc) -> 
    Location.print loc;
    fprintf err_formatter "@[Unbound type %s@." ty
  | exc -> raise exc
