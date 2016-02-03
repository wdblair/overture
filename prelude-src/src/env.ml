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

(** Generic inference environments. Used both for typing and
    clock-calculus. *)
open Utils

(* Same namespace for nodes, variables and constants *)
let initial = IMap.empty

let add_value env ident ty =
  IMap.add ident ty env

let lookup_value env ident =
  IMap.find ident env

let exists_value env ident =
  IMap.mem ident env

open Format

let pp env pp_fun =
  let (lid,lty) = list_of_imap env in
  let l' = List.combine lid lty in
  let pp_fun (id,value) =
    print_string id;
    print_string " |-> ";
    pp_fun value
  in
  pp_list l' pp_fun "" "" "\n"
