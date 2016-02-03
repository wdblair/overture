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

(** Types definitions and a few utility functions on types. *)
open Utils

type type_expr =
    {mutable tdesc: type_desc;
     tid: int}

and type_desc =
    Tvar (* Monomorphic type variable *)
  | Tint
  | Tfloat
  | Tbool
  | Trat (* Actually unused for now. Only place where it can appear is
            in a clock declaration *)
  | Tclock (* An expression explicitely tagged as carrying a clock *)
  | Tarrow of type_expr * type_expr
  | Ttuple of type_expr list
  | Tlink of type_expr (* During unification, make links instead of substitutions *)
  | Tunivar (* Polymorphic type variable *)

type error =
    Unbound_value of ident
  | Already_bound of ident
  | Already_defined of ident
  | Undefined_var of (unit IMap.t)
  | Type_clash of type_expr * type_expr
  | Poly_imported_node of ident

exception Unify of type_expr * type_expr
exception Error of Location.t * error

let new_id = ref (-1)

let new_ty desc =
  incr new_id; {tdesc = desc; tid = !new_id }

let new_var () =
  new_ty Tvar

let new_univar () =
  new_ty Tunivar

let rec repr =
  function
    {tdesc = Tlink t'} ->
      repr t'
  | t -> t

(** Splits [ty] into the [lhs,rhs] of an arrow type. Expects an arrow type
    (ensured by language syntax) *)
let split_arrow ty =
  match (repr ty).tdesc with
  | Tarrow (tin,tout) -> tin,tout
    (* Functions are not first order, I don't think the var case
       needs to be considered here *)
  | _ -> failwith "Internal error: not an arrow type"

(** Returns the type corresponding to a type list. *)
let type_of_type_list tyl =
  if (List.length tyl) > 1 then
    new_ty (Ttuple tyl)
  else
    List.hd tyl

(** [is_polymorphic ty] returns true if [ty] is polymorphic. *)
let rec is_polymorphic ty =
  match ty.tdesc with
  | Tvar | Tint | Tfloat | Tbool | Trat | Tclock -> false
  | Tarrow (ty1,ty2) -> (is_polymorphic ty1) || (is_polymorphic ty2)
  | Ttuple tl -> List.exists (fun t -> is_polymorphic t) tl
  | Tlink t' -> is_polymorphic t'
  | Tunivar -> true

(* Pretty-print*)
open Format
  
let rec print_ty ty =
  match ty.tdesc with
  | Tvar ->
      print_string "'_";
      print_string (name_of_type ty.tid)
  | Tint ->
      print_string "int"
  | Tfloat ->
      print_string "float"
  | Tbool ->
      print_string "bool"
  | Tclock ->
      print_string "clock"
  | Trat ->
      print_string "rat"
  | Tarrow (ty1,ty2) ->
      print_ty ty1;
      print_string "->";
      print_ty ty2
  | Ttuple tylist ->
      Utils.pp_list tylist print_ty "(" ")" "*"
  | Tlink ty ->
      print_ty ty
  | Tunivar ->
      print_string "'";
      print_string (name_of_type ty.tid)

let pp_error = function
  | Unbound_value id ->
      print_string "Unknown value ";
      print_string id;
      print_newline ()
  | Already_bound id ->
      print_string "\"";
      print_string id;
      print_string "\" is already declared";
      print_newline ()
  | Already_defined id ->
      print_string "Multiple definitions of variable ";
      print_string id;
      print_newline ()
  | Undefined_var vmap ->
      print_string "No definition provided for variable ";
      Utils.pp_list (fst (Utils.list_of_imap vmap)) print_string "" "" ",";
      print_newline ()
  | Type_clash (ty1,ty2) ->
      Utils.reset_names ();
      print_string "Expected type ";
      print_ty ty1;
      print_string ", got type ";
      print_ty ty2;
      print_newline ()
  | Poly_imported_node id ->
      print_string "Imported nodes cannot have a polymorphic type";
      print_newline ()

