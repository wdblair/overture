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

(** Base types and predefined operator types. *)
open Types

let type_int = new_ty Tint
let type_float = new_ty Tfloat
let type_bool = new_ty Tbool
let type_clock = new_ty Tclock

let type_unary_bool_op =
  new_ty (Tarrow (type_bool, type_bool))

let type_bin_int_op =
  new_ty (Tarrow (new_ty (Ttuple [type_int;type_int]), type_int))

let type_bin_bool_op =
  new_ty (Tarrow (new_ty (Ttuple [type_bool;type_bool]), type_bool))

let type_bin_poly_op =
  let univ = new_univar () in
  new_ty (Tarrow ((new_ty (Ttuple [univ;univ])), univ))

let type_univ_bool_univ =
  let univ = new_univar () in
  new_ty (Tarrow ((new_ty (Ttuple [univ;type_bool])), univ))

let type_bool_univ3 =
  let univ = new_univar () in
  new_ty (Tarrow ((new_ty (Ttuple [type_bool;univ;univ])), univ))

let env = Env.initial
