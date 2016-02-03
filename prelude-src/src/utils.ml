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

type rat = int*int
type ident = string
type tag = int
type longident = (string * tag) list

(** General utility functions. *)
let create_hashtable size init =
  let tbl = Hashtbl.create size in
  List.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
  tbl

module OrdIdent:Map.OrderedType with type t=ident =
  struct type t = ident;; let compare = compare end
module IMap = Map.Make(OrdIdent)

(* Warning: bad complexity *)
let list_of_imap imap =
  IMap.fold (fun i v (il,vl) -> (i::il,v::vl)) imap ([],[])

(** [gcd a b] returns the greatest common divisor of [a] and [b]. *)
let rec gcd a b =
  if b = 0 then a
  else gcd b (a mod b)

(** [lcm a b] returns the least common multiple of [a] and [b]. *)
let lcm a b =
  if a = 0 && b = 0 then
    0
  else a*b/(gcd a b)

(** [sum_rat (a,b) (a',b')] returns the sum of rationals [(a,b)] and
    [(a',b')] *)
let sum_rat (a,b) (a',b') =
  if a = 0 && b = 0 then
    (a',b')
  else if a'=0 && b'=0 then
    (a,b)
  else
    let lcm_bb' = lcm b b' in
    (a*lcm_bb'/b+a'*lcm_bb'/b',lcm_bb')

let simplify_rat (a,b) =
  let gcd = gcd a b in
  if (gcd =0) then
    (a,b)
  else (a/gcd,b/gcd)

let max_rat (a,b) (a',b') =
  let ratio_ab = (float_of_int a)/.(float_of_int b) in
  let ratio_ab' = (float_of_int a')/.(float_of_int b') in
  if ratio_ab > ratio_ab' then
    (a,b)
  else
    (a',b')

(** [list_union l1 l2] returns the union of list [l1] and [l2]. The
    result contains no duplicates. *)
let list_union l1 l2 =
  let rec aux l acc =
    match l with
    | [] -> acc
    | x::tl ->
        if List.mem x acc then
          aux tl acc
        else
          aux tl (x::acc)
  in
  let l1' = aux l1 [] in
  aux l2 l1'

(** [hashtbl_add h1 h2] adds all the bindings in [h2] to [h1]. If the
    intersection is not empty, it replaces the former binding *)
let hashtbl_add h1 h2 =
  Hashtbl.iter (fun key value -> Hashtbl.replace h1 key value) h2

let hashtbl_iterlast h f1 f2 =
  let l = Hashtbl.length h in
  ignore(
  Hashtbl.fold
    (fun k v cpt ->
      if cpt = l then
        begin f2 k v; cpt+1 end
      else
        begin f1 k v; cpt+1 end)
    h 1)

(** Match types variables to 'a, 'b, ..., for pretty-printing. Type
    variables are identified by integers. *)
let tnames = ref ([]: (int * string) list)
let tname_counter = ref 0
(* Same for carriers *)
let crnames = ref ([]: (int*int) list)
let crname_counter = ref 0

let reset_names () =
  tnames := []; tname_counter := 0; crnames := []; crname_counter := 0

(* From OCaml compiler *)
let new_tname () =
  let tname =
    if !tname_counter < 26
    then String.make 1 (Char.chr(97 + !tname_counter))
    else String.make 1 (Char.chr(97 + !tname_counter mod 26)) ^
      string_of_int(!tname_counter / 26) in
  incr tname_counter;
  tname

let new_crname () =
  incr crname_counter;
  !crname_counter-1

let name_of_type id =
  try List.assoc id !tnames with Not_found ->
    let name = new_tname () in
    tnames := (id, name) :: !tnames;
    name

let name_of_carrier id =
  let pp_id =
    try List.assoc id !crnames with Not_found ->
      let name = new_crname () in
      crnames := (id,name) :: !crnames;
      name
  in
  "_c"^(string_of_int pp_id)

open Format

let print_rat (a,b) =
  if b=1 then
    print_int a
  else
    if b < 0 then
      begin
        print_int (-a);
        print_string "/";
        print_int (-b)
      end
    else
      begin
        print_int a;
        print_string "/";
        print_int b
      end

(* Generic list pretty printing *)
let pp_list l pp_fun beg_str end_str sep_str =
  if (beg_str="\n") then
    print_newline ()
  else
    print_string beg_str;
  let rec pp_l l =
    match l with
    | [] -> ()
    | [hd] -> 
        pp_fun hd
    | hd::tl ->
        pp_fun hd;
        if (sep_str="\n") then
          print_newline ()
        else
          print_string sep_str;
        pp_l tl
  in
  pp_l l;
  if (end_str="\n") then
    print_newline ()
  else
    print_string end_str

let pp_array a pp_fun beg_str end_str sep_str =
  if (beg_str="\n") then
    print_newline ()
  else
    print_string beg_str;
  let n = Array.length a in
  if n > 0 then
    begin
      Array.iter (fun x -> pp_fun x; print_string sep_str) (Array.sub a 0 (n-1));
      pp_fun a.(n-1)
    end;
  if (end_str="\n") then
    print_newline ()
  else
    print_string end_str

let pp_hashtbl t pp_fun beg_str end_str sep_str =
  if (beg_str="\n") then
    print_newline ()
  else
    print_string beg_str;
  let pp_fun1 k v =
    pp_fun k v;
    if (sep_str="\n") then
      print_newline ()
    else
      print_string sep_str
  in
  hashtbl_iterlast t pp_fun1 pp_fun;
  if (end_str="\n") then
    print_newline ()
  else
    print_string end_str

let pp_longident lid =
  let pp_fun (nid, tag) =
    print_string nid;
    print_string "(";
    print_int tag;
    print_string ")"
  in
  pp_list lid pp_fun "" "." "."  
