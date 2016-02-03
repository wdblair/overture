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

(** Clocks definitions and a few utility functions on clocks. *)
open Utils
open Format

(* Clock type sets, for subtyping. *)
type clock_set =
    CSet_all
  | CSet_pck of int*rat

(* Clock carriers basically correspond to the "c" in "x when c" *)
type carrier_desc =
  | Carry_name
  | Carry_var
  | Carry_link of carrier_expr

(* Carriers are scoped, to detect clock extrusion. In other words, we
   check the scope of a clock carrier before generalizing it. *)
and carrier_expr =
    {mutable carrier_desc: carrier_desc;
     mutable carrier_scoped: bool;
     carrier_id: int}

type clock_expr =
    {mutable cdesc: clock_desc;
     mutable cscoped: bool;
     cid: int}

(* pck stands for periodic clock. Easier not to separate pck from other clocks *)
and clock_desc =
  | Carrow of clock_expr * clock_expr
  | Ctuple of clock_expr list
  | Con of clock_expr * carrier_expr 
  | Connot of clock_expr * carrier_expr
  | Pck_up of clock_expr * int
  | Pck_down of clock_expr * int
  | Pck_phase of clock_expr * rat
  | Pck_const of int * rat
  | Cvar of clock_set (* Monomorphic clock variable *)
  | Cunivar of clock_set (* Polymorphic clock variable *)
  | Clink of clock_expr (* During unification, make links instead of substitutions *)
  | Ccarrying of carrier_expr * clock_expr

type error =
  | Clock_clash of clock_expr * clock_expr
  | Not_pck
  | Clock_set_mismatch of clock_expr * clock_set
  | Cannot_be_polymorphic of clock_expr
  | Invalid_imported_clock of clock_expr
  | Invalid_const of clock_expr
  | Factor_zero
  | Carrier_extrusion of clock_expr * carrier_expr
  | Clock_extrusion of clock_expr * clock_expr

exception Unify of clock_expr * clock_expr
exception Subsume of clock_expr * clock_set
exception Scope_carrier of carrier_expr
exception Scope_clock of clock_expr
exception Error of Location.t * error

let new_id = ref (-1)

let new_ck desc scoped =
  incr new_id; {cdesc=desc; cid = !new_id; cscoped = scoped}

let new_var scoped =
  new_ck (Cvar CSet_all) scoped

let new_univar () =
  new_ck (Cunivar CSet_all) false

let new_carrier_id = ref (-1)

let new_carrier desc scoped =
  incr new_carrier_id; {carrier_desc = desc;
                        carrier_id = !new_carrier_id;
                        carrier_scoped = scoped}

let new_carrier_name () =
  new_carrier Carry_name true

let rec repr =
  function
      {cdesc=Clink ck'} ->
        repr ck'
    | ck -> ck

let rec carrier_repr =
  function {carrier_desc = Carry_link cr'} -> carrier_repr cr'
    | cr -> cr

(* Removes all links in a clock. Only used for clocks
   simplification though. *)
let rec deep_repr ck =
  match ck.cdesc with
  | Carrow (ck1,ck2) ->
      new_ck (Carrow (deep_repr ck1, deep_repr ck2)) ck.cscoped
  | Ctuple cl ->
      new_ck (Ctuple (List.map deep_repr cl)) ck.cscoped
  | Con (ck',c) ->
      new_ck (Con (deep_repr ck',c)) ck.cscoped
  | Connot (ck',c) ->
      new_ck (Connot (deep_repr ck',c)) ck.cscoped
  | Pck_up (ck',k) ->
      new_ck (Pck_up (deep_repr ck',k)) ck.cscoped
  | Pck_down (ck',k) ->
      new_ck (Pck_down (deep_repr ck',k)) ck.cscoped
  | Pck_phase (ck',q) ->
      new_ck (Pck_phase (deep_repr ck',q)) ck.cscoped
  | Pck_const (_,_) | Cvar _ | Cunivar _ -> ck
  | Clink ck' -> deep_repr ck'
  | Ccarrying (cr,ck') -> new_ck (Ccarrying (cr, deep_repr ck')) ck.cscoped

(** Splits ck into the [lhs,rhs] of an arrow clock. Expects an arrow clock
    (ensured by language syntax) *)
let split_arrow ck =
  match (repr ck).cdesc with
  | Carrow (cin,cout) -> cin,cout
  | _ -> failwith "Internal error: not an arrow clock"

(** Returns the clock corresponding to a clock list. *)
let clock_of_clock_list ckl =
  if (List.length ckl) > 1 then
    new_ck (Ctuple ckl) true
  else
    List.hd ckl

let clock_of_impnode_clock ck =
  let ck = repr ck in
  match ck.cdesc with
  | Carrow _ | Clink _ | Cvar _ | Cunivar _ ->
      failwith "internal error clock_of_impnode_clock"
  | Ctuple cklist -> List.hd cklist
  | Con (_,_) | Connot (_,_) | Pck_up (_,_) | Pck_down (_,_) | Pck_phase (_,_)
  | Pck_const (_,_) | Ccarrying (_,_) -> ck

(** [intersect set1 set2] returns the intersection of clock subsets
    [set1] and [set2]. *)
let intersect set1 set2 =
  match set1,set2 with
  | CSet_all,_ -> set2
  | _,CSet_all -> set1
  | CSet_pck (k,q), CSet_pck (k',q') ->
      let k'',q'' = lcm k k',max_rat q q' in
      CSet_pck (k'',q'')

(** [can_be_pck ck] returns true if [ck] "may be" a pclock (the uncertainty is
    due to clock variables) *)
let rec can_be_pck ck =
  match (repr ck).cdesc with
  | Pck_up (_,_) | Pck_down (_,_) | Pck_phase (_,_) | Pck_const (_,_)
  | Cvar _ | Cunivar _ ->
      true
  | Ccarrying (_,ck') -> can_be_pck ck
  | _ -> false

(** [is_concrete_pck ck] returns true if [ck] is a concrete [pck] (pck
    transformations applied to a pck constant) *)
let rec is_concrete_pck ck =
  match ck.cdesc with
  | Carrow (_,_) | Ctuple _ | Con (_,_) | Connot (_,_)
  | Cvar _ | Cunivar _ -> false
  | Pck_up (ck',_) | Pck_down (ck',_) -> is_concrete_pck ck'
  | Pck_phase (ck',_) -> is_concrete_pck ck'
  | Pck_const (_,_) -> true
  | Clink ck' -> is_concrete_pck ck'
  | Ccarrying (_,ck') -> false

(** [is_polymorphic ck] returns true if [ck] is polymorphic. *)
let rec is_polymorphic ck =
  match ck.cdesc with
  | Cvar _ | Pck_const (_,_) -> false
  | Carrow (ck1,ck2) -> (is_polymorphic ck1) || (is_polymorphic ck2)
  | Ctuple ckl -> List.exists (fun c -> is_polymorphic c) ckl
  | Con (ck',_) | Connot (ck',_) -> is_polymorphic ck'
  | Pck_up (ck',_) | Pck_down (ck',_) -> is_polymorphic ck'
  | Pck_phase (ck',_) -> is_polymorphic ck'
  | Cunivar _ -> true
  | Clink ck' -> is_polymorphic ck'
  | Ccarrying (_,ck') -> is_polymorphic ck

(** [constrained_vars_of_clock ck] returns the clock variables subject
    to sub-typing constraints appearing in clock [ck]. Removes duplicates *)
(* Used mainly for debug, non-linear complexity. *)
let rec constrained_vars_of_clock ck =
  let rec aux vars ck =
    match ck.cdesc with
    | Pck_const (_,_) ->
        vars
    | Cvar cset ->
        begin
          match cset with
          | CSet_all -> vars
          | _ ->
              list_union [ck] vars
        end
    | Carrow (ck1,ck2) ->
        let l = aux vars ck1 in
        aux l ck2
    | Ctuple ckl ->
        List.fold_left
          (fun acc ck' -> aux acc ck') 
          vars ckl
    | Con (ck',_) | Connot (ck',_) -> aux vars ck'
    | Pck_up (ck',_) | Pck_down (ck',_) -> aux vars ck'
    | Pck_phase (ck',_) -> aux vars ck'
    | Cunivar cset ->
        begin
          match cset with
          | CSet_all -> vars
          | _ -> list_union [ck] vars
        end
    | Clink ck' -> aux vars ck'
    | Ccarrying (_,ck') -> aux vars ck'
  in
  aux [] ck

let print_ckset s =
  match s with
  | CSet_all -> ()
  | CSet_pck (k,q) ->
      let (a,b) = simplify_rat q in
      if k = 1 && a = 0 then
        print_string "<:P"
      else
        begin
          print_string "<:P_(";
          print_int k;
          print_string ",";
          print_rat (a,b);
          print_string ")"
        end

let rec print_carrier cr =
  match cr.carrier_desc with
  | Carry_name ->
      print_string ("?"^(name_of_carrier cr.carrier_id))
  | Carry_var ->
      print_string ("_"^(name_of_carrier cr.carrier_id))
  | Carry_link cr' ->
      print_carrier cr'

(* Simple pretty-printing, performs no simplifications. Linear
   complexity. For debug mainly. *)
let rec print_ck_long ck =
  match ck.cdesc with
  | Carrow (ck1,ck2) ->
      print_ck_long ck1;
      print_string "->";
      print_ck_long ck2
  | Ctuple cklist ->
      pp_list cklist print_ck_long "(" ")" " * "
  | Con (ck,c) ->
      print_ck_long ck;
      print_string " on ";
      print_carrier c
  | Connot (ck,c) ->
      print_ck_long ck;
      print_string " on not ";
      print_carrier c
  | Pck_up (ck,k) ->
      print_ck_long ck;
      print_string "*^";
      print_int k
  | Pck_down (ck,k) ->
      print_ck_long ck;
      print_string "/^";
      print_int k
  | Pck_phase (ck,q) ->
      print_ck_long ck;
      print_string "~>";
      print_rat (simplify_rat q)
  | Pck_const (n,p) ->
      print_string "(";
      print_int n;
      print_string ",";
      print_rat (simplify_rat p);
      print_string ")"
  | Cvar cset ->
      print_string "'_";
      print_int ck.cid;
      print_ckset cset
  | Cunivar cset ->
      print_string "'";
      print_int ck.cid;
      print_ckset cset
  | Clink ck' ->
      print_string "link ";
      print_ck_long ck'
  | Ccarrying (cr,ck') ->
      print_carrier cr;
      print_string ":";
      print_ck_long ck'

(** [period ck] returns the period of [ck]. Expects a constant pclock
    expression belonging to the correct clock set. *)
let rec period ck =
  let rec aux ck =
    match ck.cdesc with
    | Carrow (_,_) | Ctuple _ | Con (_,_) | Connot (_,_)
    | Cvar _ | Cunivar _ ->
        failwith "internal error: can only compute period of const pck"
    | Pck_up (ck',k) ->
        (aux ck')/.(float_of_int k)
    | Pck_down (ck',k) ->
        (float_of_int k)*.(aux ck')
    | Pck_phase (ck',_) ->
        aux ck'
    | Pck_const (n,_) ->
        float_of_int n
    | Clink ck' -> aux ck'
    | Ccarrying (_,ck') -> aux ck'
  in
  int_of_float (aux ck)

(** [phase ck] returns the phase of [ck]. It is not expressed as a
    fraction of the period, but instead as an amount of time. Expects a
    constant expression belonging to the correct P_k *)
let phase ck =
  let rec aux ck =
  match ck.cdesc with
  | Carrow (_,_) | Ctuple _ | Con (_,_) | Connot (_,_)
  | Cvar _ | Cunivar _ ->
      failwith "internal error: can only compute phase of const pck"
  | Pck_up (ck',_) ->
      aux ck'
  | Pck_down (ck',k) ->
      aux ck'
  | Pck_phase (ck',(a,b)) ->
      let n = period ck' in
      let (a',b') = aux ck' in
      sum_rat (a',b') (n*a,b)
  | Pck_const (n,(a,b)) ->
      (n*a,b)
  | Clink ck' -> aux ck'
  | Ccarrying (_,ck') -> aux ck'
  in
  let (a,b) = aux ck in
  simplify_rat (a,b)
    
(* Returns the pck clock parent of a clock *)
let rec pclock_parent ck =
  match (repr ck).cdesc with
  | Con (ck',_) | Connot (ck',_) | Clink ck' | Ccarrying (_,ck') ->
      pclock_parent ck'
  | Pck_up _ | Pck_down _ | Pck_phase _ | Pck_const _ | Cvar _ | Cunivar _ -> ck
  | Carrow _ | Ctuple _ -> failwith "Internal error pclock_parent"

(** [normalize pck] returns the normal form of clock [pck]. *)
let normalize pck =
  let changed = ref true in
  let rec aux pck =
    match pck.cdesc with
    | Pck_up ({cdesc=Pck_up (pck',k')},k) ->
        changed:=true;
        new_ck (Pck_up (aux pck',k*k')) pck.cscoped
    | Pck_up ({cdesc=Pck_down (pck',k')},k) ->
        changed:=true;
        new_ck (Pck_down (new_ck (Pck_up (aux pck',k)) pck.cscoped,k')) pck.cscoped
    | Pck_up ({cdesc=Pck_phase (pck',(a,b))},k) ->
        changed:=true;
        new_ck (Pck_phase (new_ck (Pck_up (aux pck',k)) pck.cscoped,(a*k,b))) pck.cscoped
    | Pck_down ({cdesc=Pck_down (pck',k')},k) ->
        changed:=true;
        new_ck (Pck_down (aux pck',k*k')) pck.cscoped
    | Pck_down ({cdesc=Pck_phase (pck',(a,b))},k) ->
        changed:=true;
        new_ck (Pck_phase (new_ck (Pck_down (aux pck',k)) pck.cscoped,(a,b*k))) pck.cscoped
    | Pck_phase ({cdesc=Pck_phase (pck',(a',b'))},(a,b)) ->
        changed:=true;
        let (a'',b'') = sum_rat (a,b) (a',b') in
        new_ck (Pck_phase (aux pck',(a'',b''))) pck.cscoped
    | Pck_up (pck',k') ->
        new_ck (Pck_up (aux pck',k')) pck.cscoped
    | Pck_down (pck',k') ->
        new_ck (Pck_down (aux pck',k')) pck.cscoped
    | Pck_phase (pck',k') ->
        new_ck (Pck_phase (aux pck',k')) pck.cscoped
    | Ccarrying (cr,ck') ->
        new_ck (Ccarrying (cr, aux ck')) pck.cscoped
    | _ -> pck
  in
  let nf=ref pck in
  while !changed do
    changed:=false;
    nf:=aux !nf
  done;
  !nf

(** [canonize pck] reduces transformations of [pck] and removes
    identity transformations. Expects a normalized periodic clock ! *)
let canonize pck =
  let rec remove_id_trans pck =
    match pck.cdesc with
    | Pck_up (pck',1) | Pck_down (pck',1) | Pck_phase (pck',(0,_)) ->
        remove_id_trans pck'
    | _ -> pck
  in
  let pck =
    match pck.cdesc with
    | Pck_phase ({cdesc=Pck_down ({cdesc=Pck_up (v,k)},k')},k'') ->
        let gcd = gcd k k' in
        new_ck (Pck_phase
                  (new_ck (Pck_down
                             (new_ck (Pck_up (v,k/gcd)) pck.cscoped,k'/gcd))
                     pck.cscoped,k''))
          pck.cscoped
    | Pck_down ({cdesc=Pck_up (v,k)},k') ->
        let gcd = gcd k k' in
        new_ck (Pck_down (new_ck (Pck_up (v,k/gcd)) pck.cscoped,k'/gcd)) pck.cscoped
    | _ -> pck
  in
  remove_id_trans pck

(** [simplify pck] applies pclocks simplifications to [pck] *)
let simplify pck =
  if (is_concrete_pck pck) then
    let n = period pck in
    let (a,b) = phase pck in
    let phase' = simplify_rat (a,b*n) in 
    new_ck (Pck_const (n,phase')) pck.cscoped
  else
    let pck' = deep_repr pck in
    let nf_pck = normalize pck' in
    canonize nf_pck
        
let print_cvar cvar =
  match cvar.cdesc with
  | Cvar cset ->
      print_string "'_";
      print_string (Utils.name_of_type cvar.cid);
      print_ckset cset
  | Cunivar cset ->
      print_string "'";
      print_string (Utils.name_of_type cvar.cid);
      print_ckset cset
  | _ -> failwith "Internal error print_cvar"

(* Nice pretty-printing. Simplifies expressions before printing them. Non-linear
   complexity. *)
let print_ck ck =
  let rec aux ck =
    let ck = simplify ck in
    match ck.cdesc with
    | Carrow (ck1,ck2) ->
        aux ck1;
        print_string "->";
        aux ck2
    | Ctuple cklist ->
        pp_list cklist aux "(" ")" " * "
    | Con (ck,c) ->
        aux ck;
        print_string " on ";
        print_carrier c
    | Connot (ck,c) ->
        aux ck;
        print_string " on not ";
        print_carrier c
    | Pck_up (ck,k) ->
        aux ck;
        print_string "*.";
        print_int k
    | Pck_down (ck,k) ->
        aux ck;
        print_string "/.";
        print_int k
    | Pck_phase (ck,q) ->
        aux ck;
        print_string "->.";
        print_rat (simplify_rat q)
    | Pck_const (n,p) ->
        print_string "(";
        print_int n;
        print_string ",";
        print_rat (simplify_rat p);
        print_string ")"
    | Cvar cset ->
        print_string "'_";
        print_string (Utils.name_of_type ck.cid);
    | Cunivar cset ->
        print_string "'";
        print_string (Utils.name_of_type ck.cid)
    | Clink ck' ->
        aux ck'
    | Ccarrying (cr,ck') ->
        print_carrier cr;
        print_string ":";
        aux ck'
  in
  let cvars = constrained_vars_of_clock ck in
  aux ck;
  if cvars <> [] then
    pp_list cvars print_cvar " (where " ")" ", "

let pp_error = function
  | Clock_clash (ck1,ck2) ->
      Utils.reset_names ();
      print_string "Expected clock ";
      print_ck ck1;
      print_string ", got clock ";
      print_ck ck2;
      print_newline ()
  | Not_pck ->
      print_string "The clock of this expression must be periodic";
      print_newline ()
  | Clock_set_mismatch (ck,cset) ->
      Utils.reset_names ();
      print_string "Clock ";
      print_ck ck;
      print_string " is not included in clock set ";
      print_ckset cset;
      print_newline ()
  | Cannot_be_polymorphic ck ->
      Utils.reset_names ();
      print_string "The main node cannot have a polymorphic clock: ";
      print_ck ck;
      print_newline ();
  | Invalid_imported_clock ck ->
      Utils.reset_names ();
      print_string "Not a valid imported node clock: ";
      print_ck ck;
      print_newline ()
  | Invalid_const ck ->
      Utils.reset_names ();
      print_string "Clock ";
      print_ck ck;
      print_string " is not a valid periodic clock";
      print_newline ()
  | Factor_zero ->
      print_string "Cannot apply clock transformation with factor 0";
      print_newline ()
  | Carrier_extrusion (ck,cr) ->
      print_string "This node has clock";
      print_newline ();
      print_ck ck;
      print_newline ();
      print_string "It is invalid as ";
      print_carrier cr;
      print_string " escapes its scope.";
      print_newline ()
  | Clock_extrusion (ck_node,ck) ->
      print_string "This node has clock";
      print_newline ();
      print_ck ck_node;
      print_newline ();
      print_string "It is invalid as ";
      print_ck ck;
      print_string " escapes its scope.";
      print_newline ()
