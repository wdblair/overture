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

(** Main clock-calculus module. Based on type inference algorithms with
  destructive unification. Uses a bit of subtyping for periodic clocks. *)

(* Though it shares similarities with the typing module, no code
    is shared.  Simple environments, very limited identifier scoping, no
    identifier redefinition allowed. *)
open Utils
open Corelang
open Clocks
open Format

let loc_of_when_cond loc_containing id =
  let pos_start =
    {loc_containing.Location.loc_end with 
     Lexing.pos_cnum=loc_containing.Location.loc_end.Lexing.pos_cnum-(String.length id)}
  in
  {Location.loc_start = pos_start;
   Location.loc_end = loc_containing.Location.loc_end}

(** [occurs cvar ck] returns true if the clock variable [cvar] occurs in
    clock [ck]. False otherwise. *)
let rec occurs cvar ck =
  let ck = repr ck in
  match ck.cdesc with
  | Carrow (ck1, ck2) ->
      (occurs cvar ck1) || (occurs cvar ck2)
  | Ctuple ckl ->
      List.exists (occurs cvar) ckl
  | Con (ck',_) -> occurs cvar ck'
  | Connot (ck',_) -> occurs cvar ck'
  | Pck_up (ck',_) -> occurs cvar ck'
  | Pck_down (ck',_) -> occurs cvar ck'
  | Pck_phase (ck',_) -> occurs cvar ck'
  | Cvar _ -> ck=cvar
  | Cunivar _ | Pck_const (_,_) -> false
  | Clink ck' -> occurs cvar ck'
  | Ccarrying (_,ck') -> occurs cvar ck'

(* Clocks generalization *)
let rec generalize_carrier cr =
  match cr.carrier_desc with
  | Carry_name ->
      if cr.carrier_scoped then
        raise (Scope_carrier cr);
      cr.carrier_desc <- Carry_var
  | Carry_var -> ()
  | Carry_link cr' -> generalize_carrier cr'

(** Promote monomorphic clock variables to polymorphic clock variables. *)
(* Generalize by side-effects *)
let rec generalize ck =
    match ck.cdesc with
    | Carrow (ck1,ck2) ->
        generalize ck1; generalize ck2
    | Ctuple clist ->
        List.iter generalize clist
    | Con (ck',cr) -> generalize ck'; generalize_carrier cr
    | Connot (ck',cr) -> generalize ck'; generalize_carrier cr
    | Cvar cset ->
        if ck.cscoped then
          raise (Scope_clock ck);
        ck.cdesc <- Cunivar cset
    | Pck_up (ck',_) -> generalize ck'
    | Pck_down (ck',_) -> generalize ck'
    | Pck_phase (ck',_) -> generalize ck'
    | Pck_const (_,_) | Cunivar _ -> ()
    | Clink ck' ->
        generalize ck'
    | Ccarrying (cr,ck') ->
        generalize_carrier cr; generalize ck'

let try_generalize ck_node loc =
  try 
    generalize ck_node
  with (Scope_carrier cr) ->
    raise (Error (loc, Carrier_extrusion (ck_node, cr)))
  | (Scope_clock ck) ->
      raise (Error (loc, Clock_extrusion (ck_node, ck)))

(* Clocks instanciation *)
let instanciate_carrier cr inst_cr_vars =
  let cr = carrier_repr cr in
  match cr.carrier_desc with
  | Carry_name -> cr
  | Carry_link _ ->
      failwith "Internal error"
  | Carry_var ->
      try
        List.assoc cr.carrier_id !inst_cr_vars
      with Not_found ->            
        let cr_var = new_carrier Carry_name true in
        inst_cr_vars := (cr.carrier_id,cr_var)::!inst_cr_vars;
        cr_var

(** Downgrade polymorphic clock variables to monomorphic clock variables *)
(* inst_ck_vars ensures that a polymorphic variable is instanciated with
   the same monomorphic variable if it occurs several times in the same
   type. inst_cr_vars is the same for carriers. *)
let rec instanciate inst_ck_vars inst_cr_vars ck =
  match ck.cdesc with
  | Carrow (ck1,ck2) ->
      {ck with cdesc =
       Carrow ((instanciate inst_ck_vars inst_cr_vars ck1),
               (instanciate inst_ck_vars inst_cr_vars ck2))}
  | Ctuple cklist ->
      {ck with cdesc = Ctuple 
         (List.map (instanciate inst_ck_vars inst_cr_vars) cklist)}
  | Con (ck',c) ->
      let c' = instanciate_carrier c inst_cr_vars in
      {ck with cdesc = Con ((instanciate inst_ck_vars inst_cr_vars ck'),c')}
  | Connot (ck',c) ->
      let c' = instanciate_carrier c inst_cr_vars in
      {ck with cdesc = Connot ((instanciate inst_ck_vars inst_cr_vars ck'),c')}
  | Cvar _ | Pck_const (_,_) -> ck
  | Pck_up (ck',k) ->
      {ck with cdesc = Pck_up ((instanciate inst_ck_vars inst_cr_vars ck'),k)}
  | Pck_down (ck',k) ->
      {ck with cdesc = Pck_down ((instanciate inst_ck_vars inst_cr_vars ck'),k)}
  | Pck_phase (ck',q) ->
      {ck with cdesc = Pck_phase ((instanciate inst_ck_vars inst_cr_vars ck'),q)}
  | Clink ck' ->
      {ck with cdesc = Clink (instanciate inst_ck_vars inst_cr_vars ck')}
  | Ccarrying (cr,ck') ->
      let cr' = instanciate_carrier cr inst_cr_vars in
        {ck with cdesc =
         Ccarrying (cr',instanciate inst_ck_vars inst_cr_vars ck')}
  | Cunivar cset ->
      try
        List.assoc ck.cid !inst_ck_vars
      with Not_found ->
        let var = new_ck (Cvar cset) true in
	inst_ck_vars := (ck.cid, var)::!inst_ck_vars;
	var

(** [subsume pck1 cset1] subsumes clock [pck1] by clock subset
    [cset1]. The clock constraint is actually recursively transfered to
    the clock variable appearing in [pck1] *)
let subsume pck1 cset1 =
  let rec aux pck cset =
    match cset with
    | CSet_all ->
        ()
    | CSet_pck (k,(a,b)) ->
        match pck.cdesc with
        | Cvar cset' ->
            pck.cdesc <- Cvar (intersect cset' cset)
        | Pck_up (pck',k') ->
            let rat = if a=0 then (0,1) else (a,b*k') in
            aux pck' (CSet_pck ((k*k'),rat))
        | Pck_down (pck',k') ->
            let k''=k/(gcd k k') in
            aux pck' (CSet_pck (k'',(a*k',b)))
        | Pck_phase (pck',(a',b')) ->
            let (a'',b'')= max_rat (sum_rat (a,b) (-a',b')) (0,1) in
            aux pck' (CSet_pck (k, (a'',b'')))
        | Pck_const (n,(a',b')) ->
            if n mod k <> 0 || (max_rat (a,b) (a',b')) <> (a',b') then
              raise (Subsume (pck1, cset1))
        | Clink pck' ->
            aux pck' cset
        | Cunivar _ -> ()
        | Ccarrying (_,ck') ->
            aux ck' cset
        | _ -> raise (Subsume (pck1, cset1))
  in
  aux pck1 cset1

let rec update_scope_carrier scoped cr =
  if (not scoped) then
    begin
      cr.carrier_scoped <- scoped;
      match cr.carrier_desc with
      | Carry_name | Carry_var -> ()
      | Carry_link cr' -> update_scope_carrier scoped cr'
    end

let rec update_scope scoped ck =
  if (not scoped) then
    begin
      ck.cscoped <- scoped;
      match ck.cdesc with
      | Carrow (ck1,ck2) ->
          update_scope scoped ck1; update_scope scoped ck2
      | Ctuple clist ->
          List.iter (update_scope scoped) clist
      | Con (ck',cr) -> update_scope scoped ck'; update_scope_carrier scoped cr
      | Connot (ck',cr) -> update_scope scoped ck'; update_scope_carrier scoped cr
      | Cvar cset ->
          ck.cdesc <- Cvar cset
      | Pck_up (ck',_) -> update_scope scoped ck'
      | Pck_down (ck',_) -> update_scope scoped ck'
      | Pck_phase (ck',_) -> update_scope scoped ck'
      | Pck_const (_,_) | Cunivar _ -> ()
      | Clink ck' ->
          update_scope scoped ck'
      | Ccarrying (cr,ck') ->
          update_scope_carrier scoped cr; update_scope scoped ck'
    end

(* Unifies two static pclocks. *)
let unify_static_pck ck1 ck2 =
  if (period ck1 <> period ck2) || (phase ck1 <> phase ck2) then
    raise (Unify (ck1,ck2))

(* Unifies two clock carriers *)
let unify_carrier cr1 cr2 =
  let cr1 = carrier_repr cr1 in
  let cr2 = carrier_repr cr2 in
  if cr1=cr2 then ()
  else
    match cr1.carrier_desc, cr2.carrier_desc with
    | Carry_name,Carry_name ->
        if cr1.carrier_id < cr2.carrier_id then
          begin
            cr2.carrier_desc <- Carry_link cr1;
            update_scope_carrier cr2.carrier_scoped cr1
          end
        else
          begin
            cr1.carrier_desc <- Carry_link cr2;
            update_scope_carrier cr1.carrier_scoped cr2
          end
    | _,_ -> ()

(** [unify env ck1 ck2] unifies clocks [ck1] and [ck2]. Raises [Unify
    (ck1,ck2)] if the clocks are not unifiable.*)
let rec unify ck1 ck2 =
  let ck1 = repr ck1 in
  let ck2 = repr ck2 in
  if ck1=ck2 then
    ()
  else
    let left_const = is_concrete_pck ck1 in
    let right_const = is_concrete_pck ck2 in
    if left_const && right_const then
      unify_static_pck ck1 ck2
    else
      match ck1.cdesc,ck2.cdesc with
      | Cvar cset1,Cvar cset2->
          if ck1.cid < ck2.cid then
            begin
              ck2.cdesc <- Clink (simplify ck1);
              update_scope ck2.cscoped ck1;
              subsume ck1 cset2
            end
          else
            begin
              ck1.cdesc <- Clink (simplify ck2);
              update_scope ck1.cscoped ck2;
              subsume ck2 cset1
            end
      | Cvar cset, Pck_up (_,_) | Cvar cset, Pck_down (_,_)
      | Cvar cset, Pck_phase (_,_) | Cvar cset, Pck_const (_,_) ->
          if (occurs ck1 ck2) then
            begin
              if (simplify ck2 = ck1) then
                ck2.cdesc <- Clink (simplify ck1)
              else
                raise (Unify (ck1,ck2));
              end
          else
            begin
              ck1.cdesc <- Clink (simplify ck2);
              subsume ck2 cset
            end
      | Pck_up (_,_), Cvar cset | Pck_down (_,_), Cvar cset
      | Pck_phase (_,_), Cvar cset | Pck_const (_,_), Cvar cset ->
            if (occurs ck2 ck1) then
              begin
                if ((simplify ck1) = ck2) then
                  ck1.cdesc <- Clink (simplify ck2)
                else
                  raise (Unify (ck1,ck2));
              end
            else
              begin
                ck2.cdesc <- Clink (simplify ck1);
                subsume ck1 cset
              end
      | (Cvar cset,_) when (not (occurs ck1 ck2)) ->
          subsume ck2 cset;
          update_scope ck1.cscoped ck2;
          ck1.cdesc <- Clink (simplify ck2)
      | (_, (Cvar cset)) when (not (occurs ck2 ck1)) ->
          subsume ck1 cset;
          update_scope ck2.cscoped ck1;
          ck2.cdesc <- Clink (simplify ck1)
      | Ccarrying (cr1,ck1'),Ccarrying (cr2,ck2') ->
          unify_carrier cr1 cr2;
          unify ck1' ck2'
      | Ccarrying (_,_),_ | _,Ccarrying (_,_) ->
          raise (Unify (ck1,ck2))
      | Carrow (c1,c2), Carrow (c1',c2') ->
          unify c1 c1'; unify c2 c2'
      | Ctuple ckl1, Ctuple ckl2 ->
          if (List.length ckl1) <> (List.length ckl2) then
            raise (Unify (ck1,ck2));
          List.iter2 unify ckl1 ckl2
      | Con (ck',c1), Con (ck'',c2) ->
          unify_carrier c1 c2;
          unify ck' ck''
      | Connot (ck',c1), Connot (ck'',c2) ->
          unify_carrier c1 c2;
          unify ck' ck''
      | Pck_const (i,r), Pck_const (i',r') ->
          if i<>i' || r <> r' then
            raise (Unify (ck1,ck2))
      | (_, Pck_up (pck2',k)) when (not right_const) ->
          let ck1' = simplify (new_ck (Pck_down (ck1,k)) true) in
          unify ck1' pck2'
      | (_,Pck_down (pck2',k)) when (not right_const) ->
          subsume ck1 (CSet_pck (k,(0,1)));
          let ck1' = simplify (new_ck (Pck_up (ck1,k)) true) in
          unify ck1' pck2'
      | (_,Pck_phase (pck2',(a,b))) when (not right_const) ->
          subsume ck1 (CSet_pck (b,(a,b)));
          let ck1' = simplify (new_ck (Pck_phase (ck1,(-a,b))) true) in
          unify ck1' pck2'
      | Pck_up (pck1',k),_ ->
          let ck2' = simplify (new_ck (Pck_down (ck2,k)) true) in
          unify pck1' ck2'
      | Pck_down (pck1',k),_ ->
          subsume ck2 (CSet_pck (k,(0,1)));
          let ck2' = simplify (new_ck (Pck_up (ck2,k)) true) in
          unify pck1' ck2'
      | Pck_phase (pck1',(a,b)),_ ->
          subsume ck2 (CSet_pck (b,(a,b)));
          let ck2' = simplify (new_ck (Pck_phase (ck2,(-a,b))) true) in
          unify pck1' ck2'
      | Cunivar _, _ | _, Cunivar _ -> ()
      | _,_ -> raise (Unify (ck1,ck2))

(* Returns the value corresponding to a pclock (integer) factor
   expression. Expects a constant expression (checked by typing). *)
let int_factor_of_expr e =
  match e.expr_desc with 
  | Expr_const
      (Const_int i) -> i
  | _ -> failwith "Internal error
        int_factor_of_expr"

let rec clock_expect env expr ck =
  let cexpr = clock_expr env expr in
  try_unify env cexpr ck expr.expr_loc

and try_unify env ck1 ck2 loc =
  try
    unify ck1 ck2
  with
    Unify (ck1',ck2') ->
      raise (Error (loc, Clock_clash (ck1',ck2')))
  | Subsume (ck,cset) ->
      raise (Error (loc, Clock_set_mismatch (ck,cset)))

and clock_ident env id loc =
  clock_expr env (expr_of_ident id loc)

and clock_carrier env c loc ce =
  let expr_c = expr_of_ident c loc in
  let ck = clock_expr env expr_c in
  let cr = new_carrier Carry_name ck.cscoped in
  let ckcarry = new_ck (Ccarrying (cr,ce)) ck.cscoped in
  try_unify env ck ckcarry expr_c.expr_loc;
  cr

(** [clock_expr env expr] performs the clock calculus for expression [expr] in
    environement [env] *)
and clock_expr env expr =
  match expr.expr_desc with
  | Expr_const cst ->
      let ck = new_var true in
      expr.expr_clock <- ck;
      ck
  | Expr_ident v ->
      let ckv =
        try
          Env.lookup_value env v
        with Not_found -> failwith ("Internal error, "^v^" not found")
      in
      let ck = instanciate (ref []) (ref []) ckv in
      expr.expr_clock <- ck;
      ck
  | Expr_tuple elist ->
      let ck = new_ck (Ctuple (List.map (clock_expr env) elist)) true in
      expr.expr_clock <- ck;
      ck
  | Expr_appl (id, args) ->
      let cfun = clock_ident env id expr.expr_loc in
      let cins,couts = split_arrow cfun in
      clock_expect env args cins;
      expr.expr_clock <- couts;
      couts
  | Expr_fby (_,e) ->
      let ck = clock_expr env e in
      expr.expr_clock <- ck;
      ck
  | Expr_concat (_,e) ->
      let pck = clock_expr env e in
      if not (can_be_pck pck) then
        raise (Error (e.expr_loc, Not_pck));
      let cset = CSet_pck (1,(1,1)) in
      (try
        subsume pck cset 
      with Subsume (ck,cset) ->
        raise (Error (e.expr_loc, Clock_set_mismatch (ck,cset))));
      let ck = new_ck (Pck_phase (pck,(-1,1))) true in
      expr.expr_clock <- ck;
      ck
  | Expr_tail e ->
      let pck = clock_expr env e in
      if not (can_be_pck pck) then
        raise (Error (e.expr_loc, Not_pck));
      let ck = new_ck (Pck_phase (pck,(1,1))) true in
      expr.expr_clock <- ck;
      ck
  | Expr_when (e,c) ->
      let ce = clock_expr env e in
      let c_loc = loc_of_when_cond expr.expr_loc c in
      let cr = clock_carrier env c c_loc ce in
      let ck = new_ck (Con (ce,cr)) true in
      expr.expr_clock <- ck;
      ck
  | Expr_whennot (e,c) ->
      let ce = clock_expr env e in
      let c_loc = loc_of_when_cond expr.expr_loc c in
      let cr = clock_carrier env c c_loc ce in
      let ck = new_ck (Connot (ce,cr)) true in
      expr.expr_clock <- ck;
      ck
  | Expr_merge (c,e1,e2) ->
      let cvar = new_var true in
      let cr = clock_carrier env c expr.expr_loc cvar in
      clock_expect env e1 (new_ck (Con (cvar,cr)) true);
      clock_expect env e2 (new_ck (Connot (cvar,cr)) true);
      expr.expr_clock <- cvar;
      cvar
  | Expr_uclock (e,k) ->
      let pck = clock_expr env e in
      if not (can_be_pck pck) then
        raise (Error (e.expr_loc, Not_pck));
      if k = 0 then
        raise (Error (expr.expr_loc, Factor_zero));
      (try
        subsume pck (CSet_pck (k,(0,1)))
      with Subsume (ck,cset) ->
        raise (Error (e.expr_loc, Clock_set_mismatch (ck,CSet_pck (k,(0,1))))));
      let ck = new_ck (Pck_up (pck,k)) true in
      expr.expr_clock <- ck;
      ck
  | Expr_dclock (e,k) ->
      let pck = clock_expr env e in
      if not (can_be_pck pck) then
        raise (Error (e.expr_loc, Not_pck));
      if k = 0 then
        raise (Error (expr.expr_loc, Factor_zero));
      (try
        subsume pck (CSet_pck (1,(0,1)))
      with Subsume (ck,cset) ->
        raise (Error (e.expr_loc, Clock_set_mismatch (ck,CSet_pck (1,(0,1))))));
      let ck = new_ck (Pck_down (pck,k)) true in
      expr.expr_clock <- ck;
      ck
  | Expr_phclock (e,(a,b)) ->
      let pck = clock_expr env e in
      if not (can_be_pck pck) then
        raise (Error (e.expr_loc, Not_pck));
      let (a,b) = simplify_rat (a,b) in
      (try
        subsume pck (CSet_pck (b,(0,1)))
      with Subsume (ck,cset) ->
        raise (Error (e.expr_loc, Clock_set_mismatch (ck,CSet_pck (b,(0,1))))));
      let ck = new_ck (Pck_phase (pck,(a,b))) true in
      expr.expr_clock <- ck;
      ck

let clock_of_vlist vars =
  let ckl = List.map (fun v -> v.var_clock) vars in
  clock_of_clock_list ckl

(** [clock_eq env eq] performs the clock-calculus for equation [eq] in
    environment [env] *)
let clock_eq env eq =
  (* Clock lhs *)
  let ckl_lhs = List.map (fun v ->
    try
      Env.lookup_value env v
    with Not_found -> failwith ("Internal error, "^v^" not found")) eq.eq_lhs in
  let ck_lhs = clock_of_clock_list ckl_lhs in 
  (* Clock rhs *)
  clock_expect env eq.eq_rhs ck_lhs

(* [clock_coreclock cck] returns the clock_expr corresponding to clock
    declaration [cck] *)
let clock_coreclock env cck id loc scoped =
  match cck.ck_dec_desc with
  | Ckdec_any -> new_var scoped
  | Ckdec_pclock (n,(a,b)) ->
      let ck = new_ck (Pck_const (n,(a,b))) scoped in
      if n mod b <> 0 then raise (Error (loc,Invalid_const ck));
      ck
  | Ckdec_bool cl ->
      let temp_env = Env.add_value env id (new_var true) in
      (* We just want the id to be present in the environment *)
      let dummy_id_expr = expr_of_ident id loc in
      let when_expr =
        List.fold_left
          (fun expr c ->
            match c with
            | Wtrue id ->
                {expr_tag = new_tag ();
                 expr_desc = Expr_when (expr,id);
                 expr_type = Types.new_var ();
                 expr_clock = new_var scoped;
                 expr_loc = loc}
            | Wfalse id ->
                {expr_tag = new_tag ();
                 expr_desc=Expr_whennot (expr,id);
                 expr_type=Types.new_var ();
                 expr_clock = new_var scoped;
                 expr_loc=loc})
          dummy_id_expr cl
      in
      clock_expr temp_env when_expr

(* Clocks a variable declaration *)
let clock_var_decl scoped env vdecl =
  let ck = clock_coreclock env vdecl.var_dec_clock vdecl.var_id vdecl.var_loc scoped in
  let ck =
    if vdecl.var_type.Types.tdesc =  Types.Tclock then
      new_ck (Ccarrying ((new_carrier Carry_name scoped),ck)) scoped
    else
      ck
  in
  vdecl.var_clock <- ck;
  Env.add_value env vdecl.var_id ck

(* Clocks a variable declaration list *)
let clock_var_decl_list env scoped l =
  List.fold_left (clock_var_decl scoped) env l

(** [clock_node env nd] performs the clock-calculus for node [nd] in
    environment [env]. *)
let clock_node env loc nd =
  let is_main = nd.node_id = !Options.main_node in
  let new_env = clock_var_decl_list env false nd.node_inputs in
  let new_env = clock_var_decl_list new_env true nd.node_outputs in
  let new_env = clock_var_decl_list new_env true nd.node_locals in
  List.iter (clock_eq new_env) nd.node_eqs;
  let ck_ins = clock_of_vlist nd.node_inputs in
  let ck_outs = clock_of_vlist nd.node_outputs in
  let ck_node = new_ck (Carrow (ck_ins,ck_outs)) false in
  try_generalize ck_node loc;
  if (is_main && is_polymorphic ck_node) then
    raise (Error (loc,(Cannot_be_polymorphic ck_node)));
  nd.node_clock <- ck_node;
  Env.add_value env nd.node_id ck_node

(* Unifies all the clock variables in the clock type of an imported
   node, so that the clock type only uses at most one clock variable *)
let unify_imported_clock ck =
  let ck_var = ref None in
  let rec aux ck =
    match (repr ck).cdesc with
    | Cvar _ ->
        begin
          match !ck_var with
          | None ->
              ck_var:=Some ck
          | Some v ->
              (* cannot fail *)
              unify v ck
        end
    | Ctuple cl ->
        List.iter aux cl
    | Carrow (ck1,ck2) ->
        aux ck1; aux ck2
    | _ -> ()
  in
  aux ck

let check_imported_pclocks loc ck_node =
  let pck = ref None in
  let rec aux ck =
    match ck.cdesc with
    | Carrow (ck1,ck2) -> aux ck1; aux ck2
    | Ctuple cl -> List.iter aux cl
    | Con (ck',_) | Connot (ck',_) -> aux ck'
    | Pck_up (_,_) | Pck_down (_,_) | Pck_phase (_,_) -> 
        raise (Error (loc, (Invalid_imported_clock ck_node)))
    | Pck_const (n,p) ->
        begin
          match !pck with
          | None -> pck := Some (n,p)
          | Some (n',p') ->
              if (n,p) <> (n',p') then
                raise (Error (loc, (Invalid_imported_clock ck_node)))
        end
    | Clink ck' -> aux ck'
    | Ccarrying (_,ck') -> aux ck'
    | Cvar _ | Cunivar _ ->
        match !pck with
        | None -> ()
        | Some (_,_) ->
            raise (Error (loc, (Invalid_imported_clock ck_node)))
  in
  aux ck_node

let clock_imported_node env loc nd =
  let new_env = clock_var_decl_list env false nd.nodei_inputs in
  ignore(clock_var_decl_list new_env false nd.nodei_outputs);
  let ck_ins = clock_of_vlist nd.nodei_inputs in
  let ck_outs = clock_of_vlist nd.nodei_outputs in
  let ck_node = new_ck (Carrow (ck_ins,ck_outs)) false in
  unify_imported_clock ck_node;
  check_imported_pclocks loc ck_node;
  try_generalize ck_node loc;
  nd.nodei_clock <- ck_node;
  Env.add_value env nd.nodei_id ck_node

let clock_top_decl env decl =
  match decl.top_decl_desc with
  | Node nd ->
      clock_node env decl.top_decl_loc nd
  | ImportedNode nd ->
      clock_imported_node env decl.top_decl_loc nd
  | SensorDecl _ | ActuatorDecl _ ->
      env

let clock_prog env decls =
  ignore(List.fold_left (fun e decl -> clock_top_decl e decl) env decls)
