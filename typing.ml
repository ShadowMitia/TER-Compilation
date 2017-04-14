open Ast

module Env = Map.Make(String)

let global_env = Hashtbl.create 17
let struct_env = Hashtbl.create 17
let fun_env = Hashtbl.create 17

let mk_node t e = { info = t; node = e }

let compatible t1 t2 =
  (* Vérifie si deux types sont compatibles entre eux *)
  let rec compat_aux t1 t2 =
    match t1, t2 with
    | Tstruct id1, Tstruct id2 -> id1.node = id2.node
    | Tpointer Tvoid, Tpointer _ ->  true
    | Tpointer tt1, Tpointer tt2 -> compat_aux tt1 tt2
    | Tnull, Tpointer _ -> true
    | ((Tdouble | Tnull | Tinteger(_) ), (Tdouble | Tnull | Tinteger(_) )) -> true
    | _ -> false
  in
  compat_aux t1 t2 || compat_aux t2 t1

let rec type_eq t1 t2 =
  match t1, t2 with
  | Tnull, Tnull
  | Tvoid, Tvoid
  | Tdouble, Tdouble -> true
  | Tinteger(s1, k1), Tinteger(s2, k2) -> s1 = s2 && k1 = k2
  | Tstruct id1, Tstruct id2 -> id1.node = id2.node
  | Tpointer p1, Tpointer p2 -> type_eq p1 p2
  | _ -> false

(* input: type et expression *)
let mk_cast t e =
  if not (compatible t e.info) then
    assert false
  else
    mk_node t (Ecast(t, e))

let num t =
  (* Vérifie si c'est un numérique (nombre ou pointeur) *)
  match t with
  | Tstruct _ | Tvoid -> false
  | _ -> true

let arith t =
  (* Vérifie si le nombre est une valeur arithmétique *)
  match t with
  | Tstruct _ | Tvoid | Tpointer _ -> false
  | _ -> true

let rank t =
  let rank_aux n = match n with
    | Char -> 7
    | Short -> 15
    | Int -> 31
    | Long -> 63
  in
  match t with
  | Tinteger(Signed, n) -> rank_aux n
  | Tinteger(Unsigned, n) -> 1 + rank_aux n
  | Tdouble -> 100
  | Tnull -> 0
  | _ -> assert false


let type_lt t1 t2 =
  arith t1 && arith t2 && (rank t1) < (rank t2)

let type_lte t1 t2 =
  type_lt t1 t2 || type_eq t1 t2

let max_type t1 t2 =
  if type_lt t1 t2 then t2
  else t1

let signed_int = Tinteger(Signed, Int)
let unsigned_int = Tinteger(Unsigned, Int)
let signed_long = Tinteger(Signed, Long)
let unsigned_long = Tinteger(Unsigned, Long)

let is_double t =
  match t with
  | Tdouble -> true
  | _ -> false

exception TypeError of loc * string

let error loc msg = raise (TypeError (loc, msg))

let rec type_bf t =
  (* Vérifie si un type est bien formé *)
  match t with
  | Tpointer tt -> type_bf tt
  | Tstruct id -> Hashtbl.mem struct_env id.node;
  | _ -> true

let add_global_env tab key v =
  if Hashtbl.mem tab key.node then
    error key.info ("Redéfinition de la structure " ^ key.node)
  else begin
      Hashtbl.add tab key.node v;
    end

let add_env env vd =
  List.fold_left (fun acc (t, id) -> Env.add id.node t acc) env vd

let type_var_decl vd =
  let _ =
    List.fold_left (fun seen (t, id) ->
        if type_bf t && not (List.mem id.node seen) then
          id.node :: seen
        else error id.info "Champ ou variable incorrect") [] vd
  in vd

let type_const c =
  match c with
  | Cstring _ -> Tpointer (Tinteger(Signed, Char))
  | Cdouble _ -> Tdouble
  | Cint(s, i, _) -> Tinteger (s, i)

(* A VERIFIER: les ariths et nums de chaque cas *)
(* TODO: finir les messages d'erreurs *)
let rec type_expr env e =
  match e.node with
  | Econst c -> let tc = type_const c in
		mk_node tc (Econst c)
  | Eunop (unop, e0) ->
     begin
       let te0 = type_expr env e0 in
       match unop with
       | Pos ->
	         if not (arith te0.info) then
		   error e0.info "Type invalide : '+' pas compatible avec "
	         else
		   mk_node te0.info (Eunop(Pos, te0))
       | Neg ->
	         if not (arith te0.info) then
		   error e0.info "Type invalide : '-' pas compatible avec"
	         else
		   mk_node te0.info (Eunop(Neg, te0))
       | Not ->
	        if not (arith te0.info) then
		  error e0.info "Type invalide : '!' pas compatible avec"
	        else
		  mk_node te0.info (Eunop(Not, te0))
       | Deref ->
                  if not (num te0.info) then
                    error e0.info "Type invalide - '*' pas compatible avec"
                  else
                    mk_node te0.info (Eunop(Deref, te0))
       | Addr ->
                  if not (num te0.info) then
                    error e0.info "Type invalide - '&'"
                  else
                    mk_node te0.info (Eunop(Addr, te0))
       | PreInc ->
                   if not (num te0.info) then
                     error e0.info "Type invalide"
                   else
                     mk_node te0.info (Eunop(PreInc, te0))
       | PreDec ->
                   if not (num te0.info) then
                     error e0.info "Type invalide"
                   else
                     mk_node te0.info (Eunop(PreDec, te0))
       | PostInc ->
                    if not (num te0.info) then
                      error e0.info "Type invalide"
                    else
                      mk_node te0.info (Eunop(PostInc, te0))
       | PostDec ->
                    if not (num te0.info) then
                      error e0.info "Type invalide"
                    else
                      mk_node te0.info (Eunop(PostDec, te0))
     end
  | Ebinop (e1, Dot, e2) -> type_struct_access env e1 e2
  | Ebinop (e1, Arrow, e2) -> type_struct_access env e1 e2
  | Ebinop(e1, op, e2) ->
     let te1 = type_expr env e1 in
     let te2 = type_expr env e2 in
     let t1 = te1.info in
     let t2 = te2.info in
     let nte1, nte2 =
       if arith t1 && arith t2 && not (type_eq t1 t2) then
         if is_double t1 then
           te1, mk_cast Tdouble te2
         else if is_double t2 then
           mk_cast Tdouble te1, te2
         else
           let te1 = if type_lt t1 signed_int then mk_cast signed_int te1 else te1 in
           let te2 = if type_lt t2 signed_int then mk_cast signed_int te2 else te2 in
           let t1 = te1.info in
           let t2 = te2.info in
           if type_eq t1 unsigned_long then te1, mk_cast unsigned_long te2
           else if type_eq t2 unsigned_long then mk_cast unsigned_long te1, te2
           else if type_eq t1 signed_long then te1, mk_cast signed_long te2
           else if type_eq t2 signed_long then mk_cast signed_long te1, te2
           else if type_eq t1 unsigned_int then te1, mk_cast unsigned_int te2
           else if type_eq t2 unsigned_int then mk_cast unsigned_int te1, te2
           else te1, te2
       else
         te1, te2 in
     begin
       match op with
       | Add -> mk_node nte1.info (Ebinop(nte1, Add, nte2))
       | Mult -> mk_node nte1.info (Ebinop(nte1, Mult, nte2))
       | Minus -> mk_node nte1.info (Ebinop(nte1, Minus, nte2))
       | Div -> mk_node nte1.info (Ebinop(nte1, Div, nte2))
       | Mod -> mk_node nte1.info (Ebinop(nte1, Mod, nte2))
       | And -> mk_node nte1.info (Ebinop(nte1, And, nte2))
       | Or -> mk_node nte1.info (Ebinop(nte1, Or, nte2))
       | Eq -> mk_node nte1.info (Ebinop(nte1, Eq, nte2))
       | Neq -> mk_node nte1.info (Ebinop(nte1, Neq, nte2))
       | Lt -> mk_node nte1.info (Ebinop(nte1, Lt, nte2))
       | Le -> mk_node nte1.info (Ebinop(nte1, Le, nte2))
       | Gt -> mk_node nte1.info (Ebinop(nte1, Gt, nte2))
       | Ge -> mk_node nte1.info (Ebinop(nte1, Ge, nte2))
       | Dot -> assert false (* Dois jamais arrivé *)
       | Arrow -> assert false (* Dois jamais arrivé *)
     end

  | Egetarr (expr1, expr2) -> let  te0 = type_expr env expr1 in
                              let te1 = type_expr env expr2 in
                              if not (arith te0.info) then
                                error expr1.info "Type invalide - liste";
                              if not (arith te1.info) then
                                error expr2.info "Type invalide - indice liste";
                              mk_node te0.info (Egetarr(te0, te1))

  | Ecall (f, params) ->
     let tparams = List.map (type_expr env) params in
     begin
       try
         let tret, _, args, _ = Hashtbl.find fun_env f.node in
         try
             let new_params =
                 if args = [] then
                     tparams
                 else
                     List.map2 (fun e(t, x) ->
                         if not (compatible e.info t) then
                             error x.info ("Invalid type")
                         else
                             mk_node t (Ecast(t, e))
                 ) tparams args in
             mk_node tret (Ecall(f, new_params))
         with Invalid_argument _ -> error f.info ("Nombre d'arguments invalide pour " ^ f.node)
       with Not_found -> error f.info ("La fonction " ^ f.node ^ " n'existe pas")
     end
  | Eassign (e1, e2) ->
     let te1 = type_lvalue env e1 in
     let te2 = type_expr env e2 in
     if not (compatible te1.info te2.info) then
       error e1.info ("Type incompatible pour l'affectation")
     else
       mk_node te1.info (Eassign(te1, mk_cast te1.info te2))
  | _ -> type_lvalue env e

and type_lvalue env e =
  match e.node with
  | Eident id ->
     let t =
       try
	 try
	   Env.find id.node env
	 with
	   Not_found ->
	   Hashtbl.find global_env id.node
       with
	 Not_found -> error id.info ("Variable non définie " ^ id.node)
     in
     mk_node t (Eident id)
  | Ebinop(e1, Dot, e2) -> type_struct_access env e1 e2
  | Eunop(Deref, e) -> let t = type_expr env e in
                       begin
                         match t.info with
                         | Tpointer p -> mk_node p (Eunop(Deref, t))
                         | _ -> error e.info "Type pointeur attendue après '*'"
                       end
  | _ -> error e.info "Valeur gauche attendue "

and type_struct_access env s iden =
 let var_ident = begin match iden.node with
    | Eident i -> i
    | _ -> assert false
    end
  in
  let t_struct = type_expr env s in
  match t_struct.info with
  | Tstruct id ->
     try
       let fields = Hashtbl.find struct_env id.node in
       try
         let t, i = List.find (fun (t, i) -> i.node = var_ident.node) fields in
         mk_node t (Ebinop(t_struct, Dot, mk_node t (Eident var_ident)))
         with Not_found -> error iden.info ("Champ invalide " ^ var_ident.node)
        with Not_found -> error id.info ("Structure non définie " ^ id.node)
  | _ -> error s.info ("Accès à un champ non structure")

let rec type_instr ty env i =
  match i.node with
  | Sskip -> mk_node Tvoid Sskip
  | Sexpr e -> let te = type_expr env e in
	       mk_node te.info (Sexpr te)
  | Sblock (vars, instrs) -> let tb = type_block ty env (vars, instrs) in
                             mk_node Tvoid (Sblock tb)
  | Sif (cond, b1, None) -> let te = type_expr env cond in
                            let tb1 = type_instr ty env b1 in
                            mk_node te.info (Sif (te, tb1, None))
  | Sif (cond, b1, Some b2) -> let te = type_expr env cond in
                               let tb1 = type_instr ty env b1 in
                               let tb2 = type_instr ty env b2 in
                               mk_node te.info (Sif (te, tb1, Some tb2))
  | Sfor (None, Some cond, None, b) ->
     let te1 = type_expr env cond in
     let tb = type_instr ty env b in
     mk_node Tvoid (Sfor (None, Some te1, None, tb))
  | Sfor (Some e1, Some cond, Some e2, b) ->
     let te1 = List.map (type_expr env) e1 in
     let te2 = type_expr env cond in
     let te3 = List.map (type_expr env) e2 in
     let tb = type_instr ty env b in
     mk_node Tvoid (Sfor (Some te1, Some te2, Some te3, tb))
  | Sreturn None -> mk_node Tvoid (Sreturn None)
  | Sreturn (Some b) -> let tb = type_expr env b in
                        mk_node tb.info (Sreturn (Some tb))
  | _ -> error i.info ("Erreur de l'instruction")

and type_block t env (var_decl, instrs) =
  let new_env = add_env env var_decl in
  let tvd = type_var_decl var_decl in
  let ti = List.map (type_instr t new_env) instrs in
  (tvd, ti)


let type_decl d =
  match d with
  | Dvar (t, i) -> if type_bf t && t <> Tvoid then
                     add_global_env global_env i t; Dvar (t, i)

  | Dstruct (id, var_decl) ->
     add_global_env struct_env id var_decl;
     let t_var_decl = type_var_decl var_decl in
     Dstruct(id, t_var_decl)

  | Dfun (t, f, params, b) ->
     if not (type_bf t) then error f.info "Type de retour invalide"
     else begin
	 add_global_env fun_env f (t, f, params, b = None);
	 let t_params = type_var_decl params in
	 let t_block = match b with
	   | None -> None
	   | Some block -> let env = add_env Env.empty params in
			   let t_block = type_block t env block in
			   Some t_block
	 in
	 Dfun(t, f, t_params, t_block)
       end

let type_prog l =
  print_int (List.length l); List.map (type_decl) l;
  (*Helpers.print_fun_env fun_env*)
