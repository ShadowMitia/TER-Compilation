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
    | Tpointer (Tvoid), Tpointer _ -> true
    | Tpointer tt1, Tpointer tt2 -> compat_aux tt1 tt2
    | Tnull, Tpointer _ -> true
    | ((Tdouble | Tnull | Tinteger(_) ), (Tdouble | Tnull | Tinteger(_) )) -> true
    | _ -> false
  in
  compat_aux t1 t2 || compat_aux t2 t1


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

let inf_type t1 t2 =
  rank t1 < rank t2

let max_type t1 t2 =
  if inf_type t1 t2 then t2
  else t1

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
  | _ -> assert false


let rec type_expr env e =
  match e.node with
  | Econst c -> let tc = type_const c in
		mk_node tc (Econst c)
  | Eunop (unop, e0) ->
     begin
       match unop with
       | Pos ->  let te0 = type_expr env e0 in
	         if not (num te0.info) then
		   error e0.info "Type invalide : '+' pas compatible avec"
	         else
		   mk_node te0.info (Eunop(Pos, te0))
       | Neg ->  let te0 = type_expr env e0 in
	         if not (num te0.info) then
		   error e0.info "Type invalide : '-' pas compatible avec"
	         else
		   mk_node te0.info (Eunop(Neg, te0))
       | Not -> let te0 = type_expr env e0 in
	        if not (num te0.info) then
		  error e0.info "Type invalide : '!' pas compatible avec"
	        else
		  mk_node te0.info (Eunop(Not, te0))
       | Deref -> let te0 = type_lvalue env e0 in
                  if not (arith te0.info) then
                    error e0.info "Type invalide - '&' pas compatible avec"
                  else
                    mk_node te0.info (Eunop(Deref, te0))
       (*| Addr ->  let te0 = type_expr env e0 in
                  if not (arith te0.info) then
                    error e0.info "Type invalide"
                  else
                    mk_node te0 (Eunop(Addr, te0))*)
       (*| PreInc -> let te0 = type_expr env e0 in
                   if not (arith te0.info) then
                     error e0.info "Type invalide"
                   else
                     mk_node e0.info (Eunop(PreInc, te0))
       | PreDec -> let te0 = type_expr env e0 in
                   if not (arith te0.info) then
                     error e0.info "Type invalide"
                   else
                     mk_node e0.info (Eunop(PreDec, te0))
       | PostInc -> let te0 = type_expr env e0 in
                    if not (arith te0.info) then
                      error e0.info "Type invalide"
                    else
                      mk_node e0.info (Eunop(PostInc, te0))
       | PostDec -> let te0 = type_expr env e0 in
                    if not (arith te0.info) then
                      error e0.info "Type invalide"
                    else
                      mk_node e0.info (Eunop(PostDec, te0))
        *)
     end

  | Ebinop(e1, op, e2) ->
     begin
       match op with
       | Add -> assert false
       | Mult -> assert false
       | Minus -> assert false
       | Div -> assert false
       | Mod -> assert false
       | And -> assert false
       | Or -> assert false
       | Eq -> assert false
       | Neq -> assert false
       | Lt -> assert false
       | Le -> assert false
       | Gt -> assert false
       | Ge -> assert false
       | Dot ->
          (*let te0 = type_expr env e1 in
          begin
          match te0.info with
          | Tstruct id -> let fields = Hashtbl.find struct_env id.node in
                          begin
                            try
                              let t, _ = List.find (fun (t, y) -> y.node = e2.node) fields in
                              let te1 = type_expr env t in
                              assert false; mk_node te0.info (Ebinop(te0, Dot, te0))
                            with Not_found -> error e2.info "Champ de structure inconnu"
                          end
            | _ -> error e.info "Accès ầ une valeur non structurelle"
            end
           *) assert false;
       | Arrow -> assert false
     end

  (*| Egetarr (expr1, expr2) -> let  te0 = type_expr env expr1 in
                              let te1 = type_expr env expr2 in
                              if not (arith te0.info) then
                                error expr1.info "Type invalide - liste";
                              if not (arith te1.info) then
                                error expr2.info "Type invalide - indice liste";
                              mk_node te0.info (Egetarr(te0, te1)) *)

  | Ecall (f, params) ->
     let tparams = List.map (type_expr env) params in
     begin
       try
         let tret, _, args = Hashtbl.find fun_env f.node in
         try
           List.iter2 (fun e (t, x) ->
               if not (compatible e.info t) then
                 error x.info ("Type invalide pour le paramètre" ^ x.node ^ " de " ^ f.node))
                      tparams
                      args;
           mk_node tret (Ecall(f, tparams))
         with Invalid_argument _ -> error f.info ("Nombre d'arguments invalide pour " ^ f.node)
       with
         Not_found -> error f.info ("La fonction " ^ f.node ^ " n'existe pas")
     end
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
  | Ebinop(e1, Dot, e2) -> failwith "lvalue dot"
  | Eunop(Deref, e) -> failwith "lvalue deref"
  (* Doit gérer ident(fait), acces à un champ et étoile de qlqchose *)
  | _ -> error e.info "Valeur gauche attendue "

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
  | Swhile (cond, b) -> let te = type_expr env cond in
                        let tb = type_instr ty env b in
                        mk_node te.info (Swhile (te, tb))
  | Sreturn None -> mk_node Tvoid (Sreturn None)
  | Sreturn (Some b) -> let tb = type_expr env b in
                        mk_node tb.info (Sreturn (Some tb))

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
	 add_global_env fun_env f (t, f, params);
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
  print_int (List.length l); List.map (type_decl) l
