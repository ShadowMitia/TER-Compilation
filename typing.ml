open Ast

module Env = Map.Make(String)

let global_env = Hashtbl.create 17
let struct_env = Hashtbl.create 17
let fun_env = Hashtbl.create 17

let mk_node t e = { info = t; node = e }

let compatible t1 t2 =
	let rec compat_aux t1 t2 =
		match t1, t2 with
		| Tstruct id1, Tstruct id2 -> id1.node = id2.node
		| Tpointer (Tvoid), Tpointer _ -> true
		| Tpointer tt1, Tpointer tt2 -> compat_aux tt1 tt2
		| Tnull, Tpointer _ -> true
		| ((Tdouble | Tnull | Tinteger(_) ),
		  (Tdouble | Tnull | Tinteger(_) )) -> true
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
	match t with
	| Tstruct _ | Tvoid -> false
	| _ -> true

let arith t =
	match t with
	| Tstruct _ | Tvoid | Tpointer _ -> false
	| _ -> true

exception TypeError of loc * string
let error loc msg = raise (TypeError (loc, msg))

let rec type_bf t =
  match t with
  | Tpointer tt -> type_bf tt
  | Tstruct id -> Hashtbl.mem struct_env id.node
  | _ -> true

;;

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
      else error id.info "Champ ou variable incorrect")
    []
	vd
  in vd

let type_const c =
	match c with
	| Cstring _ -> Tpointer (Tinteger(Signed, Char))
	| Cdouble _ -> Tdouble
	(*| Cint(Signed, Int, 0) -> Tnull
	| Cint(s, i, _) -> Tinteger (s, i)*)
	| _ -> assert false

let rec type_expr env e =
	match e.node with
	| Econst c -> let tc = type_const c in
					mk_node tc (Econst c)
	| Eunop (unop, e0) ->
		begin match unop with
			| Neg -> let te0 = type_expr env e0 in
					if not (arith te0.info) then
						error e0.info "Type invalide"
					else
						mk_node te0.info (Eunop(Neg, te0))
			(*| Deref -> let te0 = type_lvalue env e0 in
						assert false*)
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
	(* Doit gérer ident(fait), acces à un champ et étoile de qlqchose *)
	| _ -> error e.info "Valeur gauhe attendue"


let typ_instr ty env i =
	match i.node with
	| Sskip -> mk_node Tvoid Sskip
	| Sexpr e -> let te = type_expr env e in
				mk_node te.info (Sexpr te)

let type_block t env (var_decl, instrs) =
	(type_var_decl var_decl,
	let new_env = add_env env var_decl in
	List.map (typ_instr t new_env) instrs)

let type_decl d =
  match d with
  | Dvar (t, i) -> if type_bf t && t <> Tvoid then
        add_global_env global_env i t;
		Dvar (t, i)

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
	List.map (type_decl) l
