open Ast
open Amd64
open Typing

let string_env = Hashtbl.create 17

let int_registers = [rdi; rsi; rdx; rcx; r8; r9]

(* Helper pour générer un label dans le code assembleur *)
let new_label =
  let c = ref 0 in
  fun s -> incr c; Printf.sprintf "__label__%s_%05i" s !c

(* *)
let rec fold2 f acc l1 l2 =
  match l1, l2 with
  | [], _ -> acc
  | _,[] -> failwith "fold2"
  | x1::ll1, x2::ll2 -> fold2 f(f acc x1 x2) ll1 ll2

(* Donne la taille en octets d'un type donné *)
let size_of t =
  match t with
  | Tvoid | Tnull -> assert false
  | Tinteger (_, Char) -> 1
  | Tinteger (_, Short) -> 2
  | Tinteger (_, Int) -> 4
  | Tinteger (_, Long) | Tdouble | Tpointer _ -> 8
  | Tstruct _ -> assert false

let align_of t =
  match t with
  | Tstruct _ -> assert false
  | _ -> size_of t

let round8 n  =
  if n mod 8 = 0 then n else ((n/8) + 1) * 8;;

let compile_const c =
  match c with
  | Cint (s, Int, i) ->
     movl ~$(Int64.to_int i) ~%r10d
  | Cint (_, _, i) ->
     movabsq (Int64.to_string i) ~%r10
  | Cstring s ->
     let label =
       try
	 Hashtbl.find string_env s
       with Not_found ->
	 let lab = new_label "string" in
	 Hashtbl.add string_env s lab;
	 lab
     in
     mov ~:label ~%r10
  | Cdouble d ->
     failwith("todo")

let reg_size_of t =
  match size_of t with
  | 1 -> `b
  | 2 -> `w
  | 4 -> `l
  | _ -> `q

(* cast la valeur dans r10 de type tfrom en type tto *)
let compile_cast tfrom tto =
  let size_tfrom = size_of tfrom in
  let size_tto = size_of tto in
  match tfrom, tto with
  | (Tvoid | Tstruct _), _ -> assert false
  | _, (Tvoid | Tstruct _) -> assert false
  | _ when size_tfrom = size_tto -> nop
  | _ when size_tto < size_tfrom ->
     let mask = (1 lsl (size_tto * 8)) - 1 in
     andq ~$mask ~%r10
  | Tinteger (Signed, Char), Tinteger(_, Short) -> movsbw ~%r10b ~%r10w
  | Tinteger (Unsigned, Char), Tinteger(_, Short) -> movzbw ~%r10b ~%r10w
  | Tinteger (Signed, Char), Tinteger(_, Int) -> movsbl ~%r10b ~%r10d
  | Tinteger (Unsigned, Char), Tinteger(_, Int) -> movzbl ~%r10b ~%r10d
  | Tinteger (Signed, Char), Tinteger(_, Long) -> movsbq ~%r10b ~%r10
  | Tinteger (Unsigned, Char), Tinteger(_, Long) -> movzbq ~%r10b ~%r10
  | Tinteger (Signed, Short), Tinteger(_, Int) -> movswl ~%r10w ~%r10d
  | Tinteger (Unsigned, Short), Tinteger(_, Int) -> movzwl ~%r10w ~%r10d
  | Tinteger (Signed, Short), Tinteger(_, Long) -> movswq ~%r10w ~%r10
  | Tinteger (Unsigned, Short), Tinteger(_, Long) -> movzwq ~%r10w ~%r10
  | Tinteger (Signed, Int), Tinteger(_, Long) -> movslq ~%r10d ~%r10
  | Tinteger (Unsigned, Int), Tinteger(_, Long) -> andq ~$0xffffffff ~%r10
  | _ -> failwith ("Error type")

let is_signed t = match t with
  | Tinteger (Signed, _) | Tpointer _ | Tnull -> true
  | _ -> false

let rec compile_expr_reg env e =
  match e.node with
  | Econst c -> compile_const c
  | Ecast (t, e0) ->
     compile_expr_reg env e0
     ++ compile_cast e0.info t
  | Eident _ | Eunop(Deref, _) | Egetarr _ ->
     let reg10 = r10_ (reg_size_of e.info) in
     compile_lvalue_reg env e ++
       mov (addr ~%r10) ~%reg10
  | Eunop(Addr, e) ->
     compile_lvalue_reg env e
  | Eassign (e1, e2) ->
     let e1_code = compile_lvalue_reg env e1 in
     let e2_code = compile_expr env e2 in
     let reg = r11_ (reg_size_of e1.info) in
     e2_code ++
       e1_code ++
       popq ~%r11 ++
       mov ~%reg (addr ~%r10)++
       movq ~%r11 ~%r10
  | Ebinop(e1, op, e2) ->
     let e1code = compile_expr env e1 in
     let e2code = compile_expr_reg env e2 in
     e1code ++
     e2code ++ (* e2 dans r10 *)
     popq ~%r11 ++(* e1 dans r11 *)
       begin
         match op with
         | Add -> failwith "todo binop add"
         | Mult -> failwith "todo binop mult"
         | Div | Mod ->
            let rsize = reg_size_of e1.info in
            let ra = rax_ rsize in
            let rd = rdx_ rsize in
            let re2 = r10_ rsize in
            let re1 = r11_ rsize in
            mov ~%re1 ~%ra ++
              (* Si on a des cas signé, il faut remplir le registre selon le signe, sinon on le met à 0 *)
              (if is_signed e1.info then
                 (if rsize = `q then
                    cqto ++ idivq ~%r10
                  else
                    cltd ++ idivl ~%r10d)
               else
                 xor ~%rd ~%rd ++
                   (if rsize = `q then divq ~%r10
                    else divl ~%r10d)
              )
            ++ (if op = Div then mov ~%ra ~%re2 else mov ~%rd ~%re2)
         | And -> failwith "todo binop and"
         | Or -> failwith "todo binop or"
         | Eq -> failwith "todo binop eq"
         | Neq -> failwith "todo binop neq"
         | Lt -> failwith "todo binop lt"
         | Le -> failwith "todo binop le"
         | Gt -> failwith "todo binop gt"
         | Ge -> failwith "todo binop ge"
         | Dot -> failwith "todo binop dot"
         | Arrow -> failwith "todo binop arrow"
         | _ -> failwith "unknown binop"
       end
  | Eunop (unop , e0) -> failwith "todo unop"
  | Ecall (f, params) ->
     let tret,_,_, extern = Hashtbl.find fun_env f.node in
     if extern then
       let arg_code = fold2 (fun (a_code) e r ->
			  a_code++
			    compile_expr env e ++
			    popq ~%r
			) nop params int_registers
       in
       print_string "extern";
       arg_code ++
	 xorq ~%rax ~%rax++
	 call f.node ++
	 mov ~%rax ~%r10
     else
       let size_ret = round8 (size_of tret) in
       let arg_size, arg_code =
	 List.fold_left (fun (a_size, a_code) e ->
	     (a_size + round8 (size_of e.info),
	      compile_expr env e ++ a_code)
	   ) (0, nop) params
       in
       subq ~$size_ret ~%rsp ++
	 arg_code ++
	 call f.node ++
	 addq ~$arg_size ~%rsp
  | Esizeof t	->failwith "todo"
  |_ -> compile_lvalue_reg env e

and compile_lvalue_reg env e =
  match e.node with
  | Eident e ->
     begin
       try
	 let offset = Env.find e.node env in
	 leaq (addr ~%rbp ~ofs:offset) ~%r10
       with Not_found ->
	 movq ~:(e.node) ~%r10
     end
  | Eunop(Deref, e) -> failwith "todo"
  | Egetarr (e, i) -> failwith "todo"
  | _ -> failwith "todo"

and compile_expr env e =
  match e.info with
  | Tstruct _ -> assert false
  | Tvoid -> compile_expr_reg env e
  | t when size_of t = 8 ->
     compile_expr_reg env e ++ pushq ~%r10
  | t ->
     let n = size_of t in
     let mask = (1 lsl (n*8)) -1 in
     compile_expr_reg env e ++
       andq ~$mask ~%r10 ++
       pushq ~%r10

and compile_clean_expr env e =
  let ecode = compile_expr env e in
  ecode ++ (if e.info = Tvoid then nop else popq ~%r10)

let rec compile_instr lab_fin rbp_offset env i =
  match i.node with
  | Sskip -> rbp_offset, nop
  | Sexpr e -> rbp_offset, compile_clean_expr env e
  | Sblock b -> compile_block lab_fin rbp_offset env b
  | Sreturn oe ->
     rbp_offset, (
      match oe with
      | None -> nop
      | Some e -> compile_expr env e
    ) ++ jmp lab_fin
  | Sif (e, i1, i2) ->
     let cond = comment "if" ++ compile_expr env e in
     let b1 = new_label "b1" in
     let b2 = new_label "b2" in
     cmpq ~%r10 ~$0;
     let block_1 = comment "i1" (* ++ (compile_instr lab_fin rbp_offset env i1) *)  in
     let block_2 = comment "i2" (* ++ ( compile_instr lab_fin rbp_offset env i2) *)  in
       rbp_offset, cond ++
                     jne b1 ++
                     jmp b2 ++
                     label b1 ++
                     block_1 ++
                     label b2 ++
                     block_2 ++
                     comment "fin if"

  | Sfor (e1, e2, e3, i) -> assert false

and compile_block lab_fin rbp_offset env (var_decls, instrs) =
  let new_offset, new_env, debug =
    List.fold_left (fun (aoffset, aenv, debug) (t, x) ->
        let aenv = Env.add x.node aoffset aenv in
        let offset = aoffset - round8 (size_of t) in
        let debug = debug ++ comment (Printf.sprintf "local: %s rbp[%d]" x.node aoffset) in
        (offset, aenv, debug)
      ) (rbp_offset, env, nop) var_decls
  in
  List.fold_left (fun (aoffset, acode) i ->
      let ioffset, icode =
        compile_instr lab_fin new_offset new_env i
      in
      (min ioffset aoffset,
       acode ++ icode)) (new_offset, debug) instrs

let compile_decl (atext, adata) d =
  match d with
  | Dstruct _ -> atext, adata
  | Dvar (t, id) ->
     atext,
     let n = size_of t in
     let a = align_of t in
     adata ++
       label id.node ++
       align a ++
       space n
  | Dfun (_, _, _, None) -> atext, adata
  | Dfun (tret, f, params, Some body) ->
     let last_offset, env =
       List.fold_left (fun (aoffset, aenv) (t, x) ->
           let aenv = Env.add x.node aoffset aenv in
           let offset = aoffset + round8 (size_of t) in
           (offset, aenv)
         ) (8, Env.empty) params
     in
     let ret_offset = last_offset + round8 (size_of tret) in
     let lab_fin = f.node ^ "_fin" in
     let max_rbp_offset, body_code = compile_block lab_fin (-8) env body in
     (* max_rbp_offset est négatif *)
     let code =
       glabel f.node ++
         comment ("On rentre dans la function " ^ f.node) ++
         pushq ~%rbp ++
         mov ~%rsp ~%rbp ++
         body_code ++

         label lab_fin ++
         (if (tret <> Tvoid) then
            if f.node = "main" then popq ~%rax
            else
              popq ~%r10 ++
                mov ~%r10 (addr ~ofs:ret_offset ~%rbp)
          else nop) ++
         mov ~%rbp ~%rsp ++
         popq ~%rbp ++
         ret
     in
     atext ++ code, adata

let compile_prog p =
  let text, data =
    List.fold_left compile_decl (nop, nop) p
  in
  let data = Hashtbl.fold (fun str lbl a_data ->
                 a_data ++
                   label lbl ++
                   string str
               ) string_env data
  in
  {
    text = text;
    data = data
  }
