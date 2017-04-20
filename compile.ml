open Ast
open Amd64
open Typing

let string_env = Hashtbl.create 17
let double_env = Hashtbl.create 17

let int_registers = [rdi; rsi; rdx; rcx; r8; r9]
let double_registers = [ xmm0; xmm1; xmm2; xmm3; xmm4; xmm5; xmm6; xmm7]

(* Helper pour générer un label dans le code assembleur *)
let new_label =
  let c = ref 0 in
  fun s -> incr c; Printf.sprintf "__label__%s_%05i" s !c

(* Donne la taille en octets d'un type donné *)
let size_of t =
  match t with
  | Tvoid -> 0
  | Tnull -> assert false
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
  if n mod 8 = 0 then n else ((n/8) + 1) * 8

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
  | Cdouble s ->
     let label =
       try
	 Hashtbl.find double_env s
       with Not_found ->
	 let lab = new_label "string" in
	 Hashtbl.add double_env s lab;
	 lab
     in
     mov ~:label ~%r10 ++
       mov (addr ~%r10) ~%r10

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

let comp_res comp =
  let lf = new_label "cond" in
  let le = new_label "cond_end" in
  comp lf ++
    movq ~$0 ~%r10 ++
    jmp le ++
    label lf ++
    movq ~$1 ~%r10 ++
    label le


let rec assign_regs env args iregs dregs (d_acc, code_acc) =
  match args, iregs, dregs with
  | [], _, _ -> d_acc, code_acc
  | e :: _, _,  [] when e.info = Tdouble -> assert false
  | e :: _, [], _ -> assert false
  | e :: next_args, _, dreg :: next_dregs when e.info = Tdouble ->
     assign_regs env next_args iregs next_dregs
                 (1 + d_acc, code_acc ++ compile_expr env e ++ popd ~%dreg)
  | e :: next_args, ireg :: next_iregs, _ ->
     assign_regs env next_args next_iregs dregs
                 (d_acc, code_acc ++ compile_expr env e ++ popq ~%ireg)

and compile_expr_reg env e =
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
     comment "assign" ++
       e2_code ++
       e1_code ++
       popq ~%r11 ++
       mov ~%reg (addr ~%r10) ++
       movq ~%r11 ~%r10 ++
       comment "end assign"
  | Ebinop(e1, op, e2) ->
     let e1code = compile_expr env e1 in
     let e2code = compile_expr_reg env e2 in
     e1code ++
       e2code ++ (* e2 dans r10 *)
       popq ~%r11 ++(* e1 dans r11 *)
       begin
       match e1.info with
       | Tdouble ->
          movq ~%r10 ~%xmm0 ++
            movq ~%r11 ~%xmm1
       | _ -> nop
       end
     ++
       begin
         match op, e1.info with
         | Add, Tdouble -> addsd ~%xmm0 ~%xmm1
         | Add, Tinteger(_, _) -> addq ~%r11 ~%r10
         | Minus, Tinteger(_,_) -> subq ~%r11 ~%r10
         | Mult, Tinteger(_,_) -> imulq ~%r11 ~%r10
         | Div, Tdouble -> divsd ~%xmm0 ~%xmm1
         | Div, Tinteger(_,_) | Mod, Tinteger(_,_) ->
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
         | And, Tinteger(_,_) -> andq ~%r11 ~%r10
         | Or, Tinteger(_,_) -> orq ~%r11 ~%r10
         | Eq, Tinteger(_,_) -> cmpq ~%r10 ~%r11 ++ comp_res jne
         | Neq, Tinteger(_,_) -> cmpq ~%r10 ~%r11 ++ comp_res je
         | Lt, Tinteger(_,_) -> cmpq ~%r10 ~%r11 ++ comp_res jg
         | Le, Tinteger(_,_) -> cmpq ~%r10 ~%r11 ++ comp_res jge
         | Ge, Tinteger(_,_) -> cmpq ~%r10 ~%r11 ++ comp_res jle
         | Gt, Tinteger(_,_) -> cmpq ~%r10 ~%r11 ++ comp_res jl
         | Dot, Tinteger(_,_) -> failwith "todo binop dot"
         | Arrow, Tinteger(_,_) -> failwith "todo binop arrow"
         | _, _ -> failwith "unknown binop"
       end
     ++
       begin
         match e1.info with
         | Tdouble -> divsd ~%xmm0 ~%xmm1
         | _ -> nop
       end

  | Eunop (unop , e0) ->
     begin
       let ecode = compile_lvalue_reg env e0 in
       comment "unop" ++
         ecode ++
         match unop with
         | Neg -> assert false (*comment "neg" ++ negq ~%r10*)
         | Deref -> assert false
         | Pos -> assert false
         | Addr -> assert false
         | PreInc -> addq ~$1 (addr ~%r10)
         | PreDec -> subq ~$1 (addr ~%r10)
         | PostInc -> addq ~$1 (addr ~%r10)
         | PostDec -> subq ~$1 (addr ~%r10)
         | Not -> notq (addr ~%r10)
     end
    ++ pushq (addr ~%r10)
     ++ comment "end unop"
  | Ecall (f, params) ->
     let tret,_,_, extern = Hashtbl.find fun_env f.node in
     if extern then
       let n_double, arg_code =
         assign_regs env params int_registers double_registers (0, nop)
       in
       arg_code ++
         mov ~$n_double ~%rax ++
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
	 addq ~$arg_size ~%rsp ++
         if (tret <> Tvoid) then popq ~%r10 else nop
  | Esizeof t -> failwith "todo"
(*|_ -> compile_lvalue_reg env e*)

and compile_lvalue_reg env e =
  match e.node with
  | Eident e ->
     comment "Eident" ++
       begin
         try
	   let offset = Env.find e.node env in
	   leaq (addr ~%rbp ~ofs:offset) ~%r10
         with Not_found ->
	   movq ~:(e.node) ~%r10
       end
     ++ comment "f Eident"
  | Eunop(Deref, e) -> failwith "todo"
  | Egetarr (e, i) -> failwith "todo"
  | _ -> failwith "todo"

and compile_expr env e =
  match e.info with
  | Tstruct _ -> failwith "On gère pas les structures"
  | Tvoid -> comment "void" ++ compile_expr_reg env e ++ comment "tvoid"
  | t when size_of t = 8 ->
     comment "8 octets" ++
       compile_expr_reg env e ++
       pushq ~%r10 ++
       comment "f 8 octets"
  | t ->
     let n = size_of t in
     let mask = (1 lsl (n*8)) -1 in
     comment "less than 8 octet" ++
       compile_expr_reg env e ++
       comment "mask" ++
       andq ~$mask ~%r10 ++
       pushq ~%r10 ++
       comment "f less than 8 octer"

and compile_clean_expr env e =
  let ecode = compile_expr env e in
  ecode ++ (if e.info = Tvoid then nop else popq ~%r10)

and compile_expr_list env elist =
  List.fold_left (fun acc e ->
      let code = compile_expr env e
      in code ++ acc)
                 nop elist

let rec compile_instr lab_fin rbp_offset env i =
  match i.node with
  | Sskip -> rbp_offset, nop ++ comment "skip"
  | Sexpr e -> rbp_offset, comment "sexpr" ++ compile_clean_expr env e ++ comment "f sexpr"
  | Sblock b -> let off, c = compile_block lab_fin rbp_offset env b in
                off, comment "block" ++ c ++ comment "f block"
  | Sreturn oe ->
     rbp_offset, comment "return" ++ (
       match oe with
       | None -> nop
       | Some e -> compile_expr env e
     ) ++ jmp lab_fin ++ comment "f return"
  | Sif (e, i1, i2) ->
     let cond = comment "condition if" ++ compile_expr env e ++ comment "fin condition if" in
     let e = new_label "else" in
     let if_end = new_label "if_end" in
     let rbp_offset, b1 = compile_instr lab_fin rbp_offset env i1 in
     let rbp_offset, b2 =
       match i2 with
       | Some i2 ->(compile_instr lab_fin rbp_offset env i2)
       | None -> rbp_offset, nop
     in
     rbp_offset,
     comment "if" ++
       cond ++
       popq ~%r10 ++
       test ~%r10 ~%r10 ++
       je e ++
       b1 ++
       jmp if_end ++
       label e ++
       b2 ++
       label if_end ++
       comment "end if"
  | Sfor (e1, e2, e3, i) ->
     let for_label = new_label "for" in
     let for_end = new_label "for_end" in
     let cond_init = match e1 with | Some e1 -> compile_expr_list env e1 | None -> nop in
     let cond = match e2 with | Some e2 -> compile_expr env e2 | None -> pushq ~$1 in
     let cond_change = match e3 with | Some e3 -> compile_expr_list env e3 | None -> nop in
     let rbp_offset, b = compile_instr lab_fin rbp_offset env i in
     rbp_offset,
     cond_init ++
       label for_label ++
       cond ++
       popq ~%r10 ++
       cmpq ~$0 ~%r10 ++
       je for_end ++
       b ++
       cond_change ++
       jmp for_label ++
       label for_end

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
  let data = Hashtbl.fold (fun dbl lbl a_data ->
                 a_data ++
                   label lbl ++
                   ddouble [dbl]) double_env data
  in
  {
    text = text;
    data = data
  }
