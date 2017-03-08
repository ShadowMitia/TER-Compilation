open Ast
open Amd64
open Typing

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
    | Cstring s -> assert false
    | Cdouble d -> assert false

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
    | _ -> assert false

let rec compile_expr_reg env e =
    match e.node with
    | Econst c -> compile_const c
    | Ecast (t, e0) ->
        compile_expr_reg env e0
        ++ compile_cast e0.info t
    | _ -> failwith "todo"

let rec compile_expr env e =
    match e.info with
    | Tstruct _ -> assert false
    | Tvoid -> compile_expr_reg env e
    | t when size_of t = 8 ->
        compile_expr_reg env e ++ pushq ~%r10
    | t ->
        let n = size_of t in
        let mask = if n = 4 then 0xffffffff
            else if n = 2 then 0xffffffff
            else 0xff
        in
        andq ~$mask ~%r10 ++
        pushq ~%r10

let compile_clean_expr env e =
    let ecode = compile_expr env e in
    ecode ++ (if e.info = Tvoid then nop else popq ~%r10)

let rec compile_instr lab_fin rbp_offset env i =
    match i.node with
    | Sskip -> rbp_offset, nop
    | Sexpr e -> rbp_offset, compile_clean_expr env e
    | Sreturn oe ->
        rbp_offset, (
            match oe with
            | None -> nop
            | Some e -> compile_expr env e
        ) ++ jmp lab_fin
    | _ -> assert false

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
    {
        text = nop;
        data = nop

    }
