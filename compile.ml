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

let compile_block lab_fin rbp_offset env body = assert false

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
        let env = Env.empty in
        let lab_fin = f.node ^ "_fin" in
        let max_rbp_offset, body_code = compile_block (-8) env body in
        (* max_rbp_offset est négatif *)
        let code =
            glabel f.node ++
            comment ("On rentre dans la function " ^ f.node) ++
            pushq ~%rbp ++
            mov ~%rsp ~%rbp ++

            (* Beaucoup de choses *)
            (* mov ~$42 ~%rax ++ *)
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
