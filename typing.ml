open Ast

let global_env = Hashtbl.create 17
let struct_env = Hashtbl.create 17

exception TypeError of loc * string
let error loc msg = raise (TypeError (loc, msg))

let rec type_bf t =
  match t with
  | Tpointer tt -> type_bf tt
  | Tstruct id -> Hashtbl.mem struct_env id.node
  | _ -> true

;;

let type_decl d =
  match d with
  | Dvar (t, i) -> if type_bf t && t <> Tvoid
      && not (Hashtbl.mem global_env i.node) then begin
        Hashtbl.add global_env i.node t
      end else
        error i.info "Déclaration globale invalide"
  | _ -> assert false

let type_prog l =
  List.iter (type_decl) l
