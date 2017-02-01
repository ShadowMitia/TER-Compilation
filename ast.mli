(* Arbres de syntaxe abstraite *)

type ('info, 'node) node = { info : 'info;
			     node : 'node }

type loc = Lexing.position * Lexing.position

type ident = (loc, string) node

type signedness = Signed | Unsigned
type num_type = Char | Short | Int | Long

type c_type =
  | Tnull (* pour typer null *)
  | Tvoid
  | Tinteger of signedness * num_type
  | Tdouble
  | Tstruct of ident
  | Tpointer of c_type

type constant =
  | Cint of int32
  | Clong of int64
  | Cdouble of float
  | Cstring of string

type binop = Add | Mult | Minus | Div | Mod
	     | And | Or
	     | Eq  | Neq  | Lt    | Le  | Gt  | Ge

type unop = Neg | Deref | Pos

type 'info expr = ('info, 'info expr_node) node
and 'info expr_node =
  | Enull (* Inséré automatiquement à partir de 0 *)
  | Econst of constant
  | Eident of ident
  | Esizeof of c_type
  | Ebinop of 'info expr * binop * 'info expr
  | Eunop of  unop * 'info expr
  (* à compléter *)

type var_decl =  c_type * ident

type 'info statement = ('info, 'info statement_node) node

and 'info statement_node =
  | Sskip
  | Sexpr of 'info expr
  (* à compléter *)

and 'info block =
    var_decl list * 'info statement list

type 'info decl =
  | Dfun of c_type * ident * var_decl list * ('info block option)
  | Dvar of var_decl
  | Dstruct of ident * var_decl list

type 'info file =  'info decl list
