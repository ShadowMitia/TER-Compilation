/* Parseur pour le compilateur C */

%{
  open Ast


  let mk_loc e l = { info = l; node = e }

  type pvar =
  | I of ident
  | P of pvar

  let rec unvar t v =
    match v with
    | I i -> (t, i)
    | P vv -> unvar (Tpointer t) vv

%}

%token EOF
%token <string> IDENT
%token <int32>  NUM
%token <float> NUM_FLOAT

%token VOID
%token CHAR SHORT INT LONG
%token DOUBLE
%token UNSIGNED
%token STRUCT EXTERN
%token SEMI STAR COMMA ARROW DOT
%token ASSIGN
%token LBRACKET RBRACKET LPAR RPAR L_SQ_BRACKET R_SQ_BRACKET
%token EQ NEQ
%token AND OR LT LTE GT GTE NOT LAND
%token PLUS MINUS DIV MOD
%token SIZEOF WHILE FOR IF ELSE RETURN
%token PLUSPLUS MINUSMINUS


/* Priorités */

%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT LTE GT GTE
%left PLUS MINUS
%left STAR DIV MOD
%left NOT PLUSPLUS MINUSMINUS
%right RPAR LBRACKET ARROW DOT

/* Point d'entrée */

%start file
%type <Ast.loc Ast.file> file

%%

file:
    | l = list(declarations) EOF { l }
;

/* déclarations */

declarations:
   | decl_vars=declare_variable SEMI { Dvar(decl_vars) }
;


declare_variable:
    ctype=c_type cvar=c_variable { unvar ctype cvar }
;

c_variable:
    | i = identifier       { I ( i ) }
    | STAR; v = c_variable { P ( v ) }
;

c_type:
   | VOID                { Tvoid }
   | cinttype=c_int_type { cinttype }
   | DOUBLE              { Tdouble }
   | STRUCT i=identifier { Tstruct i}
   ;

     identifier:
   |   i = IDENT { mk_loc i ($startpos, $endpos) }
   ;

signedness:
   |          { Signed }
   | UNSIGNED { Unsigned }
;

c_int_type:
   | s=signedness cit=c_int_types { Tinteger(s, cit) }
;

%inline c_int_types:
    | CHAR  { Char }
    | SHORT { Short }
    | INT   { Int }
    | LONG  { Long }
;

(* declaration: *)
(*     | d = decl_var;  SEMI { Dvar(d) } *)
(*     | STRUCT; i = ident; LBRACKET; l = list(terminated(decl_var, SEMI)); RBRACKET; SEMI  { Dstruct (i, l) } *)
(*     | EXTERN; t = var_type; v = var; LPAR; l = separated_list(COMMA, decl_var); RPAR; SEMI { let t, i = unvar t v in Dfun(t, i, l, None) } *)
(*     | t=var_type; v=var; LPAR; l = separated_list(COMMA, decl_var); RPAR; b = block { let t, i = unvar t v in Dfun(t, i, l, Some b) } *)
(* ; *)

(* block: *)
(*     | LBRACKET; lb = list(terminated(decl_var, SEMI)); li = list(instr); RBRACKET { (lb, li) } *)
(* ; *)

(* instr_: *)
(*     | SEMI                                                                      { (Sskip) } *)
(*     | e = expr; SEMI                                                            { (Sexpr e) } *)
(*     (\*| IF; LP; e = expr; RP; i = instr                                           { (Sif(e, i, None)) } *)
(*     | IF; LP; e = expr; RP; i = instr; ELSE; i2 = instr;                        { (Sif(e, i, Some i2)) } *)
(*     | WHILE; LP; e = expr;  RP; i = instr                                       { (Swhile(e, i)) } *)
(*     | FOR; LP; le = l_expr; SEMI;  e = expr; SEMI; le2 = l_expr; RP; i = instr  { (Swhile(e, i)) } *)
(*     | b = block                                                                 { (Sblock b) } *)
(*     | RETURN e = expr SEMI                                                     { (Sreturn (Some e)) } *)
(*     | RETURN SEMI                                                               { (Sreturn None) }*\) *)
(* ; *)

(* instr: *)
(*     | i = instr_                               { mk_loc i ($startpos, $endpos) } *)
(* ; *)

(* expr_: *)
(*     | i = ident                                    { mk_loc (Eident(i)) ($startpos, $endpos)} *)
(*     //| e1 = expr; LB; e2 = expr; RBRACKET   { Egetarr(mk_loc e1.node e1.info, mk_loc e2.node e2.info) } *)
(*     //| e = expr; DOT; i = ident                   { Eident(mk_loc i.node i.info) } *)
(*     //| e = expr; ARROW; i = ident                 { Eunop(Deref, Eident(mk_loc i.node i.info)) } *)
(*     //| e = expr; ASSIGN; e2 = expr                { Eassign(mk_loc e.node e.info, mk_loc e2.node e2.info) } *)
(*     //| i = ident; LP; le = l_expr; RP             { Ecall(i, le) } *)
(*     //| PLUSPLUS; e = expr                         { Eunop(PreInc, e)  } *)
(*     //| MINUSMINUS; e = expr                       { Eunop(PreDec, e)  } *)
(*     //| e = expr; PLUSPLUS                         { Eunop(PostInc, e) } *)
(*     //| e = expr; MINUSMINUS                       { Eunop(PostDec, e) } *)
(*     //| u=unop; e = expr;                          { Eunop(u, e) } *)
(*       | e1=expr; op=binop; e2=expr                   { mk_loc (Ebinop (mk_loc e1.node e1.info, op, mk_loc e2.node e2.info)) ($startpos, $endpos) } *)
(*     //| SIZEOF; LP; c=cplx_type; RP                { Esizeof(c) } *)
(*     //| n=NUM                                      { Econst(mk_loc Cint(n) ($startpos, $endpos)) } *)
(*     //| n=NUM_FLOAT                                { Econst(mk_loc Cdouble(n) ($startpos, $endpos)) } *)
(* ; *)

(* expr: *)
(*     | e = expr_                        { mk_loc e ($startpos, $endpos) } *)
(* ; *)

(* ident: *)
(*     | i = IDENT                        { mk_loc i ($startpos, $endpos) } *)
(* ; *)

(* %inline unop: *)
(*     | MINUS { Neg } *)
(*     | LAND  { Addr } *)
(*     | PLUS  { Pos } *)
(*     | NOT   { Not } *)
(*     | STAR  { Deref } *)
(*     ; *)

(* %inline binop: *)
(*     | PLUS  { Add   } *)
(*     | MINUS { Minus } *)
(*     | MULT  { Mult  } *)
(*     | DIV   { Div   } *)
(*     | MOD   { Mod   } *)
(*     | AND   { And   } *)
(*     | OR    { Or    } *)
(*     | EQ    { Eq    } *)
(*     | NEQ   { Neq   } *)
(*     | LT    { Lt    } *)
(*     | LTE   { Le    } *)
(*     | GT    { Gt    } *)
(*     | GTE   { Ge    } *)
(* ; *)

(* var: *)
(*     | i = ident     { I ( i ) } *)
(*     | STAR; v = var { P ( v ) } *)
(* ; *)

(* decl_var: *)
(*     | var_t=var_type var=var { unvar var_t var } *)
(* ; *)

(* var_type: *)
(*     | s=signedness i_t=integer_type  { Tinteger(s, i_t) } *)
(*     | VOID                           { Tvoid } *)
(*     | DOUBLE                         { Tdouble } *)
(* ; *)

(* signedness: *)
(*     |          { Signed } *)
(*     | UNSIGNED { Unsigned } *)
(* ; *)

(* integer_type: *)
(*     | CHAR  { Char } *)
(*     | SHORT { Short } *)
(*     | INT   { Int } *)
(*     | LONG  { Long } *)
(* ; *)

(* l_expr: *)
(*     l = separated_list(COMMA, expr) { l } *)
(* ; *)

(* cplx_type: *)
(*     | t=var_type    { t } *)
(* ; *)
