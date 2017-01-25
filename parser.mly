/* Parseur pour le compilateur C */

%{
  open Ast


  let mk_loc e l = { loc = l; node = e }

  let loc e =
    mk_loc e (Parsing.symbol_start_pos (),Parsing.symbol_end_pos())

  let loc_i i e =
    mk_loc e (Parsing.rhs_start_pos i, Parsing.rhs_end_pos i)

  let loc_dummy e =
    mk_loc e (Lexing.dummy_pos, Lexing.dummy_pos)

  type pvar =
    I of ident
  | P of pvar

  let rec unvar t v =
    match v with
    | I i -> (t, i)
    | P vv -> unvar (Tpointer t) vv

%}

%token EOF
%token <string> IDENT
%token <int32>  NUM

%token VOID
%token CHAR SHORT INT LONG
%token DOUBLE
%token UNSIGNED
%token STRUCT EXTERN
%token SEMI STAR USTAR COMMA
%token ASSIGN
%token LB RB LP RP
%token EQUAL NOT_EQUAL
%token AND OR LT LTE GT GTE
%token PLUS MINUS DIV MOD UPLUS UMINUS /* MULT = STAR */


/* Priorités */

   %right ASSIGN
   %left OR
   %left AND
   %left EQUAL NOT_EQUAL
   %left LT LTE GT GTE
   %left PLUS MINUS
   %left MULT DIV MOD
   %left NOT PLUSPLUS MINUSMINUS ADDR USTAR UPLUS UMINUS
   %right RP LB ARROW DOT


/* Point d'entrée */

%start file
%type <Ast.loc Ast.file> file

%%

file:
| l = list(decl) EOF { l }
;

  /* déclarations */

decl:
    | d = decl_var;  SEMI { Dvar(d) }
    | STRUCT; i = IDENT; LB; l = list(terminated(decl_var, SEMI)); RB; SEMI  { Dstruct ((loc i), l) }
    | EXTERN; t = var_type; v = var; LP; l = separated_list(COMMA, decl_var); RP; SEMI { let t, i = unvar t v in Dfun(t, i, l, None) }
    | t=var_type; v=var; LP; l = separated_list(COMMA, decl_var); RP; b = block { let t, i = unvar t v in Dfun(t, i, l, None) }
    | t=var_type; v=var; LP; l = separated_list(COMMA, decl_var); RP; SEMI { let t, i = unvar t v in Dfun(t, i, l, None) }
;

block:
    | LB; lb = list(terminated(decl_var, SEMI)); li = list(instr); RB { (lb, li) }
;

instr:
    | SEMI { loc Sskip }
    | e = expr; SEMI { loc (Sexpr e) }
;

expr:
    | i = IDENT                        { (loc (Eident (loc i))) }
    | e1 = expr; op = binop; e2 = expr { (loc (Ebinop (e1, op, e2))) }
;

%inline binop:
    | PLUS { Add }

var:
    | i = IDENT { I( (loc i) ) }
    | STAR; v = var { P ( v ) }

decl_var:
    | vt=var_type v = var{ unvar vt v }
;

var_type:
  | s=signedness t=integer_type  { Tinteger(s, t) }
  | VOID                         { Tvoid }
  | DOUBLE                       { Tdouble }
;

signedness:
  |          { Signed }
  | UNSIGNED { Unsigned }
;

integer_type:
  | CHAR  { Char }
  | SHORT { Short }
  | INT   { Int }
  | LONG  { Long }
;


  l_expr:
    l = separated_list(COMMA, expr) { l }
