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
%token <int64>  NUM
%token <int64>  UNSIGNED_NUM
%token <int64>  UNSIGNED_LONG_NUM
%token <int64>  LONG_NUM
%token <float>  NUM_FLOAT
%token <string> CONST_STRING
%token <string> CONST_CHAR
%token VOID
%token CHAR SHORT INT LONG
%token DOUBLE
%token UNSIGNED
%token STRUCT EXTERN
%token SEMI STAR COMMA ARROW DOT
%token ASSIGN
%token LBRACKET RBRACKET LPAR RPAR L_SQ_BRACKET R_SQ_BRACKET
%token EQ NEQ
%token AND OR LT LTE GT GTE NOT BAND
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
   (* Variable declarations *)
   | decl_vars=variable_declaration SEMI { Dvar(decl_vars) }
   (* Struct declarations *)
   | STRUCT i=identifier LBRACKET decl_vars_list=list(terminated(variable_declaration, SEMI)) RBRACKET SEMI { Dstruct(i, decl_vars_list)  }
   (* Function declarations *)
   | fun_type=c_type fun_identifier=c_variable LPAR fun_args=separated_list(COMMA, variable_declaration) RPAR fun_block=block {  let t, i = unvar fun_type fun_identifier in Dfun(t, i, fun_args, Some fun_block)  }
   | fun_type=c_type fun_identifier=c_variable LPAR fun_args=separated_list(COMMA, variable_declaration) RPAR SEMI {  let t, i = unvar fun_type fun_identifier in Dfun(t, i, fun_args, None)  }
   (* Extern declarations *)
   | EXTERN fun_type=c_type fun_identifier=c_variable LPAR fun_args=separated_list(COMMA, variable_declaration) RPAR SEMI {  let t, i = unvar fun_type fun_identifier in Dfun(t, i, fun_args, None)  }
;

block:
   | LBRACKET; lb = list(terminated(variable_declaration, SEMI)); li = list(instruction); RBRACKET { (lb, li) }
;

instruction_:
   | SEMI { Sskip }
   | expr = expression { Sexpr expr }
   | IF LPAR expr = expression RPAR instr = instruction { Sif(expr, instr, None) }
   | IF LPAR expr = expression RPAR instr1 = instruction ELSE instr2 = instruction { Sif(expr, instr1, Some instr2) }
   | WHILE LPAR expr = expression RPAR instr = instruction { Swhile(expr, instr) }
   (*
                                                                                                   Swhile(Sexpr(1), instr) }
   | FOR LPAR SEMI SEMI RPAR instr = instruction {
                                         Swhile(Sexpr(1), instr) }
       *)
   | b = block { Sblock b }
   | RETURN expr = expression SEMI { Sreturn (Some expr) }
   | RETURN SEMI { Sreturn None }
;

instruction:
   | i = instruction_ { mk_loc i ($startpos, $endpos) }
;

l_expr:
    l = separated_list(COMMA, expression) { l }
;

  expression_:
   (* Manque le cas du short? *)
   | n = NUM { Econst(Cint(Signed, Int, n))  }
   | n = UNSIGNED_LONG_NUM { Econst(Cint(Unsigned, Long, n)) }
   | n = LONG_NUM { Econst(Cint(Signed, Long, n)) }
   | n = UNSIGNED_NUM { Econst(Cint(Unsigned, Int, n)) }
   | n = NUM_FLOAT { Econst(Cdouble(n))     }
   | c = CONST_CHAR { Econst(Cstring(c)) }  (* A CHANGER PROBABLMENT *)
   | c = CONST_STRING { Econst(Cstring(c)) } (* A CHANGER PROBABLEMENT *)
   | i = identifier { Eident(i) }
   | STAR expr = expression { Eunop(Deref, expr)  }
   | expr1 = expression L_SQ_BRACKET expr2 = expression R_SQ_BRACKET { Eunop(Deref, mk_loc (Ebinop(expr1, Add, expr2))  ($startpos, $endpos) ) }
   | expr = expression DOT id = identifier SEMI { Ebinop(expr, Dot, mk_loc (Eident id) id.info) }
   | expr = expression ARROW id = identifier SEMI { (Ebinop(mk_loc (Eunop (Deref, expr)) expr.info, Dot, mk_loc (Eident id) id.info )) }
   | expr1 = expression ASSIGN expr2 = expression { Eassign(expr1, expr2) }
   | i = identifier LPAR lexpr = l_expr RPAR   { Ecall( i, lexpr)  }
   | PLUSPLUS expr = expression { Eunop(PreInc, expr) }
   | MINUSMINUS expr = expression { Eunop(PreDec, expr) }
   | expr = expression PLUSPLUS { Eunop(PostInc, expr) }
   | expr = expression MINUSMINUS { Eunop(PostDec, expr) }
   | uop = unop expr = expression { Eunop(uop, expr) }
   | expr1 = expression op = binop expr2 = expression { Ebinop(expr1, op, expr2) }
   | SIZEOF LPAR cplxtype = cplx_type RPAR {(Esizeof cplxtype) }
   | LPAR cplxtype = cplx_type RPAR expr = expression { Ecast(cplxtype, expr) }
   | LPAR expr = expression_ RPAR { expr }
;

cplx_type:
   | t = c_type { t }
;

expression:
   | e = expression_ { mk_loc e ($startpos, $endpos) }
;

variable_declaration:
   | ctype=c_type cvar=c_variable { unvar ctype cvar }
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

%inline unop:
    | MINUS { Neg }
    | BAND  { Addr }
    | PLUS  { Pos }
    | NOT   { Not }
    | STAR  { Deref }
;

%inline binop:
    | PLUS  { Add   }
    | MINUS { Minus }
    | STAR  { Mult  }
    | DIV   { Div   }
    | MOD   { Mod   }
    | AND   { And   }
    | OR    { Or    }
    | EQ    { Eq    }
    | NEQ   { Neq   }
    | LT    { Lt    }
    | LTE   { Le    }
    | GT    { Gt    }
    | GTE   { Ge    }
;
