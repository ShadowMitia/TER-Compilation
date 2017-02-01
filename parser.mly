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

%token VOID
%token CHAR SHORT INT LONG
%token DOUBLE
%token UNSIGNED
%token STRUCT EXTERN
%token SEMI STAR USTAR COMMA ARROW DOT
%token ASSIGN
%token LB RB LP RP
%token EQ NEQ
%token AND OR LT LTE GT GTE NOT LAND LBRACKET RBRACKET
%token PLUS MINUS DIV MULT MOD UPLUS UMINUS /* MULT = STAR */
%token SIZEOF WHILE FOR IF ELSE RETURN
%token PLUSPLUS MINUSMINUS


/* Priorités */

%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT LTE GT GTE
%left PLUS MINUS
%left MULT DIV MOD
%left NOT PLUSPLUS MINUSMINUS ADDR USTAR UPLUS UMINUS
%right RP LB ARROW DOT
%right ustar uminus

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
    | STRUCT; i = ident; LB; l = list(terminated(decl_var, SEMI)); RB; SEMI  { Dstruct ( i, l) }
    | EXTERN; t = var_type; v = var; LP; l = separated_list(COMMA, decl_var); RP; SEMI { let t, i = unvar t v in Dfun(t, i, l, None) }
    | t=var_type; v=var; LP; l = separated_list(COMMA, decl_var); RP; b = block { let t, i = unvar t v in Dfun(t, i, l, None) }
    | t=var_type; v=var; LP; l = separated_list(COMMA, decl_var); RP; SEMI { let t, i = unvar t v in Dfun(t, i, l, None) }
;

block:
    | LB; lb = list(terminated(decl_var, SEMI)); li = list(instr); RB { (lb, li) }
;

instr_:
    | SEMI                               {  }
    | e = expr; SEMI                     {  }
    | IF; LP; e = expr; RP; i = instr    {  }
    | IF; LP; e = expr; RP; i = instr; ELSE; i2 = instr;  {  }
    | WHILE; LP; e = expr; RP; i = instr { }
    | FOR; LP; le = l_expr; SEMI;  e = expr; SEMI; le2 = l_expr; RP; i = instr { }
    | b = block                          { }
    | RETURN e = expr; SEMI              { }
;

instr:
    | i = instr_                       { mk_loc i ($startpos, $endpos) }
;

expr_:
    | i = ident                                { }
    | STAR ; e = expr                          { }
    | e1 = expr; LBRACKET; e2 = expr; RBRACKET { }
    | e = expr; DOT; i = instr                 { }
    | e = expr; ARROW; i = instr               { }
    | e = expr; ASSIGN; i = instr              { }
    | i = ident; LP; le = l_expr; RP           { }
    | PLUSPLUS; e = expr                       { }
    | MINUSMINUS; e = expr                     { }
    | e = expr; PLUSPLUS                       { }
    | e = expr; MINUSMINUS                     { }
    | LAND; e = expr;                          { }
    | NOT;  e = expr;                          { }
    | MINUS; e = expr;                         { }
    | PLUS; e = expr;                          { }
    | e1=expr; op=binop; e2=expr               { }
    | SIZEOF; LP; c=cplx_type; RP              { }
    | LP ; e = expr_ ; RP                      { }
;

expr:
    | e = expr_                        { mk_loc e ($startpos, $endpos) }
;

ident:
    | i = IDENT                        { mk_loc i ($startpos, $endpos) }
;

%inline binop:
    | PLUS  { Add   }
    | MINUS { Minus }
    | MULT  { Mult  }
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

var:
    | i = ident     { I ( i ) }
    | STAR; v = var { P ( v ) }
;

decl_var:
    | vt=var_type v = var { unvar vt v }
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
;

  cplx_type:
  | t = var_type STAR      { " " }
;
