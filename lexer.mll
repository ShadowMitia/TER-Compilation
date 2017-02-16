(* Analyse lexicale *)
{

  open Lexing
  open Parser
  open Ast

  (* Erreurs lexicales *)

  exception Lexical_error of string

  let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum; pos_cnum = 0 }

  let char_error s = raise (Lexical_error ("illegal character sequence: " ^ s))

  let keyword_or_ident s =
    try
      List.assoc s [
                   "int", INT;
                   "void", VOID;
                   "char", CHAR;
                   "short", SHORT;
                   "long", LONG;
                   "double", DOUBLE;
                   "unsigned", UNSIGNED;
                   "struct", STRUCT;
                   "extern", EXTERN;
		   "sizeof", SIZEOF;
                   "if", IF;
                   "else", ELSE;
                   "while", WHILE;
                   "for", FOR;
                   "return", RETURN;
                  ];
    with Not_found -> IDENT s

}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let identifier = (alpha | ['_']) (alpha | digit | ['_'])*
let const_int = (digit+) (['u'] | ['U'])?(['l'] | ['L'])?
let const_float = (digit+['.']digit* | ['.']digit+)((['e'] | ['E'])['-']?digit+)?
let char = ([' '-'~']#['\'' '\\' '\"'])

rule token = parse
  | '\n'             { newline lexbuf; token lexbuf }
  | [' ' '\t' '\r']+ { token lexbuf }
  | eof              { EOF }
  | ";"              { SEMI }
  | "="              { ASSIGN }
  | "*"              { STAR }
  | "{"              { LBRACKET }
  | "}"              { RBRACKET }
  | "("              { LPAR }
  | ")"              { RPAR }
  | ","              { COMMA }
  | "-"              { MINUS }
  | "+"              { PLUS }
  | "*"              { STAR }
  | "/"              { DIV }
  | "=="             { EQ }
  | "!="             { NEQ }
  | ">"              { GT }
  | ">="             { GTE }
  | "<"              { LT }
  | "<="             { LTE }
  | "%"              { MOD }
  | "&&"             { AND }
  | "||"             { OR }
  | "++"             { PLUSPLUS }
  | "--"             { MINUSMINUS }
  | "!"              { NOT }
  | "&"              { LAND }
  | "->"             { ARROW }
  | "."              { DOT }
  | "["              { L_SQ_BRACKET }
  | "]"              { R_SQ_BRACKET }
  | const_int        { NUM (Int32.of_string (lexeme lexbuf)) }
  | const_float      { NUM_FLOAT (float_of_string (lexeme lexbuf)) }
  | identifier       { keyword_or_ident(lexeme lexbuf) }
  | "/*"             { comment lexbuf }
  | "#"              { macro lexbuf }
  | _                { char_error(lexeme lexbuf) }

and comment = parse
            | "*/" { token lexbuf }
            | _    { comment lexbuf }
            | "/*" { failwith "Error: Imbricated comments are forbiden" }
            | eof  { failwith "Lexical error : unterminated comment" }

and macro = parse
          | _    { macro lexbuf }
          | ['\r' '\n']+ { token lexbuf }
