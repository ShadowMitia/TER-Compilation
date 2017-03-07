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

  let int32_of_number_string num ign =
    Int32.of_string (String.sub num 0 ((String.length num) - ign))

  let int64_of_number_string num ign =
    Int64.of_string (String.sub num 0 ((String.length num) - ign))

}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let identifier = (alpha | ['_']) (alpha | digit | ['_'])*
let const_float = (digit+ ['.'] digit* | ['.']digit+)((['e'] | ['E'])['-']?digit+)?
let const_char = ([' '-'~']#['\'' '\\' '\"'])
let const_string = ['\"'] (const_char | [' '] | ['\t'])+ ['\"']


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
  | "&"              { BAND }
  | "->"             { ARROW }
  | "."              { DOT }
  | "["              { L_SQ_BRACKET }
  | "]"              { R_SQ_BRACKET }
  | (['-'])? digit+ (['u'] | ['U']) (['l'] | ['L']) { UNSIGNED_LONG_NUM (int64_of_number_string (lexeme lexbuf) 2) }
  | ['-']? digit+ (['l'] | ['L']) { LONG_NUM (int64_of_number_string (lexeme lexbuf) 1) }
  | digit+ (['u'] | ['U']) { UNSIGNED_NUM (int64_of_number_string (lexeme lexbuf) 1) }
  | ['-']? digit+ { NUM (int64_of_number_string (lexeme lexbuf) 0) }
  | ['-']? const_float      { NUM_FLOAT (float_of_string (lexeme lexbuf)) }
  | const_string     { CONST_STRING (lexeme lexbuf)  }
  | identifier       { keyword_or_ident(lexeme lexbuf) }
  | "/*"             { comment lexbuf }
  | "#"              { macro lexbuf }
  | "*/"             { failwith "Error: unmatched comment ending" }
  | _                { char_error (lexeme lexbuf) }

and comment = parse
            | "*/" { token lexbuf }
            | _    { comment lexbuf }
            | "/*" { failwith "Error: Imbricated comments are forbiden" }
            | eof  { failwith "Lexical error : unterminated comment" }

and macro = parse
          | _             { macro lexbuf }
          | ['\r' '\n']+  { token lexbuf }
