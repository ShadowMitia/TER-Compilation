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

  let process_char c =
    match c with
    | "\\n" -> "\n"
    | "\\t" -> "\t"
    | "\\r" -> "\r"
    | "\\\\"  -> "\\"
    | _ as s -> print_string ("["^s^"]"); s

  let string_buffer = Buffer.create 80

}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let identifier = (alpha | ['_']) (alpha | digit | ['_'])*
let const_float = (digit+ ['.'] digit* | ['.']digit+)((['e'] | ['E'])['-']?digit+)?
let simple_char = [' '-'~']#['\'' '\\' '\"']
let complexe_char = ['\\'] (['\\'] | ['n'] | ['t'] | ['r'])
let const_string = ['"'] (simple_char | complexe_char)* ['"']

rule token = parse
  | '\n'             { newline lexbuf; token lexbuf }
  | [' ' '\t' '\r']+ { token lexbuf }
  | "/*"             { comment lexbuf }
  | "#"              { macro lexbuf }
  | "*/"             { raise (Lexical_error "Error: unmatched comment ending") }
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
  | "//"             { comment_line lexbuf }
  | "\""             { Buffer.reset string_buffer; CONST_STRING(string lexbuf) }
  | "'" simple_char "'" { CONST_CHAR (lexeme_char lexbuf 0) }
  | "'" complexe_char "'" { CONST_CHAR (String.get (process_char (lexeme lexbuf)) 0) }
  | (['-'])? digit+ (['u'] | ['U']) (['l'] | ['L']) { UNSIGNED_LONG_NUM (int64_of_number_string (lexeme lexbuf) 2) }
  | ['-']? digit+ (['l'] | ['L']) { LONG_NUM (int64_of_number_string (lexeme lexbuf) 1) }
  | digit+ (['u'] | ['U']) { UNSIGNED_NUM (int64_of_number_string (lexeme lexbuf) 1) }
  | ['-']? digit+ { NUM (int64_of_number_string (lexeme lexbuf) 0) }
  | ['-']? const_float      { NUM_FLOAT (float_of_string (lexeme lexbuf)) }
  | identifier       { keyword_or_ident (lexeme lexbuf) }
  | _                { char_error (lexeme lexbuf) }

and string = parse
           (* TODO: changer les char en leur valeur pour que ce soit correctement interprété *)
           | simple_char   { Buffer.add_string string_buffer (lexeme lexbuf); string lexbuf }
           | complexe_char { Buffer.add_string string_buffer (process_char (lexeme lexbuf));  string lexbuf }
           | "\""          { Buffer.contents string_buffer }
           | _             { raise (Lexical_error "Invalid character") }

and chara = parse
          | simple_char { (lexeme lexbuf) }
          | complexe_char { (process_char (lexeme lexbuf)) }

and comment_line = parse
                 | _            { comment_line lexbuf }
                 | ['\n' '\r']+ { token lexbuf }
                 | eof          { token lexbuf }

and comment = parse
            | "*/" { token lexbuf }
            | _    { comment lexbuf }
            | "/*" { raise (Lexical_error "Imbricated comments are forbiden") }
            | eof  { raise (Lexical_error "Unterminated comment") }

and macro = parse
          | _             { macro lexbuf }
          | ['\r' '\n']+  { token lexbuf }
