(* Programme principal *)

open Format
open Lexing
open Lexer
open Parser
open Ast
open Typing


let usage = "usage: compilo [options] file.c"

let parse_only = ref false
let type_only = ref false
let inline_mode = ref false

let spec =
  ["-parse-only", Arg.Set parse_only, "  stops after parsing";
   "-type-only", Arg.Set type_only, "  stops after typing";
   "-inline", Arg.Set inline_mode, " all functions are inlined";
]

let file =
  let file = ref None in
  let set_file s =
    if not (Filename.check_suffix s ".c") then
      raise (Arg.Bad "no .c extension");
    file := Some s
  in
  Arg.parse spec set_file usage;
  match !file with Some f -> f | None -> Arg.usage spec usage; exit 1

let report_loc (b,e) =
  if b = dummy_pos || e = dummy_pos then
  eprintf "File \"%s\":\n" file
  else
  let l = b.pos_lnum in
  let fc = b.pos_cnum - b.pos_bol + 1 in
  let lc = e.pos_cnum - b.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" file l fc lc

let () =
  let c = open_in file in
  let lb = Lexing.from_channel c in
  try
    let p = Parser.file Lexer.token lb in
    close_in c;
    if !parse_only then exit 0;
    let tp = Typing.type_prog p in ();
                                   if !type_only then exit 0;
                                   let compiler_args = {
                                       inline = !inline_mode
                                     } in
    let code = Compile.compile_prog tp compiler_args in
    let out_file = Filename.chop_suffix file ".c" in
    Amd64.print_in_file ~file:(out_file ^ ".s") code
  with
    | Lexical_error s ->
	report_loc (lexeme_start_p lb, lexeme_end_p lb);
	eprintf "lexical error: %s\n@." s;
	exit 1
    | Parser.Error ->
	report_loc (lexeme_start_p lb, lexeme_end_p lb);
	eprintf "syntax error\n@.";
	exit 1
    | Typing.TypeError (loc, msg) ->
       report_loc loc;
       eprintf "typing error: %s\n@." msg;
    | e ->
	eprintf "Anomaly: %s\n@." (Printexc.to_string e);
	exit 2
