open Tptp_parser

let print_error_position lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  Printf.eprintf "Syntax error: line %d, column %d (character %d)\n"
    pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)
    pos.pos_cnum

let () =
  if Array.length Sys.argv < 2 then
    Printf.printf "Usage: %s <file.p>\n" Sys.argv.(0)
  else
    let filename = Sys.argv.(1) in
    let ic = open_in filename in
    let lexbuf = Lexing.from_channel ic in
    
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };

    try
      let ast = Parser.problem Lexer.token lexbuf in
      Writer.print_problem ast;
      close_in ic
    with
    | Parser.Error ->
        close_in ic;
        print_error_position lexbuf;
        Printf.eprintf "Parser error: The structural framework of the formula is invalid here.\n";
        exit 1

    | Lexer.Lexing_error msg ->
        close_in ic;
        print_error_position lexbuf;
        Printf.eprintf "Lexical error: %s\n" msg;
        exit 1

    | e ->
        close_in ic;
        Printf.eprintf "Unexpected error: %s\n" (Printexc.to_string e);
        exit 1