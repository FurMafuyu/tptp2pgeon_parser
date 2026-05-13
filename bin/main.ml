open Tptp_parser

let () =
  if Array.length Sys.argv < 2 then
    Printf.printf "Usage: %s <fichier.p>\n" Sys.argv.(0)
  else
    let filename = Sys.argv.(1) in
    let ic = open_in filename in
    try
      let lexbuf = Lexing.from_channel ic in
      let ast = Parser.problem Lexer.token lexbuf in
      
      Writer.print_problem ast;
      
      close_in ic
    with
    | e ->
        close_in ic;
        Printf.printf "Erreur lors du traitement : %s\n" (Printexc.to_string e);
        exit 1