open Tptp_parser
open Ast

let debug = false

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
    let status = Status.extract_status filename in
    
    let status_str = 
      match status with
      | Status.Unsatisfiable -> "Unsatisfiable"
      | Status.Theorem       -> "Theorem"
      | Status.NonTheorem    -> "NonTheorem"
      | Status.Satisfiable   -> "Satisfiable"
      | Status.Unsolved      -> "Unsolved"
      | Status.UnknownStatus -> "UnknownStatus"
    in
    
    let should_negate = 
      match status with
      | Status.Unsatisfiable -> 
          if debug then Printf.eprintf "[DEBUG] Detected Unsatisfiable status. Keeping formulas as they are.\n";
          false
      | Status.Theorem -> 
          if debug then Printf.eprintf "[DEBUG] Detected Theorem status (S4 Constant if modal). Enabling negation.\n";
          true
      | Status.NonTheorem ->
          if debug then Printf.eprintf "[DEBUG] Formula is Non-Theorem, keeping formula as they are, should succeed.\n";
          false
      | Status.Satisfiable ->
          if debug then Printf.eprintf "[DEBUG] Satisfiable formula, keeping formula as they are, should fail/timeout.\n";
          false
      | Status.Unsolved | Status.UnknownStatus ->
          if debug then Printf.eprintf "[DEBUG] Error: Status is Unsolved / Unknown. Aborting.\n";
          exit 1
    in

    let ic = open_in filename in
    let lexbuf = Lexing.from_channel ic in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };

    try
      let ast = Parser.problem Lexer.token lexbuf in
      
      let final_ast = 
        if should_negate then
          let closed_formulas = List.map Writer.close_formula ast.formulas in
          match closed_formulas with
          | [] -> ast
          | [seule_formule] -> { formulas = [EApp ("not", [seule_formule])] }
          | premiere :: reste ->
              let grosse_conjonction = 
                List.fold_left (fun acc_expr f -> EApp ("and", [acc_expr; f])) premiere reste
              in
              { formulas = [EApp ("not", [grosse_conjonction])] }
        else 
          ast
      in

      Writer.print_problem final_ast status_str;
      close_in ic
    with
    | Parser.Error -> close_in ic; print_error_position lexbuf; exit 1
    | Lexer.Lexing_error msg -> close_in ic; print_error_position lexbuf; exit 1
    | e -> close_in ic; Printf.eprintf "Unexpected error: %s\n" (Printexc.to_string e); exit 1