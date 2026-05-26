open Tptp_parser
open Ast

let debug = true

let print_error_position lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  Printf.eprintf "Syntax error: line %d, column %d (character %d)\n"
    pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)
    pos.pos_cnum

(* Fonction utilitaire pour appliquer la négation (pour factoriser le code) *)
let negate_ast ast =
  let closed_formulas = List.map Writer.close_formula ast.formulas in
  match closed_formulas with
  | [] -> ast
  | [seule_formule] -> { formulas = [EApp ("not", [seule_formule])] }
  | premiere :: reste ->
      let grosse_conjonction = 
        List.fold_left (fun acc_expr f -> EApp ("and", [acc_expr; f])) premiere reste
      in
      { formulas = [EApp ("not", [grosse_conjonction])] }

(* Fonction pour écrire l'AST dans un fichier spécifique plutôt que sur stdout *)
let write_to_file output_filename ast status_str =
  if debug then Printf.eprintf "[DEBUG] Creating file: %s\n" output_filename;
  let oc = open_out output_filename in
  (* On duplique temporairement stdout vers notre fichier pour utiliser Writer.print_problem *)
  let old_stdout = Unix.dup Unix.stdout in
  Unix.dup2 (Unix.descr_of_out_channel oc) Unix.stdout;
  
  try
    Writer.print_problem ast status_str;
    flush stdout;
    Unix.dup2 old_stdout Unix.stdout;
    close_out oc
  with e ->
    flush stdout;
    Unix.dup2 old_stdout Unix.stdout;
    close_out oc;
    raise e

let () =
  if Array.length Sys.argv < 2 then
    Printf.printf "Usage: %s <file.p>\n" Sys.argv.(0)
  else
    let filename = Sys.argv.(1) in
    let base_name = Filename.basename filename in
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

    let ic = open_in filename in
    let lexbuf = Lexing.from_channel ic in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };

    try
      let ast = Parser.problem Lexer.token lexbuf in
      close_in ic;

      (* AIGUILLAGE INTELLIGENT *)
      match status with
      | Status.Unsolved | Status.UnknownStatus ->
          if debug then Printf.eprintf "[DEBUG] Unsolved/Unknown status. Generating POS and NEG files.\n";
          
          (* 1. Version positive (sans négation) *)
          let file_pos = "UNK_pos_" ^ base_name in
          write_to_file file_pos ast status_str;
          
          (* 2. Version négative (avec négation) *)
          let file_neg = "UNK_neg_" ^ base_name in
          let negated_ast = negate_ast ast in
          write_to_file file_neg negated_ast status_str

      | _ ->
          (* Comportement standard pour les statuts connus (écriture sur stdout) *)
          let should_negate = 
            match status with
            | Status.Theorem -> true
            | _ -> false
          in
          let final_ast = if should_negate then negate_ast ast else ast in
          Writer.print_problem final_ast status_str
          
    with
    | Parser.Error -> close_in_noerr ic; print_error_position lexbuf; exit 1
    | Lexer.Lexing_error msg -> close_in_noerr ic; print_error_position lexbuf; exit 1
    | e -> close_in_noerr ic; Printf.eprintf "Unexpected error: %s\n" (Printexc.to_string e); exit 1