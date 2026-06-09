open Tptp_parser
open Ast

let debug = true

type target_prover = Pgeon | Twb

let target = ref Pgeon
let anon_files = ref []

let print_error_position lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  Printf.eprintf "Syntax error: line %d, column %d (character %d)\n"
    pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)
    pos.pos_cnum

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

let write_to_file target_mode output_filename ast status_str =
  if debug then Printf.eprintf "[DEBUG] Creating file: %s\n" output_filename;
  let oc = open_out output_filename in
  let old_stdout = Unix.dup Unix.stdout in
  Unix.dup2 (Unix.descr_of_out_channel oc) Unix.stdout;
  
  try
    begin match target_mode with
    | Pgeon -> Writer.print_problem ast status_str
    | Twb -> Twb_writer.print_problem ast status_str
    end;
    flush stdout;
    Unix.dup2 old_stdout Unix.stdout;
    close_out oc
  with e ->
    flush stdout;
    Unix.dup2 old_stdout Unix.stdout;
    close_out oc;
    raise e

let speclist = [
  ("--pgeon", Arg.Unit (fun () -> target := Pgeon), " Translate TPTP to Pgeon syntax (default)");
  ("--twb",   Arg.Unit (fun () -> target := Twb),   " Translate TPTP to TWB syntax");
]

let usage_msg = "Usage: " ^ Sys.argv.(0) ^ " [--pgeon | --twb] <file.p>"

let () =
  Arg.parse speclist (fun anonymous_arg -> anon_files := anonymous_arg :: !anon_files) usage_msg;

  match !anon_files with
  | [] -> 
      Arg.usage speclist usage_msg; 
      exit 1
  | _ :: _ :: _ -> 
      Printf.eprintf "Error: Only one file can be processed at a time.\n"; 
      exit 1
  | [filename] ->
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

        match status with
        | Status.Unsolved | Status.UnknownStatus ->
            if debug then Printf.eprintf "[DEBUG] Unsolved/Unknown status. Generating POS and NEG files.\n";
            
            let file_pos = "UNK_pos_" ^ base_name in
            write_to_file !target file_pos ast status_str;
            
            let file_neg = "UNK_neg_" ^ base_name in
            let negated_ast = negate_ast ast in
            write_to_file !target file_neg negated_ast status_str

        | _ ->
            let should_negate = 
              match status with
              | Status.Theorem -> true
              | _ -> false
            in
            let final_ast = if should_negate then negate_ast ast else ast in
            
            begin match !target with
            | Pgeon -> Writer.print_problem final_ast status_str
            | Twb -> Twb_writer.print_problem final_ast status_str
            end
            
      with
      | Parser.Error -> close_in_noerr ic; print_error_position lexbuf; exit 1
      | Lexer.Lexing_error msg -> close_in_noerr ic; print_error_position lexbuf; exit 1
      | e -> close_in_noerr ic; Printf.eprintf "Unexpected error: %s\n" (Printexc.to_string e); exit 1