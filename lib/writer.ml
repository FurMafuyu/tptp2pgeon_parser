open Ast

(* ouais heu ca a voir *)
module Signature = Set.Make(struct
  type t = string * int
  let compare = compare
end)

let rec expr_to_pgeon = function
  | EApp ("and", [a; b]) -> 
    Printf.sprintf "and(%s, %s)" (expr_to_pgeon a) (expr_to_pgeon b)
  | EApp ("not", [f]) -> 
      Printf.sprintf "not(%s)" (expr_to_pgeon f)
  | EApp ("or", [a; b]) ->
      Printf.sprintf "or(%s, %s)" (expr_to_pgeon a) (expr_to_pgeon b)
  | EApp ("implies", [a; b]) ->
      Printf.sprintf "imp(%s, %s)" (expr_to_pgeon a) (expr_to_pgeon b)
  | EApp ("equ", [a; b]) ->
    Printf.sprintf "equ(%s, %s)" (expr_to_pgeon a) (expr_to_pgeon b)
  | EVar v -> v
  | EBind (binder, var, body) ->
      Printf.sprintf "%s %s. %s" binder var (expr_to_pgeon body) (* faire tests *)
  | EModal (kind, body) ->
      Printf.sprintf "%s(%s)" kind (expr_to_pgeon body)
  | EApp (name, []) -> name ^ "()"
  | EApp (name, args) ->
      let translated_args = List.map expr_to_pgeon args in
      Printf.sprintf "%s(%s)" name (String.concat ", " translated_args)
  
;;

let rec collect_signatures acc = function
  | EApp (name, args) ->
      let builtins = ["and"; "or"; "not"; "imp"; "implies"; "equ"] in
      let acc = 
        if List.mem name builtins then acc 
        else Signature.add (name, List.length args) acc 
      in
      List.fold_left collect_signatures acc args
  | EBind (_, _, body) -> collect_signatures acc body
  | EModal (_, body) -> collect_signatures acc body
  | EVar _ -> acc

let generate_header (prob : problem_decl) =
  let all_sigs = List.fold_left collect_signatures Signature.empty prob.formulas in
  if Signature.is_empty all_sigs then ""
  else
    let names = Signature.elements all_sigs 
                |> List.map fst 
                |> String.concat " " in
    Printf.sprintf "function %s: -> formula\n\n" names



let print_problem (prob : problem_decl) =
  print_string (generate_header prob);
  List.iter (fun f -> 
    let s = expr_to_pgeon f in
    Printf.printf "%s ;\n" s
  ) prob.formulas
;;