open Ast

module StringSet = Set.Make(String)

type pgeon_type = Term | Formula
type ctx = FormulaCtx | TermCtx

module SignatureMap = Map.Make(String)

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
      Printf.sprintf "%s %s. %s" binder var (expr_to_pgeon body)
  | EModal (kind, body) ->
      Printf.sprintf "%s(%s)" kind (expr_to_pgeon body)
  | EApp (name, []) -> name ^ "()"
  | EApp (name, args) ->
      let translated_args = List.map expr_to_pgeon args in
      Printf.sprintf "%s(%s)" name (String.concat ", " translated_args)
;;

let rec collect_signatures ctx acc = function
  | EApp (name, args) ->
      let builtins = ["and"; "or"; "not"; "imp"; "implies"; "equ"] in
      if List.mem name builtins then
        List.fold_left (collect_signatures FormulaCtx) acc args
      else
        let arity = List.length args in
        let ret_type = match ctx with FormulaCtx -> Formula | TermCtx -> Term in
        let acc = SignatureMap.add name (arity, ret_type) acc in
        List.fold_left (collect_signatures TermCtx) acc args
  | EBind (_, _, body) -> collect_signatures ctx acc body
  | EModal (_, body) -> collect_signatures ctx acc body
  | EVar _ -> acc
;;

module GroupKey = struct
  type t = int * pgeon_type
  let compare = compare
end
module GroupMap = Map.Make(GroupKey)

let group_signatures sig_map =
  SignatureMap.fold (fun name (arity, ret_type) acc ->
    let current_names = match GroupMap.find_opt (arity, ret_type) acc with
      | Some names -> names
      | None -> []
    in
    GroupMap.add (arity, ret_type) (name :: current_names) acc
  ) sig_map GroupMap.empty
;;

let format_type arity ret_type =
  let args_str = String.concat " " (List.init arity (fun _ -> "term")) in
  let ret_str = match ret_type with Term -> "term" | Formula -> "formula" in
  if arity > 0 then args_str ^ " -> " ^ ret_str
  else "-> " ^ ret_str
;;

let generate_header (prob : problem_decl) =
  let pure_formulas = List.map snd prob.formulas in
  let sig_map = List.fold_left (collect_signatures FormulaCtx) SignatureMap.empty pure_formulas in
  if SignatureMap.is_empty sig_map then ""
  else
    let grouped = group_signatures sig_map in
    let lines = GroupMap.fold (fun (arity, ret_type) names acc ->
      let sorted_names = List.sort String.compare names in
      let names_str = String.concat " " sorted_names in
      let type_str = format_type arity ret_type in
      let line = Printf.sprintf "function %s : %s\n" names_str type_str in
      line :: acc
    ) grouped [] in
    String.concat "" (List.rev lines) ^ "\n"
;;

let rec get_free_vars bound acc = function
  | EVar v -> 
      if StringSet.mem v bound then acc else StringSet.add v acc
  | EApp (_, args) -> 
      List.fold_left (get_free_vars bound) acc args
  | EModal (_, body) -> 
      get_free_vars bound acc body
  | EBind (binder, var, body) -> 
      get_free_vars (StringSet.add var bound) acc body
;;

let close_formula expr =
  let free_vars = get_free_vars StringSet.empty StringSet.empty expr in
  StringSet.fold (fun var acc_expr ->
    EBind ("forall", var, acc_expr)
  ) free_vars expr
;;

let print_problem (prob : problem_decl) (expected_status : string) =
  print_string (generate_header prob);
  Printf.printf "/* expected: %s */\n" expected_status;
  List.iter (fun (_, f) -> 
    let closed_f = close_formula f in 
    let s = expr_to_pgeon closed_f in
    Printf.printf "%s ;\n" s
  ) prob.formulas
;;