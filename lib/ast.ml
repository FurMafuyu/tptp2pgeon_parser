type expr =
  | EVar of string
  | EApp of string * expr list
  | EBind of string * string * expr (* quantificateur x nom x form *)
  | EModal of string * expr   

type problem_decl = {
  formulas : expr list;                
}



(*
  VERS .TXT pour pgeon
*)