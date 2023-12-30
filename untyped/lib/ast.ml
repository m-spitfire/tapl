type term =
  | TmVar of int
  | TmAbs of string * term
  | TmApp of term * term
[@@deriving show, eq]

type ir_term =
  | IRVar of string
  | IRAbs of string * ir_term
  | IRApp of ir_term * ir_term
[@@deriving show, eq]

let rec convert_ir ctx ir =
  match ir with
  | IRVar s -> TmVar (Util.get_ind ctx s 0)
  | IRAbs (arg, b) -> TmAbs (arg, convert_ir (arg :: ctx) b)
  | IRApp (t1, t2) -> TmApp (convert_ir ctx t1, convert_ir ctx t2)
;;
