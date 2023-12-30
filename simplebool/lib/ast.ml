type ty =
  | TyBool
  | TyArr of ty * ty
[@@deriving show, eq]

type term =
  | TmTrue
  | TmFalse
  | TmIf of term * term * term
  | TmVar of int
  | TmAbs of string * ty * term
  | TmApp of term * term
[@@deriving show, eq]

type ir_term =
  | IRVar of string
  | IRAbs of string * ty * ir_term
  | IRApp of ir_term * ir_term
  | IRTrue
  | IRFalse
  | IRIf of ir_term * ir_term * ir_term
[@@deriving show, eq]

let rec convert_ir ctx ir =
  match ir with
  | IRVar s -> TmVar (Util.get_ind ctx s 0)
  | IRAbs (arg, t, b) -> TmAbs (arg, t, convert_ir (arg :: ctx) b)
  | IRApp (t1, t2) -> TmApp (convert_ir ctx t1, convert_ir ctx t2)
  | IRIf (c, t, f) -> TmIf (convert_ir ctx c, convert_ir ctx t, convert_ir ctx f)
  | IRTrue -> TmTrue
  | IRFalse -> TmFalse
;;
