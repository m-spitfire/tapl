open Ast

type binding = VarBind of Ast.ty

exception TypeError

let get_ty ctx i =
  match List.nth ctx i |> snd with
  | VarBind t -> t
;;

let rec typeof ctx t =
  match t with
  | TmTrue | TmFalse -> TyBool
  | TmIf (cond, th, el) ->
    if typeof ctx cond = TyBool
    then (
      let thty = typeof ctx th in
      if typeof ctx el = thty then thty else raise TypeError)
    else raise TypeError
  | TmVar i -> get_ty ctx i
  | TmAbs (x, arg_ty, body) ->
    let ctx' = (x, VarBind arg_ty) :: ctx in
    let bod_ty = typeof ctx' body in
    TyArr (arg_ty, bod_ty)
  | TmApp (t1, t2) ->
    let ty1 = typeof ctx t1 in
    (match ty1 with
     | TyArr (arg_ty, body_ty) ->
       let ty2 = typeof ctx t2 in
       if ty2 = arg_ty then body_ty else raise TypeError
     | _ -> raise TypeError)
;;
