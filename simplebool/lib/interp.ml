open Ast

type binding = NameBind
type context = (string * binding) list

exception NoRuleApplies

let rec pickfreshname ctx x =
  match List.find_opt (fun a -> fst a = x) ctx with
  | Some _ -> pickfreshname ctx (x ^ "'")
  | None -> (x, NameBind) :: ctx, x
;;

let index2name ctx x = List.nth ctx x |> fst

let rec printtm ctx t =
  match t with
  | TmAbs (x, _, t1) ->
    let ctx', x' = pickfreshname ctx x in
    print_string ("(lambda " ^ x' ^ ". ");
    printtm ctx' t1;
    print_string ")"
  | TmApp (t1, t2) ->
    print_string "(";
    printtm ctx t1;
    print_string " ";
    printtm ctx t2;
    print_string ")"
  | TmVar x -> print_string (index2name ctx x)
  | TmTrue -> print_string "true"
  | TmFalse -> print_string "false"
  | TmIf (c, t, f) ->
    print_string "if ";
    printtm ctx c;
    print_string " then ";
    printtm ctx t;
    print_string " else ";
    printtm ctx f
;;

let term_shift d t =
  let rec walk c t =
    match t with
    | TmVar x -> if x >= c then TmVar (x + d) else TmVar x
    | TmAbs (x, typ, t1) -> TmAbs (x, typ, walk (c + 1) t1)
    | TmApp (t1, t2) -> TmApp (walk c t1, walk c t2)
    | TmIf (cond, t1, t2) -> TmIf (walk c cond, walk c t1, walk c t2)
    | (TmTrue | TmFalse) as t -> t
  in
  walk 0 t
;;

let term_sub j s t =
  let rec walk c t =
    match t with
    | TmVar x -> if x = j + c then term_shift c s else TmVar x
    | TmAbs (x, typ, t1) -> TmAbs (x, typ, walk (c + 1) t1)
    | TmApp (t1, t2) -> TmApp (walk c t1, walk c t2)
    | TmIf (cond, t1, t2) -> TmIf (walk c cond, walk c t1, walk c t2)
    | (TmTrue | TmFalse) as t -> t
  in
  walk 0 t
;;

let term_sub_top s t = term_shift (-1) (term_sub 0 (term_shift 1 s) t)

let isval = function
  | TmAbs (_, _, _) -> true
  | _ -> false
;;

let rec eval1 = function
  | TmApp (TmAbs (_, _, t12), v2) when isval v2 -> term_sub_top v2 t12
  | TmApp (v1, t2) when isval v1 ->
    let t2' = eval1 t2 in
    TmApp (v1, t2')
  | TmApp (t1, t2) -> TmApp (eval1 t1, t2)
  | _ -> raise NoRuleApplies
;;

let rec eval2 = function
  | TmAbs (_, _, _) as t -> t
  | TmApp (t1, t2) ->
    (match eval2 t1 with
     | TmAbs (_, _, t12) -> term_sub_top (eval2 t2) t12 |> eval2
     | _ -> raise NoRuleApplies)
  | _ -> raise NoRuleApplies
;;

let evalbig t =
  try eval2 t with
  | NoRuleApplies -> t
;;

let rec evalsmall t =
  let t'opt =
    try Some (eval1 t) with
    | NoRuleApplies -> None
  in
  match t'opt with
  | Some t' -> evalsmall t'
  | None -> t
;;
