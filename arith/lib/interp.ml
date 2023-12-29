open Ast

let rec isnumericalval = function
  | TmZero -> true
  | TmSucc t1 -> isnumericalval t1
  | _ -> false
;;

let isval = function
  | TmTrue -> true
  | TmFalse -> true
  | t -> isnumericalval t
;;

exception NoRuleApplies

let rec eval1 = function
  | TmIf (TmTrue, t2, _) -> t2
  | TmIf (TmFalse, _, t3) -> t3
  | TmIf (t1, t2, t3) -> TmIf (eval1 t1, t2, t3)
  | TmSucc t1 -> TmSucc (eval1 t1)
  | TmPred TmZero -> TmZero
  | TmPred (TmSucc nv) when isnumericalval nv -> nv
  | TmPred t1 -> TmPred (eval1 t1)
  | TmIsZero TmZero -> TmTrue
  | TmIsZero (TmSucc nv) when isnumericalval nv -> TmFalse
  | TmIsZero t1 -> TmIsZero (eval1 t1)
  | _ -> raise NoRuleApplies
;;

let rec eval2 = function
  | v when isval v -> v
  | TmIf (t1, t2, t3) ->
    (match eval2 t1 with
     | TmTrue -> eval2 t2
     | TmFalse -> eval2 t3
     | _ -> raise NoRuleApplies)
  | TmSucc t1 ->
    let t1val = eval2 t1 in
    if isnumericalval t1val then TmSucc t1val else raise NoRuleApplies
  | TmPred t1 ->
    (match eval2 t1 with
     | TmZero -> TmZero
     | TmSucc nv when isnumericalval nv -> nv
     | _ -> raise NoRuleApplies)
  | TmIsZero t1 ->
    (match eval2 t1 with
     | TmZero -> TmTrue
     | TmSucc nv when isnumericalval nv -> TmFalse
     | _ -> raise NoRuleApplies)
  | _ -> raise NoRuleApplies
;;

let evalbig t =
  try
    let t' = eval2 t in
    t'
  with
  | NoRuleApplies -> t
;;

let rec evalsmall t =
  try
    let t' = eval1 t in
    evalsmall t'
  with
  | NoRuleApplies -> t
;;
