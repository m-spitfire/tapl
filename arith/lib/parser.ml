open Angstrom
open Ast

let ws =
  skip_while (function
    | '\x20' | '\x0a' | '\x0d' | '\x09' -> true
    | _ -> false)
;;

let lchar c = ws *> char c
let lrchar c = ws *> char c <* ws
let lrstring s = ws *> string s <* ws
let _true = lrstring "true" *> return TmTrue
let _false = lrstring "false" *> return TmFalse
let zero = lrchar '0' *> return TmZero
let sc = lchar ';'
let parens p = lchar '(' *> p <* lchar ')'

let expr : term t =
  let open Angstrom.Let_syntax in
  fix (fun expr ->
    let ifelse =
      let* cond = lrstring "if" *> expr in
      let* thenexpr = lrstring "then" *> expr in
      let* elseexpr = lrstring "else" *> expr in
      return (TmIf (cond, thenexpr, elseexpr))
    in
    let _succ = lrstring "succ" *> expr >>| fun e -> TmSucc e in
    let _pred = lrstring "pred" *> expr >>| fun e -> TmPred e in
    let iszero = lrstring "iszero" *> expr >>| fun e -> TmIsZero e in
    let choice_pars = choice [ _true; _false; zero; _succ; _pred; iszero; ifelse ] in
    let parens_pars = parens choice_pars in
    parens_pars <|> choice_pars)
  <* sc
  <* ws
;;

let file : term list t = many expr

let parse str =
  match parse_string ~consume:All file str with
  | Ok v -> v
  | Error msg -> failwith msg
;;

