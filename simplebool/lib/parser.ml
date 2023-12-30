open Angstrom
open Ast

let ws =
  skip_while (function
    | '\x20' | '\x0a' | '\x0d' | '\x09' -> true
    | _ -> false)
;;

let not_string str =
  string str
  >>| (fun _ -> false)
  <|> return true
  >>= function
  | true -> return ()
  | false -> fail "not_string"
;;

let alpha =
  not_string "if"
  >>= fun _ ->
  not_string "then"
  >>= fun _ ->
  not_string "lambda"
  >>= fun _ ->
  not_string "else"
  >>= fun _ ->
  not_string "Bool"
  >>= fun _ ->
  take_while1 (fun c ->
    match c with
    | 'a' .. 'z' | 'A' .. 'Z' -> true
    | _ -> false)
;;

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init
;;

let lrstring s = ws *> string s <* ws
let lrchar c = ws *> char c <* ws
let lchar c = ws *> char c
let parens p = lchar '(' *> p <* lchar ')'
let sc = lchar ';'
let parse_var = ws *> alpha >>| fun x -> IRVar x
let app = ws *> return (fun x y -> IRApp (x, y))
let arrow = string "->" *> return (fun x y -> TyArr (x, y))
let ty_bool = string "Bool" *> return TyBool
let parse_ty = chainl1 ty_bool arrow
let _true = lrstring "true" *> return IRTrue
let _false = lrstring "false" *> return IRFalse

let expr : ir_term t =
  fix (fun expr ->
    let lambda =
      let* arg = lrstring "lambda" *> alpha in
      let* typ = char ':' *> parse_ty in
      let+ bod = char '.' *> ws *> expr in
      IRAbs (arg, typ, bod)
    in
    let ifelse =
      let* cond = lrstring "if" *> expr in
      let* thenexpr = lrstring "then" *> expr in
      let+ elseexpr = lrstring "else" *> expr in
      IRIf (cond, thenexpr, elseexpr)
    in
    let atom = choice [ ifelse; _true; _false; lambda; parse_var; parens expr ] in
    chainl1 atom app)
  <* sc
  <* ws
;;

let file_ir : ir_term list t = many expr

let parse_to_ir str =
  match parse_string ~consume:All file_ir str with
  | Ok v -> v
  | Error msg -> failwith msg
;;
