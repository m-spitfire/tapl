open Angstrom
open Ast

let ws =
  skip_while (function
    | '\x20' | '\x0a' | '\x0d' | '\x09' -> true
    | _ -> false)
;;

let alpha =
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

let expr : ir_term t =
  fix (fun expr ->
    let lambda =
      lrstring "lambda" *> alpha
      >>= fun arg -> lrchar '.' *> expr >>| fun e -> IRAbs (arg, e)
    in
    let atom = choice [ parens expr; lambda; parse_var ] in
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
