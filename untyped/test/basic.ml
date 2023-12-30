open Untyped.Ast

let term_testable = Alcotest.testable pp_term equal_term

let evaluator ev inp oracle () =
  let res =
    Untyped.Parser.parse_to_ir inp |> List.map (Untyped.Ast.convert_ir []) |> List.map ev
  in
  Alcotest.(check (list term_testable)) "same result" res oracle
;;

let test_cases =
  [ ( {| lambda x. x; (lambda x. x) (lambda x. x x); |}
    , [ TmAbs ("x", TmVar 0); TmAbs ("x", TmApp (TmVar 0, TmVar 0)) ] )
  ; ( {|(lambda p.lambda q.p q p) (lambda x. lambda y.x) (lambda x.lambda y.y);
        (lambda p.lambda q.p p q) (lambda x. lambda y.x) (lambda x.lambda y.y);
        (lambda p.lambda q.p q p) (lambda x. lambda y.x) (lambda x.lambda y.x);|}
    , [ TmAbs ("x", TmAbs ("y", TmVar 0))
      ; TmAbs ("x", TmAbs ("y", TmVar 1))
      ; TmAbs ("x", TmAbs ("y", TmVar 1))
      ] )
  ]
;;

let interp_funs = [ Untyped.Interp.evalsmall; Untyped.Interp.evalbig ]
let get_test_name f = if f == Untyped.Interp.evalsmall then "test small" else "test big"

let suite =
  List.fold_left
    (fun acc test_case ->
      List.fold_left
        (fun acc interp_fun ->
          ( get_test_name interp_fun
          , `Quick
          , evaluator interp_fun (fst test_case) (snd test_case) )
          :: acc)
        acc
        interp_funs)
    []
    test_cases
;;

let () =
  let open Alcotest in
  run "Untyped" [ "Parse & Interp", suite ]
;;
