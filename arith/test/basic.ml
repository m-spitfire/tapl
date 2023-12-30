open Arith.Ast

let term_testable = Alcotest.testable pp_term equal_term

let test_eval ev () =
  let res =
    Arith.Parser.parse
      {|
true;
if false then true else false; 

0; 
succ (pred 0);
iszero (pred (succ (succ 0)));|}
    |> List.map ev
  in
  Alcotest.(check (list term_testable))
    "same result"
    res
    [ TmTrue; TmFalse; TmZero; TmSucc TmZero; TmFalse ]
;;

let suite =
  [ "can evaluate with big step", `Quick, test_eval Arith.Interp.evalbig
  ; "can evaluate with big step", `Quick, test_eval Arith.Interp.evalsmall
  ]
;;

let () =
  let open Alcotest in
  run "Arith" [ "Parse & Interp", suite ]
;;
