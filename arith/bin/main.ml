open Arith.Interp

let read_file fname =
  let in_ch = open_in fname in
  let f_len = in_channel_length in_ch in
  let content = really_input_string in_ch f_len in
  close_in in_ch;
  content
;;

let () =
  let file_cont = read_file Sys.argv.(1) in
  let terms = Arith.Parser.parse file_cont in
  print_endline "Big step: ";
  List.map evalbig terms |> List.iter (fun x -> print_endline (Arith.Ast.show_term x));
  print_endline "Small step: ";
  List.map evalsmall terms |> List.iter (fun x -> print_endline (Arith.Ast.show_term x))
;;
