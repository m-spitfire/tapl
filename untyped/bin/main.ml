let read_file fname =
  let in_ch = open_in fname in
  let f_len = in_channel_length in_ch in
  let content = really_input_string in_ch f_len in
  close_in in_ch;
  content
;;

let () =
  let file_cont = read_file Sys.argv.(1) in
  let ir_t = Untyped.Parser.parse_to_ir file_cont in
  print_endline (Untyped.Ast.show_ir_term (List.hd ir_t));
  let terms =
    Untyped.Parser.parse_to_ir file_cont |> List.map (Untyped.Ast.convert_ir [])
  in
  print_endline "Small step: ";
  List.map Untyped.Interp.evalsmall terms
  |> List.iter (fun x -> Untyped.Interp.printtm [] x);
  print_endline "Big step: ";
  List.map Untyped.Interp.evalbig terms
  |> List.iter (fun x -> Untyped.Interp.printtm [] x)
;;
