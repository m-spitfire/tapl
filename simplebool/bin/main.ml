let read_file fname =
  let in_ch = open_in fname in
  let f_len = in_channel_length in_ch in
  let content = really_input_string in_ch f_len in
  close_in in_ch;
  content
;;

let () =
  let file_cont = read_file Sys.argv.(1) in
  (* let ir_t = Simplebool.Parser.parse_to_ir file_cont in *)
  let terms =
    Simplebool.Parser.parse_to_ir file_cont |> List.map (Simplebool.Ast.convert_ir [])
  in
  print_endline "Type: ";
  terms
  |> List.map (Simplebool.Typecheck.typeof [])
  |> List.iter (fun x -> print_endline (Simplebool.Ast.show_ty x))
;;
(* print_endline "Small step: "; *)
(* List.map Simplebool.Interp.evalsmall terms *)
(* |> List.iter (fun x -> Simplebool.Interp.printtm [] x); *)
(* print_endline "Big step: "; *)
(* List.map Simplebool.Interp.evalbig terms *)
(* |> List.iter (fun x -> Simplebool.Interp.printtm [] x) *)
