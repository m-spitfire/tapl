let rec get_ind l elem c =
  match l with
  | [] -> raise Not_found
  | hd :: tl -> if hd = elem then c else get_ind tl elem (c + 1)
;;
