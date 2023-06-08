exception Not_implemented of string
exception Fatal_error of string
                           
let not_implemented m = raise (Not_implemented m)
let fatal_error m = raise (Fatal_error m)
                      
let pp_spc fmt () = Format.fprintf fmt "@ "
let pp_cut fmt () = Format.fprintf fmt "@,"

let pp_list_v pp fmt l =
  let open Format in 
  match l with 
  | [] -> fprintf fmt "[]"
  | [x] -> fprintf fmt "@[<h>[%a]@]" pp x 
  | _ -> fprintf fmt "@[<v>[@,%a@,]@]" (pp_print_list ~pp_sep:pp_cut pp) l

let pp_list_h ?(sep="") pp fmt l = Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "%s" sep) pp fmt l

let pp_opt ?(none="?") pp fmt v =
  match v with
| None -> Format.pp_print_string fmt none
| Some v' -> Format.fprintf fmt "%a" pp v'

let subst_id phi id =
  if List.mem_assoc id phi then List.assoc id phi else id

let neg f x = not (f x)

let rec list_find_dupl = function
  | [] -> None
  | hd::tl ->
     if List.exists ((=) hd) tl then Some hd
     else list_find_dupl tl

let list_cart_prod2 l1 l2 =
  List.map (fun x1 -> List.map (fun x2 -> x1,x2) l2) l1 |> List.flatten

let to_string pp x = 
  let open Format in
  fprintf str_formatter "%a" pp x;
  flush_str_formatter ()

let check_dir path = 
  if not (Sys.is_directory path) then raise (Sys_error ("file " ^ " is not a directory"))
