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
  | _ -> fprintf fmt "@[<v>[%a]@]" (pp_print_list ~pp_sep:pp_cut pp) l

let pp_list_h ?(sep="") pp fmt l = Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "%s" sep) pp fmt l

let pp_opt ?(none="?") pp fmt v =
  match v with
| None -> Format.pp_print_string fmt none
| Some v' -> Format.fprintf fmt "%a" pp v'

let subst_id phi id =
  if List.mem_assoc id phi then List.assoc id phi else id

let neg f x = not (f x)

let list_cart_prod2 l1 l2 =
  List.map (fun x1 -> List.map (fun x2 -> x1,x2) l2) l1 |> List.flatten

let to_string pp x = 
  let open Format in
  fprintf str_formatter "%a" pp x;
  flush_str_formatter ()

let check_dir path = 
  if not (Sys.is_directory path) then raise (Sys_error ("file " ^ " is not a directory"))

let add_assoc k v l = 
  let v' = List.assoc k l in
  (k,v::v') :: List.remove_assoc k l

let list_scatter f l = 
  let add k v l = 
    let v' = List.assoc k l in
    (k,v::v') :: List.remove_assoc k l in
  List.fold_left
    (fun acc x ->
      let k = f x in
      if List.mem_assoc k acc then add k x acc else (k,[x])::acc)
    []
    l

let replace_assoc k v l = (k,v) :: List.remove_assoc k l 

let string_length_nl s = 
  List.fold_left
    (fun acc s -> max acc (String.length s))
    0
    (String.split_on_char '\n' s)
