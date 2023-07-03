exception Not_implemented of string
exception Fatal_error of string
                           
let not_implemented m = raise (Not_implemented m)
let fatal_error m = raise (Fatal_error m)
let warning msg = Printf.printf "** Warning: %s\n" msg; flush stdout
                      
let pp_spc fmt () = Format.fprintf fmt "@ "
let pp_cut fmt () = Format.fprintf fmt "@,"

let pp_list_v pp fmt l =
  let open Format in 
  match l with 
  | [] -> fprintf fmt "[]"
  | [x] -> fprintf fmt "@[<h>[%a]@]" pp x 
  | _ -> fprintf fmt "@[<v>[%a]@]" (pp_print_list ~pp_sep:pp_cut pp) l

let pp_list_h ?(sep="") pp fmt l = Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "%s" sep) pp fmt l

let pp_opt_list ~lr ~sep pp fmt l =
  match l with
  | [] -> ()
  | _ -> Format.fprintf fmt "%s%a%s" (fst lr) (pp_list_h ~sep pp) l (snd lr)

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

let update_list_assoc k v l =
    (* If key [k] does not belong to assoc list [l], add it, with associated value [[v]].
     else add [v] to the associated value *)
    let rec h = function
        [] -> [k,[v]]
      | (k',vs)::l -> if k=k' then (k,v::vs) :: l else (k',vs) :: h l in
    h l

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

let list_iter_fst f l =
  ignore (List.fold_left (fun z x -> f z x; false) true l)

let replace_assoc k v l = (k,v) :: List.remove_assoc k l 

let string_length_nl s = 
  List.fold_left
    (fun acc s -> max acc (String.length s))
    0
    (String.split_on_char '\n' s)

let open_file fname =
  let oc = open_out fname in
  oc, Format.formatter_of_out_channel oc

let close_file (oc,ocf) =
  Format.fprintf ocf "};@."; (* flush *)
  close_out oc

let copy_with_subst defns ic oc = 
  let rec subst mdefs s = match mdefs with
      [] -> s
    | (v,v')::ds -> subst ds (Str.global_replace (Str.regexp_string v) v' s)  in
   try
     while true do
       let line = input_line ic in
       Printf.fprintf oc "%s\n" (subst defns line)
     done
   with End_of_file ->
     ()
