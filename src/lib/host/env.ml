include Map.Make(Ident)

let dom e = List.map fst (bindings e)

let upd x v e = update x (fun _ -> Some v) e

let init kvs = List.fold_left (fun e (k,v) -> add k v e) empty kvs

let union e1 e2 = union (fun _ v1 _ -> Some v1) e1 e2
             
let pp ?(sep="=") ?(vlayout=false) pp_v fmt e =
  let open Format in
  let pp_binding fmt (k,v) = fprintf fmt "%a%s%a" Ident.pp k sep pp_v v in
  match bindings e with
  | [] -> fprintf fmt "[]"
  | [b] -> fprintf fmt "@[[%a]@]" pp_binding b
  | bs ->
     if vlayout then fprintf fmt "@[<v>[@,%a@,]@]" (Misc.pp_list_v pp_binding) (bindings e)
     else fprintf fmt "@[<h>[@,%a@,]@]" (Misc.pp_list_h ~sep:"," pp_binding) (bindings e)

let pp_dom ~pp_ident fmt e =
  Format.fprintf fmt "@[<h>[@,%a@,]@]" (Misc.pp_list_h ~sep:"," pp_ident ) (dom e)
