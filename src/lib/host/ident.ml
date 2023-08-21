let global_qualifier = "$" 

type scope = Local | Global
   [@@deriving show {with_path=false}]
                   
type t = {
    scope: scope;
    id: string
    }
                     
(* type subst = (t * t) list *)

let mk ?(scope=Global) id = { scope=scope; id=id }

let mk_global id = { id with scope = Global }
let mk_local id = { id with scope = Local }

let upd_id f i = { i with id = f i.id }

(* let subst (phi: subst) id =
 *   if List.mem_assoc id phi then List.assoc id phi else id *)

let pp fmt i = Format.fprintf fmt "%s" i.id

let pp_qual fmt s = (* So bad optional arguments cant be used with [pp] fns ... *)
  match s.scope with
  | Global -> Format.fprintf fmt "%s%s" global_qualifier s.id
  | Local -> Format.fprintf fmt "%s" s.id

let pp' fmt i = Format.fprintf fmt "<%s,%a>" i.id pp_scope i.scope (* For debug *)

let to_string = Misc.to_string pp

let compare = Stdlib.compare
