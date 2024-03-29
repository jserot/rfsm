(**********************************************************************)
(*                                                                    *)
(*              This file is part of the RFSM package                 *)
(*                                                                    *)
(*  Copyright (c) 2018-present, Jocelyn SEROT.  All rights reserved.  *)
(*                                                                    *)
(*  This source code is licensed under the license found in the       *)
(*  LICENSE file in the root directory of this source tree.           *)
(*                                                                    *)
(**********************************************************************)

include Map.Make(Ident)

let dom e = List.map fst (bindings e)

let upd x v e = update x (fun _ -> Some v) e

let init kvs = List.fold_left (fun e (k,v) -> add k v e) empty kvs

let union e1 e2 = union (fun _ v1 _ -> Some v1) e1 e2
             
let pp ?(sep="=") ?(vlayout=false) ?(qual=false) pp_v fmt e =
  let open Format in
  let pp_binding fmt (k,v) = fprintf fmt "%a%s%a" (if qual then Ident.pp_qual else Ident.pp) k sep pp_v v in
  match bindings e with
  | [] -> fprintf fmt "[]"
  | [b] -> fprintf fmt "@[[%a]@]" pp_binding b
  | _ ->
     if vlayout then fprintf fmt "@[<v>[@,%a@,]@]" (Ext.List.pp_v pp_binding) (bindings e)
     else fprintf fmt "@[<h>[@,%a@,]@]" (Ext.List.pp_h ~sep:"," pp_binding) (bindings e)

let pp_dom ~pp_ident fmt e =
  Format.fprintf fmt "@[<h>[@,%a@,]@]" (Ext.List.pp_h ~sep:"," pp_ident ) (dom e)
