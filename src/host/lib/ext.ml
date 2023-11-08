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

module Base = struct
  
  let pow2 k =
    let rec h k = if k = 0 then 1 else 2 * h (k-1) in (* Not tail recursive, but who cares, here ... *)
    if k >= 0 then h k else invalid_arg "Ext.Base.pow2"

  let neg f x = not (f x)

  let swap (x,y) = (y,x)

  let clone (type t) (x : t) : t = 
    let buf = Marshal.(to_bytes x [No_sharing; Closures]) in
    Marshal.from_bytes buf 0

(* let pp_subst ~pp_ident fmt phi =
 *   let pp_subst_comp fmt (id,id') = Format.fprintf fmt "%a->%a" pp_ident id pp_ident id' in
 *   Format.fprintf fmt "[%a]" (pp_list_h ~sep:", " pp_subst_comp) phi *)

end
              
module Format = struct

  let pp_spc fmt () = Format.fprintf fmt "@ "

  let pp_cut fmt () = Format.fprintf fmt "@,"

  let to_string pp x = Format.asprintf "%a" pp x

end

module List = struct

  let cart_prod l1 l2 =
    List.map (fun x1 -> List.map (fun x2 -> x1,x2) l2) l1 |> List.flatten

  let replace_assoc k v l = (k,v) :: List.remove_assoc k l 

  (* let add_list_assoc k v l = 
   *   let v' = List.assoc k l in
   *   (k,v::v') :: List.remove_assoc k l *)

  let add_list_assoc k v l =
    (* If key [k] does not belong to assoc list [l], add it, with associated value [[v]].
     else add [v] to the associated list of values *)
    let rec h = function
        [] -> [k,[v]]
      | (k',vs)::l -> if k=k' then (k,v::vs) :: l else (k',vs) :: h l in
    h l

  let scatter f l = 
    let add k v l = 
      let v' = List.assoc k l in
      (k,v::v') :: List.remove_assoc k l in
    List.fold_left
      (fun acc x ->
        let k = f x in
        if List.mem_assoc k acc then add k x acc else (k,[x])::acc)
      []
      l

  let iter_fst f l =
    ignore (List.fold_left (fun z x -> f z x; false) true l)

  let fold_leftr f l acc = List.fold_left f acc l

  let pp_v pp fmt l =
    let open Stdlib.Format in 
    match l with 
    | [] -> fprintf fmt "[]"
    | [x] -> fprintf fmt "@[<h>[%a]@]" pp x 
    | _ -> fprintf fmt "@[<v>[%a]@]" (pp_print_list ~pp_sep:Format.pp_cut pp) l

  let pp_h ?(sep="") pp fmt l = Stdlib.Format.pp_print_list ~pp_sep:(fun fmt () -> Stdlib.Format.fprintf fmt "%s" sep) pp fmt l

  let pp_opt ~lr ~sep pp fmt l =
    match l with
    | [] -> ()
    | _ -> Stdlib.Format.fprintf fmt "%s%a%s" (fst lr) (pp_h ~sep pp) l (snd lr)

  let pp_assoc (pp_k,pp_v) fmt l = 
    let open Stdlib.Format in
    let pp_elem fmt (k,v) =  fprintf fmt "%a->%a" pp_k k pp_v v in
    fprintf fmt "[%a]" (pp_h ~sep:"," pp_elem) l

end

module Option = struct
  
  let pp ?(none="?") pp fmt v =
    let open Stdlib.Format in
    match v with
    | None -> pp_print_string fmt none
    | Some v' -> fprintf fmt "%a" pp v'

end
              
module File = struct

  let check_dir path = 
    if not (Sys.is_directory path) then raise (Sys_error ("file " ^ " is not a directory"))

  let open_file fname =
    let oc = open_out fname in
    oc, Stdlib.Format.formatter_of_out_channel oc

  let close_file (oc,ocf) =
    Stdlib.Format.fprintf ocf "@."; (* flush *)
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

end

