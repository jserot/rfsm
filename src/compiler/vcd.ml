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

open Printf
open Types
open Fsm
open Simul

type vcd_config = {
    mutable default_int_size: int;
    mutable float_precision: int;
  }

let cfg = {
  default_int_size = 8;
  float_precision = 8;
  }

let bits_for_range min max = Utils.Misc.log2 (max-min) +1 

let bits_of_uint s n = 
  let b = Bytes.make s '0' in
  let rec h n i =
    if i >= 0 then begin
      Bytes.set b i (if n mod 2 = 1 then '1' else '0');
      h (n/2) (i-1)
      end in
  h n (s-1);
  Bytes.to_string b


let cpl2 n x =
  let rec pow2 k = if k = 0 then 1 else 2 * pow2 (k-1) in (* Not tail recursive, but who cares, here ... *)
  pow2 n - x

let bits_of_int s v = 
  if v < 0 
  then bits_of_uint s (cpl2 s (-v))
  else bits_of_uint s v

let vcd_size_of_range = function
  | Types.SzExpr1 (Types.Index.TiConst sz) -> sz
  | Types.SzExpr2 (Types.Index.TiConst min, Types.Index.TiConst max) -> bits_for_range min max
  | _ -> cfg.default_int_size

let vcd_kind_of ty = match ty with
  TyEvent -> "event", 1
| TyBool  -> "wire", 1
| TyFloat -> "real", 1
| TyEnum _ -> "real", 1
| TyChar -> "wire", 8
| TyInt r -> "wire", vcd_size_of_range r
  | _ -> Misc.fatal_error "Vcd.vcd_kind_of"

let start_symbol = 33 

let signal_cnt = ref start_symbol

type vcd_signal = string * (char * Types.typ) 

let register_signal =
  fun acc (id,ty)  ->
    if List.mem_assoc id acc  then (* Already registered *)
      acc
    else
      let acc' = (id, (Char.chr !signal_cnt, ty)) :: acc in
      incr signal_cnt;
      acc'

let array_cell_id id i =
  match id with
  | Ident.Local (f, id') -> Ident.Local (f, id' ^ "." ^ string_of_int i)
  | _ -> failwith "Vcd.array_cell_id"

let record_field_id id fd =
  match id with
  | Ident.Local (f, id') -> Ident.Local (f, id' ^ "." ^ fd)
  | Ident.Global id' -> Ident.Global (id' ^ "." ^ fd)

let register_fsm_var acc ((id,ty) as s) =
   match ty with
  | TyArray (Types.Index.TiConst sz, ty') ->
     List.fold_left
       (fun acc n -> register_signal acc (n,ty'))
       acc 
       (Utils.ListExt.range (array_cell_id id) 0 (sz-1))
  | TyArray (_, _) ->
     failwith "Vcd.register_fsm_var"
  | TyRecord (_,fs) ->
     List.fold_left
       (fun acc (fd,ty') -> register_signal acc (record_field_id id fd, ty'))
       acc 
       fs
  | _ ->
     register_signal acc s
 
let register_fsm acc f =
  let f' = f.Dynamic.f_static in
  let sigs = (Ident.Local (f'.f_name, "state"), TyEnum (Types.new_name_var(), states_of_inst f'))
             :: List.map (function (id,ty) -> Ident.Local (f'.f_name,id),ty) f'.f_vars in
  List.fold_left register_fsm_var acc sigs

let register_fsms fsms = List.fold_left register_fsm [] fsms

let register_io gls acc (id,_) =
  let ty =
    try fst (List.assoc id gls)
    with Not_found -> Misc.fatal_error "Vcd.register_io" (* should not happen *) in
  match ty with
  | TyArray (sz,ty') -> failwith "Vcd.register_io: array type" (* TO FIX *)
  | TyRecord (_,fs) ->
     List.fold_left
       (fun acc (fd,ty') -> register_signal acc (record_field_id (Ident.Global id) fd, ty'))
       acc 
       fs
  | _ -> register_signal acc (Ident.Global id, ty)
  
let mk_evbuf_name id = id ^ ".val"

let register_event =
  fun acc (id,(ty,v))  ->
    if List.mem_assoc (Ident.Global id) acc  then (* Already registered *)
      acc
    else
      match ty with
      | Types.TyEvent ->  (* Instantaneous event *)
         let acc' = (Ident.Global id, (Char.chr !signal_cnt, ty)) :: acc in
         signal_cnt := !signal_cnt + 1;
         acc'
      | _ ->
         acc

let register_ios gls ios l = List.fold_left (register_io gls) l ios
let register_evs evs l = List.fold_left register_event l evs

exception Error of string

let dump_reaction oc signals (t,evs) =
  let dump_scalar_event (lhs,value) = 
    let name = match lhs with
      | Dynamic.LVar id -> id
      | _ -> failwith "Vcd.dump_scalar_event: non scalar LHS" (* should not happen *) in
    let (id,ty) =
      try List.assoc name signals
      with Not_found -> raise (Error ("unknown signal: " ^ Ident.to_string name)) in
    match ty, value.Expr.v_desc with
        TyEvent, _ -> fprintf oc "1%c\n" id          (* Instantaneous event *)
      | TyEnum _, Expr.Val_enum e -> fprintf oc "s%s %c\n" e id
      | TyBool, Expr.Val_bool b -> fprintf oc "b%d %c\n" (if b then 1 else 0) id
      | TyBool, Expr.Val_int n -> fprintf oc "b%d %c\n" (if n > 0 then 1 else 0) id
      | TyInt r, Expr.Val_int n -> fprintf oc "b%s %c\n" (bits_of_int (vcd_size_of_range r) n) id
      | TyInt r, Expr.Val_bool b -> fprintf oc "b%s %c\n" (bits_of_int (vcd_size_of_range r) (if b then 1 else 0)) id
      | TyFloat, Expr.Val_float n -> fprintf oc "r%.*f %c\n" cfg.float_precision n id
      | TyChar, Expr.Val_char c -> fprintf oc "b%s %c\n" (bits_of_int 8 (int_of_char c)) id
      | _, _-> () in
  let dump_event (lhs,value) =
    match lhs, value.Expr.v_desc with
    | Dynamic.LVar id, Expr.Val_record fs ->
       List.iter
         (fun (n,v) -> dump_scalar_event (Dynamic.LVar (record_field_id id n), v))
         fs
    (* TODO: handle arrays here *)
    | Dynamic.LArrInd (a, idx), _  -> dump_scalar_event (Dynamic.LVar (array_cell_id a idx), value)
    | _ -> dump_scalar_event (lhs, value) in
  fprintf oc "#%d\n" t;
  List.iter dump_event evs

let dump_signal oc (name,(id,ty)) =
  match ty with
  | TyArray (Types.Index.TiConst sz, ty') ->
     let kind, size =  vcd_kind_of ty'  in
     for i=0 to sz-1 do
       fprintf oc "$var %s %d %c %s $end\n" kind size id (Ident.to_string (array_cell_id name i))
     done
  | TyArray (_, _) ->
     failwith "Vcd.register_fsm_var"
  | TyRecord (_,fs) ->
     List.iter
       (fun (fd,ty') ->
         let kind, size =  vcd_kind_of ty'  in
         fprintf oc "$var %s %d %c %s $end\n" kind size id (Ident.to_string (record_field_id name fd)))
       fs
  | _ ->
     let kind, size =  vcd_kind_of ty in
     fprintf oc "$var %s %d %c %s $end\n" kind size id (Ident.to_string name)
  
let output m ctx fname reacts =
  let oc = open_out fname in
  let local_signals = register_fsms (fst ctx.c_fsms @ snd ctx.c_fsms) in
  let global_signals = 
     []
     |> register_ios m.Static.m_inputs ctx.c_inputs
     |> register_ios m.Static.m_outputs ctx.c_outputs
     |> register_ios m.Static.m_shared ctx.c_vars
     |> register_evs ctx.c_evs in
  fprintf oc "$date\n";
  fprintf oc "   %s\n" (Utils.Misc.time_of_day());
  fprintf oc "$end\n";
  fprintf oc "$version\n";
  fprintf oc "   RFSM %s\n" Version.version;
  fprintf oc "$end\n";
  fprintf oc "$comment\n";
  fprintf oc "   Generated from file <tbd>\n";
  fprintf oc "$end\n";
  fprintf oc "$timescale 1ns $end\n";
  fprintf oc "$scope module top $end\n";
  List.iter (dump_signal oc) global_signals;
  List.iter
    (function (name,(id,ty)) ->
      fprintf oc "$scope module %s $end\n" (Ident.local_of name);
      dump_signal oc (name,(id,ty));
      fprintf oc "$upscope $end\n")
    local_signals;
  fprintf oc "$upscope $end\n";
  fprintf oc "$enddefinitions\n$end\n";
  List.iter (dump_reaction oc (global_signals @ local_signals)) reacts;
  Logfile.write fname;
  close_out oc  

