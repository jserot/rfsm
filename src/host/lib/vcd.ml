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

(**{1 VCD output} *)

type cfg = {
    mutable default_int_size: int;
    mutable float_precision: int;
  }

let cfg = {
    default_int_size = 8;
    float_precision = 8;
  }


module type VCD = sig
  type seq
  exception Unsupported of Vcd_types.vcd_typ * Vcd_types.vcd_value
  val output: fname:string -> seq -> unit
end

module Make (EvSeq: Evseq.EVSEQ) : VCD with type seq = EvSeq.t =
struct

  type seq = EvSeq.t
  module Event = EvSeq.Evset.Event

  open Printf
 
  exception Unsupported of Vcd_types.vcd_typ * Vcd_types.vcd_value
     
  let vcd_kind_of ty =
    match ty with
    | Vcd_types.TyEvent -> "event", 1
    | Vcd_types.TyBool  -> "wire", 1
    | Vcd_types.TyInt (Some w) -> "wire", w
    | Vcd_types.TyInt None -> "wire", cfg.default_int_size
    | Vcd_types.TyString -> "real", 1
    | Vcd_types.TyFloat -> "real", cfg.float_precision
    | Vcd_types.TyChar -> "wire", 8

  let vcd_repr ty v = match ty, v with
  | Vcd_types.TyInt (Some w), Vcd_types.Val_int v -> Printf.sprintf "b%s" (Bits.of_int w v)
  | Vcd_types.TyInt None, Vcd_types.Val_int v -> Printf.sprintf "b%s" (Bits.of_int (cfg.default_int_size) v)
  | Vcd_types.TyBool, Vcd_types.Val_bool v -> Printf.sprintf "b%d" (if v then 1 else 0)
  | Vcd_types.TyString, Vcd_types.Val_string s -> Printf.sprintf "s%s" s
  | Vcd_types.TyFloat, Vcd_types.Val_float n -> Printf.sprintf "r%.*f" cfg.float_precision n
  | Vcd_types.TyChar, Vcd_types.Val_char c -> Printf.sprintf "b%s" (Bits.of_int 8 (int_of_char c))
  | _, _ -> raise (Unsupported (ty,v))

  let register_event acc e =
    match e with
    | Event.Ev name -> Vcd_types.register_signal acc (name, Vcd_types.TyEvent)
    | Event.Upd (lval,v) -> Vcd_types.register_signal acc (Event.Syntax.lval_vcd_repr lval, Event.Value.vcd_type v)
    | Event.StateMove (s,q) -> Vcd_types.register_signal acc (Ident.mk s, Vcd_types.TyString)

  let register_signals acc (s:EvSeq.Evset.t) =
    List.fold_left register_event acc (EvSeq.Evset.events s)

  let dump_signal oc (name,(id,ty)) =
    let kind, size =  vcd_kind_of ty in
    fprintf oc "$var %s %d %c %s $end\n" kind size id (Ident.to_string name)

  let dump_evseq oc signals s =
    let lookup name = 
      try List.assoc name signals
      with Not_found -> Misc.fatal_error ("Vcd.dump_evseq: unknown signal: " ^ Ext.Format.to_string Ident.pp name) in
    let dump_stimulus s = match s with
      | Event.Ev name ->
         let id, _  = lookup name in 
         fprintf oc "1%c\n" id          (* Instantaneous event *)
      | Event.Upd (l, v) ->             (* Update *)
         let name = Event.Syntax.lval_vcd_repr l in
         let id, ty  = lookup name in 
         let v' = Event.Value.vcd_value v in
         let fmt = vcd_repr ty v'  in
         fprintf oc "%s %c\n" fmt id
      | Event.StateMove (s,q) ->
         let id, ty  = lookup (Ident.mk s) in 
         fprintf oc "s%s %c\n" q id in  (* State move*)
    fprintf oc "#%d\n" (EvSeq.Evset.date s);
    List.iter dump_stimulus (EvSeq.Evset.events s) 
  
  let normalize_seq rs =
    (* "Normalizes" a trace by transforming "complex" events into simple ones :
        - turns LHS descriptions into VCD signal names (ex: "x[3]" -> "x.3",  "r.f" -> "r.f")
        - turns structured value updates into scalar updates (ex: "x <- {f1=v1;f2=v2}" -> [x.f1<-v1; x.f2<-v2]) *)
    let module Event = EvSeq.Evset.Event in
    let normalize_event (e: Event.t) = 
      match e with
        | Event.Upd (lval,v) as ev -> 
           let base_name = Event.Syntax.lval_base_name lval in (* Ex: "x"->"x", "r.f"-> "r", "a[i]" ->"a" *)
           let es = 
           begin
             match Event.Value.flatten ~base:base_name v with
             | [_] -> [ev]  (* "Simple" case *)
             | nvs -> List.map (fun (n,v) -> Event.Upd (Event.Syntax.mk_simple_lval n, v)) nvs (* "Complex" case *)
           end in
           es
        | _ -> [e]  in
    let normalize_evset (evs: EvSeq.Evset.t) = 
      let open EvSeq.Evset in 
      let t = date evs in
      let es = events evs in
      let es' = List.flatten (List.map normalize_event es) in
      mk t es' in
    List.map normalize_evset rs
      
  let output ~fname (rs:EvSeq.t) =
    let rs' = normalize_seq rs in
    let oc = open_out fname in
    let signals = List.fold_left register_signals [] rs' in
    fprintf oc "$date\n";
    fprintf oc "   %s\n" "today";
    fprintf oc "$end\n";
    (* fprintf oc "$version\n";
     * fprintf oc "   RFSM\n";
     * fprintf oc "$end\n"; *)
    fprintf oc "$comment\n";
    fprintf oc "   Generated by RFSM compiler (github.com/jserot/rfsm)\n";
    fprintf oc "$end\n";
    fprintf oc "$timescale 1ns $end\n";
    fprintf oc "$scope module top $end\n";
    List.iter (dump_signal oc) signals;
    fprintf oc "$upscope $end\n";
    fprintf oc "$enddefinitions\n$end\n";
    List.iter (dump_evseq oc signals) rs';
    close_out oc  
end
