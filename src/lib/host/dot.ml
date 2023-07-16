module type DOT = sig
  module Static: Static.T
  val output_static:
    dir:string ->
    name:string ->
    with_models:bool ->
    with_caption:bool ->
    Static.t ->
    string list
end

type cfg = {
    mutable node_shape: string;
    mutable node_style: string;
    mutable layout: string;
    mutable rankdir: string;
    mutable mindist: float;
    mutable trans_vlayout: bool;
    mutable qual_ids: bool;
    mutable abbrev_types: bool;
  }
             
let cfg = {
    node_shape = "circle";
    node_style = "solid";
    layout = "dot";
    rankdir = "UD";
    mindist = 1.0;
    trans_vlayout = true;
    qual_ids = false;
    abbrev_types = false;
  }

module Make(S: Static.T) : DOT with module Static = S =
struct

  module Static = S
  module Syntax = Static.Syntax
  module Types = Syntax.Guest.Types

  let pp_ident fmt i =
    if cfg.qual_ids then Ident.pp_qual fmt i else Ident.pp fmt i

  let pp_lhs fmt l =
    if cfg.qual_ids then Syntax.Guest.pp_qual_lhs fmt l else Syntax.Guest.pp_lhs fmt l

  let pp_cond fmt { Annot.desc=e,gs; _ } =  
    let open Format in
    let pp_guards fmt gs =
      let pp_guard' fmt g = fprintf fmt "(%a)" Syntax.Guest.pp_expr g in
      let pp_guard_list fmt gs = Misc.pp_list_h ~sep:"." pp_guard' fmt gs in
      match gs with
      | [] -> pp_print_text fmt ""
      | gs  -> fprintf fmt ".%a" pp_guard_list gs in
    fprintf fmt "%a%a" pp_ident e pp_guards gs

  let pp_action fmt { Annot.desc=a; _ } =
    let open Format in
    match a with
    | Syntax.Emit e -> fprintf fmt "%a" pp_ident e
    | Syntax.Assign (lhs,expr) ->
       fprintf fmt "%a:=%a" pp_lhs lhs Syntax.Guest.pp_expr expr

  let pp_actions fmt acts =
    Misc.pp_list_h ~sep:(if cfg.trans_vlayout then "\n" else ";") pp_action fmt acts

  let pp_cond_acts fmt (cond,acts) =
    match acts, cfg.trans_vlayout with
    | [], _ -> Format.fprintf fmt "%a" pp_cond cond;
    | _, true ->
      let s1 = Misc.to_string pp_cond cond in
      let s2 = Misc.to_string pp_actions acts in
      let l = String.make (max (Misc.string_length_nl s1) (Misc.string_length_nl s2)) '_' in
      Format.fprintf fmt "%s\n%s\n%s" s1 l s2
    | _, _ ->
         Format.fprintf fmt "%a / %a" pp_cond cond pp_actions acts

  let pp_list_r pp fmt l = 
    match l with 
    | [] -> ()
    | _ ->
       Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.pp_print_string fmt "\n")
         pp
         fmt
         l
  let pp_ovs fmt ovs =
    let open Format in
    let pp_ov fmt (o,v) = fprintf fmt "%a=%a" pp_ident o Syntax.Guest.pp_expr v in
    pp_list_r pp_ov fmt ovs
 
  let outp_model ocf ~name ~kind ~with_caption m = 
    let open Syntax in
    let open Format in
    let node_id i = Ident.to_string name ^ "_" ^ string_of_int i in
    let ini_id = Ident.to_string name ^ "_ini" in
    let nodes, _ = 
      List.fold_left
        (fun (acc,n) { Annot.desc= q,ovs; _ } ->
          (q,node_id n)::acc, n+1)
        ([],0)
        m.states in
    let node_of q =
      try List.assoc q nodes 
      with Not_found -> Misc.fatal_error ("Dot.output_fsm_model: cannot find state " ^ Ident.to_string q) in
    let dump_state {Annot.desc=q,ovs; _} =
      let id = node_of q in
      begin match ovs with
      | [] -> fprintf ocf "%s [label = \"%a\", shape = %s, style = %s]\n"
                id pp_ident q cfg.node_shape cfg.node_style
      | _ ->  fprintf ocf "%s [label = \"%a\n%a\", shape = %s, style = %s]\n"
                id pp_ident q pp_ovs ovs cfg.node_shape cfg.node_style
      end in
    let dump_itransition { Annot.desc=(q,a); _ } =
      let id = node_of q in
      if cfg.trans_vlayout then 
        let s = Misc.to_string pp_actions a in
        let l = String.make (Misc.string_length_nl s) '_' in
        fprintf ocf "%s -> %s [label=\"%s\n%s\"];\n" ini_id id l s
      else 
        fprintf ocf "%s -> %s [label=\"/ %a\"];\n" ini_id id pp_actions a in
    let dump_transition { Annot.desc=(q,c,a,q',_); _ } =
      let id = node_of q in
      let id' = node_of q' in
      fprintf ocf "%s -> %s [label = \"%a\"];\n" id id' pp_cond_acts (c,a) in
    fprintf ocf "%s %s {\nlayout = %s;\nrankdir = %s;\nsize = \"8.5,11\";\nlabel = \"\"\n center = 1;\n nodesep = \"0.350000\"\n ranksep = \"0.400000\"\n fontsize = 14;\nmindist=\"%1.1f\"\n" kind (Ident.to_string m.name) cfg.layout cfg.rankdir cfg.mindist;
    fprintf ocf "%s [shape=point; label=\"\"; style = invis]\n" ini_id;
    List.iter dump_state m.states;
    dump_itransition m.itrans;
    List.iter dump_transition m.trans;
    if with_caption then begin
        let pp_io ocf kind (id,te) =
          fprintf ocf "%s %a: %a\\r" kind pp_ident id (Misc.pp_opt (Static.Syntax.Guest.Types.pp_typ ~abbrev:cfg.abbrev_types)) (te.Annot.typ) in
        let pp_ios ocf m = 
         List.iter (pp_io ocf "param") m.params; 
         List.iter (pp_io ocf "input") m.inps; 
         List.iter (pp_io ocf "output") m.outps in 
        fprintf ocf "%a_ios [label=\"%a\", shape=rect, style=rounded]\n" Ident.pp m.name pp_ios m
      end

  let output_model ~dir ~name ~with_caption { Annot.desc=m; _ } = 
    let fname = Filename.concat dir (Ident.to_string name ^ ".dot") in
    let oc = open_out fname in
    let ocf = Format.formatter_of_out_channel oc in
    outp_model ocf ~name:m.Syntax.name ~kind:"digraph" ~with_caption m;
    Format.fprintf ocf "}";
    close_out oc;
    fname

  let output_fsm ocf f = 
    outp_model ocf ~name:f.Static.name ~kind:"subgraph" ~with_caption:false f.Static.model.Annot.desc;
    if not (Env.is_empty f.params) then begin
        let pp_param fmt (id,v) = Format.fprintf fmt "%a = %a" pp_ident id Static.Value.pp v in      
        let pp_params fmt params = pp_list_r pp_param fmt params in
        Format.fprintf ocf "%a_params [label=\"%a\", shape=rect, style=rounded]\n"
          Ident.pp f.name pp_params (Env.bindings f.params)
      end;
    Format.fprintf ocf "}"

  let output_static ~dir ~name ~with_models ~with_caption (sd: Static.t) =
    let open Static in
    let open Format in
    let fnames = (* Dump all FSM models *)
      if with_models then 
        List.map
          (function m -> output_model ~dir ~name:m.Annot.desc.Syntax.name ~with_caption m)
          sd.models
      else 
        [] in
    match sd.fsms with
    | [] -> (* No instance, that's all folks *)
       fnames
    | _ -> (* System, with FSM instances, globals, etc.. *)
       let fname = Filename.concat dir (name ^ ".dot") in
       let oc = open_out fname in
       let ocf = formatter_of_out_channel oc in
       fprintf ocf "digraph %s {\nlayout = %s;\nrankdir = %s;\nsize = \"8.5,11\";\nlabel = \"\"\n center = 1;\n nodesep = \"0.350000\"\n ranksep = \"0.400000\"\n fontsize = 14;\nmindist=\"%1.1f\"\n" name cfg.layout cfg.rankdir cfg.mindist;
       List.iter (output_fsm ocf) sd.fsms;
       let pp_io ~with_stim kind ocf (id,cc) = 
         if with_stim then
           fprintf ocf "%s %a: %a = %a\\r"
             kind
             pp_ident id
             (Types.pp_typ ~abbrev:cfg.abbrev_types) cc.ct_typ
             (Misc.pp_opt Syntax.pp_stimulus_desc) cc.ct_stim
         else
           fprintf ocf "%s %a: %a\\r"
             kind
             pp_ident id
             (Types.pp_typ ~abbrev:cfg.abbrev_types) cc.ct_typ in
       let pp_ios ocf ctx = 
         List.iter (pp_io ~with_stim:false "input" ocf)  ctx.inputs; 
         List.iter (pp_io ~with_stim:false "output" ocf) ctx.outputs; 
         List.iter (pp_io ~with_stim:false "shared" ocf) ctx.shared in
       fprintf ocf "%s_ios [label=\"%a\", shape=rect, style=rounded]\n" name pp_ios sd.Static.ctx;
       fprintf ocf "}\n";
       close_out oc;
       fname :: fnames
end
