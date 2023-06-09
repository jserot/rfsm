module type DOT = sig
  module Static: Static.STATIC
  val output_static: dir:string -> name:string -> with_models:bool -> Static.t -> string list
end

type cfg = {
    mutable node_shape: string;
    mutable node_style: string;
    mutable act_sep: string;
    mutable layout: string;
    mutable rankdir: string;
    mutable mindist: float;
  }
             
let cfg = {
    node_shape = "circle";
    node_style = "solid";
    layout = "dot";
    rankdir = "UD";
    mindist = 1.0;
    act_sep = " ";
  }

module Make(S: Static.STATIC) : DOT with module Static = S =
struct

  module Static = S
  module Syntax = Static.Syntax

  let pp_cond fmt { Annot.desc=e,gs; _ } =  
    let open Format in
    let pp_guards fmt gs =
      let pp_guard' fmt g = fprintf fmt "(%a)" (Syntax.Guest.pp_expr ~with_type:false) g in
      let pp_guard_list fmt gs = Misc.pp_list_h ~sep:"." pp_guard' fmt gs in
      match gs with
      | [] -> pp_print_text fmt ""
      | gs  -> fprintf fmt ".%a" pp_guard_list gs in
    fprintf fmt "%a%a" pp_print_text e pp_guards gs

  let pp_action fmt { Annot.desc=a; _ } =
    let open Format in
    match a with
    | Syntax.Emit e -> fprintf fmt "%s" e
    | Syntax.Assign (lhs,expr) -> fprintf fmt "%a:=%a" (Syntax.Guest.pp_lhs ~with_type:false) lhs (Syntax.Guest.pp_expr ~with_type:false) expr

  let pp_actions fmt acts = Misc.pp_list_h ~sep:";" pp_action fmt acts

  let pp_cond_acts fmt (cond,acts) = match acts with
    | [] -> Format.fprintf fmt "%a" pp_cond cond;
    | _ -> Format.fprintf fmt "%a/%a" pp_cond cond pp_actions acts
    
  let outp_model ocf ~kind m = 
    let open Syntax in
    let open Format in
    let node_id i = m.name ^ "_" ^ string_of_int i in
    let string_of_state q = q in
    let ini_id = m.name ^ "_ini" in
    let ndescs, _ = 
      List.fold_left
        (fun (acc,n) q ->
          (q,(node_id n,string_of_state q))::acc, n+1)
        ([],0)
        m.states in
    let ndesc q =
      try List.assoc q ndescs 
      with Not_found -> Misc.fatal_error ("Dot.output_fsm_model: cannot find state " ^ (string_of_state q)) in
    let dump_state q =
      let id, lbl = ndesc q in
      fprintf ocf "%s [label = \"%s\", shape = %s, style = %s]\n" id lbl (cfg.node_shape) (cfg.node_style) in
    let dump_itransition { Annot.desc=(q,a); _ } =
      let id, _ = ndesc q in
      fprintf ocf "%s -> %s [label=\"%a\"];\n" ini_id id pp_actions a in
    let dump_transition { Annot.desc=(q,c,a,q'); _ } =
      let id, _ = ndesc q in
      let id', _ = ndesc q' in
      fprintf ocf "%s -> %s [label = \"%a\"];\n" id id' pp_cond_acts (c,a) in
    fprintf ocf "%s %s {\nlayout = %s;\nrankdir = %s;\nsize = \"8.5,11\";\nlabel = \"\"\n center = 1;\n nodesep = \"0.350000\"\n ranksep = \"0.400000\"\n fontsize = 14;\nmindist=\"%1.1f\"\n" kind m.name cfg.layout cfg.rankdir cfg.mindist;
    fprintf ocf "%s [shape=point; label=\"\"; style = invis]\n" ini_id;
    List.iter dump_state m.states;
    dump_itransition m.itrans;
    List.iter dump_transition m.trans;
    fprintf ocf "}"

  let output_model ~dir ~name { Annot.desc=m; _ } = 
    let fname = Filename.concat dir (name ^ ".dot") in
    let oc = open_out fname in
    let ocf = Format.formatter_of_out_channel oc in
    outp_model ocf ~kind:"digraph" m;
    close_out oc;
    fname

  let output_static ~dir ~name ~with_models (sd: Static.t) =
    let open Static in
    let open Format in
    let fnames = (* Dump all FSM models *)
      if with_models then 
        List.map
          (function m -> output_model ~dir ~name:m.Annot.desc.Syntax.name m)
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
       List.iter
         (function f -> outp_model ocf ~kind:"subgraph" f.Static.model.Annot.desc)
         sd.fsms;
       let pp_io ocf kind (id,ty) = 
         fprintf ocf "%s %s: %a\\r" kind id Typing.Types.pp_typ ty in (* TO FIX: add type *)
       let pp_ios ocf ctx = 
         List.iter (pp_io ocf "input") ctx.inputs; 
         List.iter (pp_io ocf "output") ctx.outputs; 
         List.iter (pp_io ocf "shared") ctx.shared in
       fprintf ocf "%s_ios [label=\"%a\", shape=rect, style=rounded]\n" name pp_ios sd.Static.ctx;
       fprintf ocf "}\n";
       close_out oc;
       fname :: fnames
end
