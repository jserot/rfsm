open Ocamlbuild_plugin

let () =
  dispatch begin function
  | Before_rules ->
     rule "options_spec.ml"
       ~dep:"options_spec.txt"
       ~prod: "options_spec.ml" 
       (fun _ _ -> Cmd(S [Sh "../build_options_spec"; P "options_spec.txt"; P "options_spec.ml"]))
  | _ -> ()
  end
