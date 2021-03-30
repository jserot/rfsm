open Ocamlbuild_plugin
 
let () =
  dispatch begin function
  | After_rules ->
     ocaml_lib ~extern:true ~dir:"/Users/jserot/Dev/ml/rfsm/working/lib/ml" "rfsm"
  | _ -> ()
  end
