open Ocamlbuild_plugin
 
let () =
  dispatch begin function
  | After_rules ->
     ocaml_lib ~extern:true ~dir:"../../../../../../lib/ml" "rfsm"
  | _ -> ()
  end
