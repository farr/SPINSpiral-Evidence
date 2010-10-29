open Ocamlbuild_plugin

let mcmc_dir = "/Users/farr/Documents/code/mcmc-ocaml/_build"
let oUnit_dir = "/Users/farr/Documents/code/oUnit"

let _ = dispatch begin function 
  | After_rules -> 
    ocaml_lib ~extern:true ~dir:mcmc_dir "mcmc";
    ocaml_lib ~extern:true ~dir:oUnit_dir "oUnit"
  | _ -> ()
end
