let file = ref ""

let _ = 
  Arg.parse [] (fun name -> file := name)
    "inspnest_maxLogL.{byte, native} FILE";
  if !file = "" then 
    raise (Failure "inspnest_maxLogL: need an input filename");
  let samples = Read_inspnest.read_inspnest_samples !file in
  let maxLL = 
    Array.fold_left
      (fun mll {Mcmc.like_prior = {Mcmc.log_likelihood = ll}} -> 
        max mll ll)
      neg_infinity
      samples in 
    Printf.printf "Max log(L) = %g\n" maxLL
    
