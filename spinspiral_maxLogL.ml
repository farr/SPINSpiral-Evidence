let files = ref []

let _ = 
  Arg.parse [] (fun file -> files := file :: !files)
    "max_logL.{byte,native} FILES ...";
  let samples = List.map Read_spinspiral.read_spinspiral_samples !files in 
  let maxLL = 
    List.fold_left 
      (fun maxLL samps -> 
        Array.fold_left 
          (fun maxLL {Mcmc.like_prior = {Mcmc.log_likelihood = ll}} -> 
            max maxLL ll)
          maxLL
          samps)
      neg_infinity
      samples in 
    Printf.printf "Max log(Likelihood) = %g\n" maxLL
