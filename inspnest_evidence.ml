module Ev = Evidence_common

let nmin = ref 2
let nmax = ref 256
let log_l_noise = ref 0.0
let file = ref ""
let nbootstrap = ref 1

let options = 
  Arg.align
    [("--nmin", Arg.Set_int nmin, 
      Printf.sprintf "nm Minimum boxing number (default %d)" !nmin);
     ("--nmax", Arg.Set_int nmax,
      Printf.sprintf "nm Maximum boxing number (default %d)" !nmax);
     ("--log-l-noise", Arg.Set_float log_l_noise,
      Printf.sprintf "ll Log(Likelihood) for noise (default %g)" !log_l_noise);
     ("--bootstrap", Arg.Set_int nbootstrap,
      Printf.sprintf "n Number of samples to output, > 1 implies bootstrap (default %d)" !nbootstrap)]

let rescale_samples samples = 
  Array.map
    (fun ({Mcmc.like_prior = {Mcmc.log_likelihood = ll; log_prior = lp}} as samp) -> 
      {samp with Mcmc.like_prior = {Mcmc.log_likelihood = ll -. !log_l_noise; log_prior = lp}})
    samples

let _ = 
  Arg.parse options (fun name -> file := name)
    "inspnest_evidence.{byte,native} OPTION ... FILE";
  Randomize.randomize ();
  let samples = rescale_samples (Read_inspnest.read_inspnest_samples !file) in 
    Ev.output_ev !nmin !nmax samples;
    for i = 2 to !nbootstrap do 
      Ev.output_ev !nmin !nmax (Ev.bootstrap_sample samples)
    done
