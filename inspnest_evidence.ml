module Ev = Evidence_common

let nmin = ref 2
let nmax = ref 512
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

let pi = 3.1415926535897932385

let prior_norm = 
  let volume = 
    0.944218 *. (* Integral of Mc^(-5/6) over 1 <= M1,M2 <= 34, M1+M2 < 35 *)
      0.1 *. (* Time range *)
      2.0*.pi *. (* Phi0 *)
      (1.0/.3.0)*.(100.0*.100.0*.100.0) *. (* Distance *)
      4.0*.pi *. (* RA and Dec *)
      4.0*.pi (* Orbit orientation---psi and iota *) in 
    1.0 /. volume

let prior samp = 
  let mc = samp.(0) and 
      dist = samp.(4) and 
      dec = samp.(6) and 
      inc = samp.(8) in 
    mc**(-5.0/.6.0) *. dist*.dist *. (cos dec) *. (sin inc) *. prior_norm

let rescale_samples samples = 
  Array.map
    (fun ({Mcmc.like_prior = {Mcmc.log_likelihood = ll; log_prior = lp}; value = pt} as samp) -> 
      {samp with Mcmc.like_prior = {Mcmc.log_likelihood = ll -. !log_l_noise; log_prior = log (prior pt)}})
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
