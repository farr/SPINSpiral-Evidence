open Printf

module Ev = Evidence_common

let prior = ref 1.0
let spinspiral_outputs = ref []
let ignore_coordinates = ref 0
let nmin = ref 4
let nmax = ref 8192
let nbootstrap = ref 0
let downsample = ref 1
let icoords = ref []

let options = 
  Arg.align
    [("--prior", Arg.Set_float prior,
      sprintf "pr additional prior factor (default %g)" !prior);
     ("--ignore-coords", Arg.Set_int ignore_coordinates,
      sprintf "n number of coordinates at end of sample to ignore");
     ("--ignore-coord", Arg.Int (fun i -> icoords := i :: !icoords), 
      sprintf "i ignore coordinate i (counting from 0)");
     ("--nmin", Arg.Set_int nmin, 
      sprintf "n minimum boxing number (default %d)" !nmin);
     ("--nmax", Arg.Set_int nmax,
      sprintf "n maximum boxing number (default %d)" !nmax);
     ("--bootstrap", Arg.Set_int nbootstrap,
      sprintf "n number of bootstrap samples to follow the true sample (default %d)" !nbootstrap);
     ("--downsample", Arg.Set_int downsample,
      sprintf "n number of MCMC samples to read before recording one (default %d)" !downsample)]

let trim_coordinates n samples = 
  Array.map (fun ({Mcmc.value = coords} as samp) -> {samp with Mcmc.value = Array.sub coords 0 (Array.length coords - n)}) samples

let trim_coords samples = 
  Array.map
    (fun ({Mcmc.value = coords} as samp) -> 
      let (_, new_coords) = 
        Array.fold_right 
          (fun coord (i,nc) -> 
            if List.mem i !icoords then 
              (i-1, nc)
            else
              (i-1, coord :: nc))
          coords
          (Array.length coords - 1, []) in 
        {samp with Mcmc.value = Array.of_list new_coords})
    samples          

let rescale_samples samples = 
  Array.map 
    (fun ({Mcmc.like_prior = {Mcmc.log_likelihood = ll; log_prior = lp}} as samp) -> 
      {samp with Mcmc.like_prior = {Mcmc.log_likelihood = ll; log_prior = lp +. (log !prior)}})
    samples

let _ = 
  Arg.parse options (fun s -> spinspiral_outputs := s :: !spinspiral_outputs) 
    "spinspiral_evidence.{byte,native} OPTION ... OUTPUT_FILE ...";
  if !ignore_coordinates != 0 && !icoords != [] then begin
    eprintf "Cannot use both -ignore-coordinates and -ignore-coord!\n";
    exit 1
  end;
  Randomize.randomize ();
  let samples = 
    List.map 
      (fun file -> Read_spinspiral.read_spinspiral_samples ~downsample:(!downsample) file)
      !spinspiral_outputs in 
  let samples = 
    if !ignore_coordinates > 0 then 
      List.map (fun samps -> trim_coordinates !ignore_coordinates samps) samples
    else if !icoords != [] then 
      List.map trim_coords samples 
    else
      samples in
  let samples = List.fold_left Array.append [| |] samples in
  let samples = rescale_samples samples in 
    Ev.output_ev !nmin !nmax samples;
    if !nbootstrap > 0 then 
      for i = 1 to !nbootstrap do 
        Ev.output_ev !nmin !nmax (Ev.bootstrap_sample samples)
      done
