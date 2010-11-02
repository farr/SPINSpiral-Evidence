open Printf

module Ev = Evidence.Make(struct
  type params = float array
  let to_coords (params : params) = params
end)

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

let bootstrap_sample arr = 
  let n = Array.length arr in 
  let res = Array.make n arr.(Random.int n) in   
    for i = 1 to n - 1 do 
      res.(i) <- arr.(Random.int n)
    done;
    res

let output_ev samples = 
  let (low,high) = Ev.Kd.bounds_of_objects (Array.to_list samples) in
  let tree = Ev.kd_tree_of_samples samples low high in 
  let rec loop n = 
    if n > !nmax then 
      ()
    else begin
      printf "%d %g\n" n (!prior *. (Ev.evidence_direct_tree ~n:n tree));
      loop (n*2)
    end in 
    loop !nmin

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
    output_ev samples;
    if !nbootstrap > 0 then 
      for i = 1 to !nbootstrap do 
        output_ev (bootstrap_sample samples)
      done
