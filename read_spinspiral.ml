let ignore_header inp = 
  let rec loop () = 
    let line = input_line inp in 
    let maybem1 = 
      try Scanf.sscanf line " %g " (fun m1 -> m1) with 
        | Scanf.Scan_failure(_) -> 0.0
        | Failure(_) -> 0.0
        | End_of_file -> 0.0
    in 
      if maybem1 = -1.0 then 
        ()
      else
        loop () in 
    loop ()

let read_array ichan = 
  let elts = ref [] in 
    (try 
       while true do
         let elt = Scanf.bscanf ichan " %g " (fun x -> x) in 
           elts := elt :: !elts
       done
     with 
       | End_of_file -> ());
    Array.of_list (List.rev !elts)

let read_spinspiral_sample inp = 
  let line = input_line inp in 
  let chan = Scanf.Scanning.from_string line in 
  let cycle = Scanf.bscanf chan " %d " (fun c -> c) in 
  let log_post = Scanf.bscanf chan " %g " (fun lp -> lp) in 
  let prior = Scanf.bscanf chan " %g " (fun p -> p) in 
  let coords = read_array chan in 
    ignore(cycle);
    {Mcmc.like_prior = {Mcmc.log_likelihood = log_post;
                        log_prior = log prior};
     value = coords}

let read_spinspiral_samples ?(downsample = 1) file = 
  Printf.eprintf "Reading SPINspiral output %s\n%!" file;
  let inp = open_in file in 
  let samples = ref [] in 
    ignore_header inp;
    (try 
       while true do 
         for i = 1 to downsample - 1 do 
           ignore(read_spinspiral_sample inp)
         done;
         try 
           let samp = read_spinspiral_sample inp in 
             samples := samp :: !samples
         with 
           | Scanf.Scan_failure(_) -> ()
       done
     with 
       | End_of_file -> 
         close_in inp;
         ());
    Array.of_list (List.rev !samples)
