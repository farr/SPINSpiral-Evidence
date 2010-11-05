let read_array buf = 
  let elts = ref [] in 
    begin
      try
        while true do 
          Scanf.bscanf buf " %g " (fun x -> elts := x :: !elts)
        done
      with 
        | End_of_file -> ()
    end;
    Array.of_list (List.rev !elts)

let read_inspnest_sample inp = 
  let line = input_line inp in 
  let buf = Scanf.Scanning.from_string line in 
  let params_like = read_array buf in 
    {Mcmc.value = Array.sub params_like 0 9;
     like_prior = {Mcmc.log_likelihood = params_like.(9);
                   log_prior = 0.0}}

let read_inspnest_samples file = 
  let inp = open_in file in 
    ignore(input_line inp); (* Ignore header line *)
    let samples = ref [] in 
    let samples =
      try 
        while true do 
          samples := (read_inspnest_sample inp) :: !samples 
        done;
        !samples 
      with 
        | End_of_file -> !samples in
      Array.of_list (List.rev samples)
