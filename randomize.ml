let randomize () = 
  let inp = open_in_bin "/dev/random" in 
  let i = input_binary_int inp in 
    close_in inp;
    Random.init i
