module Ev = Evidence.Make(struct
  type params = float array
  let to_coords (params : params) = params
end)

let bootstrap_sample arr = 
  let n = Array.length arr in 
  let res = Array.make n arr.(Random.int n) in   
    for i = 1 to n - 1 do 
      res.(i) <- arr.(Random.int n)
    done;
    res

let output_ev nmin nmax samples = 
  let (low,high) = Ev.Kd.bounds_of_objects (Array.to_list samples) in
  let tree = Ev.kd_tree_of_samples samples low high in 
  let rec loop n = 
    if n > nmax then 
      ()
    else begin
      let ev = Ev.evidence_direct_tree ~n:n tree in
      Printf.printf "%d %g %g\n" n ev (log ev);
      loop (n*2)
    end in 
    loop nmin
