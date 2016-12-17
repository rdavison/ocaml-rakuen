open Printf

(* Here is a global function *)
let f = ref (fun x -> x + 0)

(* Define a function to replace the global function with something else *)
let replace g =
  f := (fun x -> g x)

(* Replace the global and then save it into a list *)
let rec collect_f i acc =
  if i > 0 then
  begin
    replace (fun x -> x + i);
    collect_f (i-1) (!f :: acc)
  end
  else
    acc

(* Run through the list and apply the function to the number 0 *)
let main =
  let fs = collect_f 100000 [] in
  let res = List.map (fun f -> f 0) fs in
  List.iter (printf "%d\n") res
