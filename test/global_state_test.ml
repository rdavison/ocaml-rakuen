open Printf

let f = ref (fun x -> x + 0)

let replace g =
  f := (fun x -> g x)

let rec collect_f i acc =
  if i > 0 then
  begin
    replace (fun x -> x + i);
    collect_f (i-1) (!f :: acc)
  end
  else
    acc

let main =
  let fs = collect_f 100000 [] in
  let res = List.map (fun f -> f 0) fs in
  List.iter (printf "%d\n") res
