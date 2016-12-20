open Printf

let binsearch x s =
  let rec aux i j =
    let mid = (i + j) / 2 in
    if i > j then
      None
    else if s.(mid) < x then
      aux (mid + 1) j
    else if s.(mid) > x then
      aux i (mid - 1)
    else
      Some mid
  in
  aux 0 (Array.length s - 1)

let main =
  let a = [|0;1;2;3;4;5;6;7;8;9|] in
  let b = Array.map (fun i -> binsearch i a) a in
  let print_result needle = function
    | Some i -> printf "search %d: Some %d\n" needle i
    | None -> printf "search %d: None\n" needle
  in
  Array.iteri print_result b;
  let needle = 10   in a |> binsearch needle |> print_result needle;
  let needle = 1    in a |> binsearch needle |> print_result needle;
  let needle = 100  in a |> binsearch needle |> print_result needle;
  let needle = -1   in a |> binsearch needle |> print_result needle;
  let needle = 0    in a |> binsearch needle |> print_result needle;
  let needle = -100 in a |> binsearch needle |> print_result needle;
