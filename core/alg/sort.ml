module Mergesort = struct
  let split a m len =
    let left = Array.init m (fun i -> a.(i)) in
    let right = Array.init (len - m)  (fun i -> a.(m + i)) in
    left, right

  let merge left right =
    let i, j, k = ref 0, ref 0, ref 0 in
    let len_left = Array.length left in
    let len_right = Array.length right in
    let a = Array.make (len_left + len_right) 0 in
    while !i < len_left && !j < len_right do
      let val_left = left.(!i) in
      let val_right = right.(!j) in
      if val_left <= val_right then
        begin
          a.(!k) <- val_left;
          incr i;
        end
      else
        begin
          a.(!k) <- val_right;
          incr j;
        end;
      incr k;
    done;
    while !i < len_left do
      a.(!k) <- left.(!i);
      incr i;
      incr k;
    done;
    while !j < len_right do
      a.(!k) <- right.(!j);
      incr j;
      incr k;
    done;
    a

  let rec sort a =
    let len = Array.length a in
    if len <= 1 then a else
      let m = len / 2 in
      let left, right = split a m len in
      merge (sort left) (sort right)
end

let main =
  Random.self_init ();
  let a = Array.init 50 (fun _ -> Random.int 10) in
  Array.iter print_int a;
  print_newline ();
  let a = Mergesort.sort a in
  Array.iter print_int a;
  print_newline ();
