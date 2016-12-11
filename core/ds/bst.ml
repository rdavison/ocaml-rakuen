type 'a t =
  | Node of 'a t * 'a * 'a t
  | Empty

let init () = Empty

let rec insert a = function
  | Node (less, x, more) ->
      if a = x then
        Node (less, x, more)
      else if a < x then
        Node (insert a less, x, more)
      else
        Node (less, x, insert a more)
  | Empty -> Node (Empty, a, Empty)

let rec map f = function
  | Node (less, x, more) -> Node (map f less, f x, map f more)
  | Empty -> Empty

let rec find a = function
  | Node (less, x, more) ->
      if a = x then
        Some x
      else if a < x then
        find a less
      else
        find a more
  | Empty -> None

let rec rightmost = function
  | Empty -> None
  | Node (Empty, x, Empty) -> Some x
  | Node (left, x, right) ->
      let l = lazy (rightmost left) in
      let r = rightmost right in
      (match l, r with
      | _, Some r -> Some r
      | l, None -> Lazy.force l)

let rec leftmost = function
  | Empty -> None
  | Node (Empty, x, Empty) -> Some x
  | Node (left, x, right) ->
      let r = lazy (leftmost right) in
      let l = leftmost left in
      (match l, r with
      | Some l, _ -> Some l
      | None, r -> Lazy.force r)

let print t =
  let indent = 2 in
  let rec aux tree level prefix =
    match tree with
    | Empty -> ()
    | Node (l, x, r) ->
        for i = 0 to (indent * level - 1) do print_char ' ' done;
        print_string prefix;
        print_int x;
        print_newline ();
        aux l (level + 1) "L:";
        aux r (level + 1) "R:"
  in
  aux t 0 "Root:"

let _ =
  let tree1 =
    init ()
    |> insert 10
    |> insert 5
    |> insert 1
    |> insert 3
  in
  print tree1
