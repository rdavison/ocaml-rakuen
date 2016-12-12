type 'a t = 
| Empty
| Node of 'a node

and 'a node = {
  data   : 'a;
  left   : 'a t;
  right  : 'a t;
  parent : 'a t;
  color  : color;
}

and color = Red | Black

let init () = Empty

let caseof t = `Case1

let insert a tree =
  let rec aux node parent =
    match node with
    | Node { left; data; right } ->
        if a < data then
          aux left node
        else if a > data then
          aux right node
        else
          node
    | Empty ->
        Node { data   = a;
               left   = Empty;
               right  = Empty;
               parent = parent;
               color  = Red; }
  in
  let tree = aux tree Empty in
  match caseof tree with
  | `Case1 -> ()
  | `Case2 -> ()
  | `Case3 -> ()
  | `Case4 -> ()
  | `Case5 -> ()
