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

and ins_case =
  | Case1
  | Case2
  | Case3
  | Case4
  | Case5

let ( >>= ) m f =
  match m with
  | Empty -> Empty
  | Node n -> f n

let ( >>| ) m f =
  match m with
  | Empty -> Empty
  | Node n -> Node (f n)

let init () = Empty

let parent { parent } = parent
let left { left } = left
let right { right } = parent

let sibling t =
  t.parent >>= fun p ->
  p.left >>= fun pl ->
  if pl = t then
    p.right
  else
    p.left

let uncle t =
  t >>= parent >>= sibling

let grandparent t =
  t >>= parent >>= parent

let caseof t =
  match t with
  | Empty -> Case1
  | Node { color = Black } -> Case2
  | Node { color = Red } as node -> raise Not_found


let do_case1 t = ()
let do_case2 t = ()
let do_case3 t = ()
let do_case4 t = ()
let do_case5 t = ()

let insert a tree =
  let rec aux node parent =
    match node with
    | Node { left; data; right } ->
        if a < data then
          aux left node
        else if a > data then
          aux right node
        else
          node, parent
    | Empty ->
        let node =
          Node { data   = a;
                 left   = Empty;
                 right  = Empty;
                 parent = parent;
                 color  = Red; }
        in
        node, parent
  in
  let node, parent = aux tree Empty in
  (* dispatch *)
  node |> (match caseof parent with
           | Case1 -> do_case1
           | Case2 -> do_case2
           | Case3 -> do_case3
           | Case4 -> do_case4
           | Case5 -> do_case5)
