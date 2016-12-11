type 'a t =
  | Node of 'a * 'a t
  | Empty

let init () = Empty

let insert a t = Node (a, t)

let rec foldl f a = function
  | Node (b, bs) ->
      let a' = f a b in
      foldl f a' bs
  | Empty -> a

let rec map f = function
  | Node (x, xs) -> Node (f x, map f xs)
  | Empty -> Empty

let reverse t =
  let rec aux acc = function
    | Node (x, xs) -> aux (insert x acc) xs
    | Empty -> acc
  in
  aux Empty t

let rec filter p = function
  | Node (x, xs) ->
      if p x then
        Node (x, filter p xs)
      else
        filter p xs
  | Empty -> Empty

let filter' p t =
  let rec aux acc = function
    | Node (x, xs) ->
        if p x then
          aux (insert x acc) xs
        else
          aux acc xs
    | Empty -> acc
  in
  aux Empty (reverse t)

let rec find a = function
  | Node (x, xs) ->
    if a = x then
      Some x
    else
      find a xs
  | Empty -> None

let rec length = function
  | Node (x, xs) -> 1 + length xs
  | Empty -> 0

let length' t =
  let rec aux acc t =
    match t with
    | Node (_, rest) -> aux (acc + 1) rest
    | Empty -> acc
  in
  aux 0 t

let length'' t =
  foldl (fun a _ -> a + 1) 0 t

let head = function
  | Node (x, _) -> Some x
  | Empty -> None

let tail = function
  | Node (_, xs) -> Some xs
  | Empty -> None

let rec iter f = function
  | Node (x, xs) -> f x; iter f xs
  | Empty -> ()

let concat a b =
  let rec aux acc = function
    | Node (x, xs) -> aux (insert x acc) xs
    | Empty -> acc
  in
  aux b (reverse a)

let replicate n a =
  let rec aux acc i =
    if i < n then
      aux (insert a acc) (i+1)
    else
      acc
  in
  aux Empty 0

module Monad = struct
  let rec join = function
    | Node (xs, xss) -> concat xs (join xss)
    | Empty -> Empty

  let bind m f = map f m |> join

  let return a = Node (a, Empty)

  let ( >>= ) m f = bind m f
end

let zip xs ys = let open Monad in
  xs >>= fun x ->
  ys >>= fun y ->
  return (x, y)
