﻿module BinarySearchTree

type Tree = Node of int * Tree * Tree | Empty

let leaf value = Node(value, Empty, Empty)

let data = function Empty -> failwith "!" | Node(d,_,_) -> d

let rec insert tree value =
    match tree with
    | Empty -> leaf value
    | Node(v,l,r) when value <= v -> 
        match l with
        | Empty -> Node(v, (leaf value), r)
        | l -> Node(v, (insert l value), r)
    | Node(v,l,r) ->
        match r with
        | Empty -> Node(v, l, (leaf value))
        | r -> Node(v, l, (insert r value))

let rec remove root value =
    
    let rec maxNode root =
        match root with
        | Empty
        | Node(_,_,Empty) -> root
        | Node(_,_,r) -> maxNode r

    match root with
    | Empty -> root
    | Node(v,l,r) when value < v -> Node(v, (remove l value), r)
    | Node(v,l,r) when value > v -> Node(v, l, (remove r value))
    | Node(_,Empty,Empty) -> Empty
    | Node(_,Empty,r) -> r
    | Node(_,l,Empty) -> l
    | Node(_,l,r) -> 
        let maxValue = (maxNode>>data)l 
        Node(maxValue, (remove l maxValue), r)

let find root value = failwith ""

let min root = failwith ""

let max root = failwith ""

let next root value = failwith ""

let prev root value = failwith ""
    
let left = function Empty | Node(_,Empty,_) -> None | Node(_,l,_) -> Some l

let right = function Empty | Node(_,_,Empty) -> None | Node(_,_,r) -> Some r

let create items = Seq.fold insert Empty items

let inorder root =
    let rec walk = function Empty -> [] | Node(v,l,r) -> walk l @ [v] @ walk r
    walk root

let preorder root =
    let rec walk = function Empty -> [] | Node(v,l,r) ->  [v] @ walk l @ walk r
    walk root

let postorder root =
    let rec walk = function Empty -> [] | Node(v,l,r) ->  walk l @ walk r @ [v]
    walk root

let breadthFirst root =
    let rec walk nodes result =
        match nodes with
        | [] -> result
        | h::t ->
            match h with
            | Empty -> walk t result
            | Node(v,Empty,Empty) -> walk t (v::result)
            | Node(v,l,r) -> walk (t @ [l;r]) (v::result)
    walk [root] [] |> List.rev