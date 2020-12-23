module BinarySearchTree

type Tree = Node of int * Tree * Tree | Empty

let leaf value = Node(value, Empty, Empty)

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
    
let left = function Empty | Node(_,Empty,_) -> None | Node(_,l,_) -> Some l

let right = function Empty | Node(_,_,Empty)-> None | Node(_,_,r) -> Some r

let data = function Empty -> failwith "!" | Node(d,_,_) -> d

let create items = Seq.fold insert Empty items

let sortedData root =
    let rec trav = function Empty -> [] | Node(v,l,r) -> trav l @ [v] @ trav r
    trav root 
