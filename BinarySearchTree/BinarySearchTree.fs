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

let inorder root =
    let rec walk = function Empty -> [] | Node(v,l,r) -> walk l @ [v] @ walk r
    walk root

let preorder root =
    let rec walk = function Empty -> [] | Node(v,l,r) ->  [v] @ walk l @ walk r
    walk root

let postorder root =
    let rec walk = function Empty -> [] | Node(v,l,r) ->  walk l @ walk r @ [v]
    walk root
