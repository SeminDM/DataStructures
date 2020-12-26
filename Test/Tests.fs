module BinarySearchTreeTests

open BinarySearchTree

open FsUnit.Xunit
open Xunit

let initialList() = [31; 15; 24; 47; 36; 52; 7]

[<Fact>]
let ``Inorder tree walk`` () =
    let tree = create <| initialList()
    inorder tree |> should equal [7; 15; 24; 31; 36; 47; 52]

[<Fact>]
let ``Preorder tree walk`` () =
    let tree = create <| initialList()
    preorder tree |> should equal [31; 15; 7; 24; 47; 36; 52]

[<Fact>]
let ``Postorder tree walk`` () =
    let tree = create <| initialList()
    postorder tree |> should equal [7; 24; 15; 36; 52; 47; 31]

[<Fact>]
let ``Breadth-first tree walk`` () =
    let tree = create <| initialList()
    breadthFirst tree |> should equal [31; 15; 47; 7; 24; 36; 52]

[<Fact>]
let ``Remove maximum`` () =
    let tree = create <| initialList()
    remove tree 52 |> inorder |> should equal [7; 15; 24; 31; 36; 47]

[<Fact>]
let ``Remove minimum`` () =
    let tree = create <| initialList()
    remove tree 7 |> inorder |> should equal [15; 24; 31; 36; 47; 52]

[<Fact>]
let ``Remove node which has both left and right children`` () =
    let tree = create <| initialList()
    remove tree 15 |> inorder |> should equal [7; 24; 31; 36; 47; 52]

[<Fact>]
let ``Remove root`` () =
    let tree = create <| initialList()
    remove tree 31 |> inorder |> should equal [7; 15; 24; 36; 47; 52]

[<Fact>]
let ``Remove node without left child`` () =
    let tree = create [31; 15; 24; 47; 36; 52; 7; 28; 23; 20; 30]
    remove tree 28 |> inorder |> should equal [7; 15; 20; 23; 24; 30; 31; 36; 47; 52]

[<Fact>]
let ``Remove node without right child`` () =
    let tree = create [31; 15; 24; 47; 36; 52; 7; 28; 23; 20; 30]
    remove tree 23 |> inorder |> should equal [7; 15; 20; 24; 28; 30; 31; 36; 47; 52]

[<Fact>]
let ``Remove node`` () =
    let tree = create [31; 15; 24; 47; 36; 52; 7; 28; 23; 20; 30; 22; 21]
    remove tree 24 |> inorder |> should equal [7; 15; 20; 21; 22; 23; 28; 30; 31; 36; 47; 52]

[<Fact>]
let ``Find node which has both left and right children`` () =
    let tree = create [31; 15; 24; 47; 36; 52; 7; 28; 23; 20; 30; 22; 21]
    let result = findNode tree 24
    match result with
    | Error _ -> result |> should equal Ok
    | Ok node ->
        match node with
        | Empty -> node |> should equal Node
        | Node(v,l,r) ->
            v |> should equal 24
            l |> getValue |> Option.get |> should equal 23
            r |> getValue |> Option.get |> should equal 28

[<Fact>]
let ``Find node which has left child only`` () =
    let tree = create [31; 15; 24; 47; 36; 52; 7; 28; 23; 20; 30; 22; 21]
    let result = findNode tree 23
    match result with
    | Error _ -> result |> should equal Ok
    | Ok node ->
        match node with
        | Empty -> node |> should equal Node
        | Node(v,l,r) ->
            v |> should equal 23
            l |> getValue |> Option.get |> should equal 20
            r |> should equal BinarySearchTree.Empty

[<Fact>]
let ``Find leaf`` () =
    let tree = create [31; 15; 24; 47; 36; 52; 7; 28; 23; 20; 30; 22; 21]
    let result = findNode tree 21
    match result with
    | Error _ -> result |> should equal Ok
    | Ok node ->
        match node with
        | Empty -> node |> should equal Node
        | Node(v,l,r) ->
            v |> should equal 21
            l |> should equal BinarySearchTree.Empty
            r |> should equal BinarySearchTree.Empty

[<Fact>]
let ``Find root`` () =
    let tree = create [31; 15; 24; 47; 36; 52; 7; 28; 23; 20; 30; 22; 21]
    let result = findNode tree 31
    match result with
    | Error _ -> result |> should equal Ok
    | Ok node ->
        match node with
        | Empty -> node |> should equal Node
        | Node(v,l,r) ->
            v |> should equal 31
            l |> getValue |> Option.get |> should equal 15
            r |> getValue |> Option.get |> should equal 47

[<Fact>]
let ``Find not existing node`` () =
    let tree = create [31; 15; 24; 47; 36; 52; 7; 28; 23; 20; 30; 22; 21]
    let result = findNode tree 1
    match result with
    | Error msg -> msg |> should equal "NotFound"
    | Ok _ -> result |> should equal Error
