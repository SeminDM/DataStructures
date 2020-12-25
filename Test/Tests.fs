module BinarySearchTreeTests

open BinarySearchTree

open FsUnit.Xunit
open Xunit

let initialList() = [31; 15; 24; 47; 36; 52; 7]

[<Fact>]
let ``Data is retained`` () =
    let tree = create [4]
    tree |> data |> should equal 4
    tree |> left |> should equal None
    tree |> right |> should equal None

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
