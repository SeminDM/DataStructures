module BinarySearchTreeTests

open BinarySearchTree

open FsUnit.Xunit
open Xunit

[<Fact>]
let ``Data is retained`` () =
    let treeData = create [4]
    treeData |> data |> should equal 4
    treeData |> left |> should equal None
    treeData |> right |> should equal None

[<Fact>]
let ``Can sort complex tree`` () =
    let treeData = create [2; 1; 3; 6; 7; 5]
    inorder treeData |> should equal [1; 2; 3; 5; 6; 7]

[<Fact>]
let ``Can sort complex tree2`` () =
    let treeData = create [2; 1; 3; 6; 7; 5]
    preorder treeData |> should equal [1; 2; 3; 5; 6; 7]

[<Fact>]
let ``Can sort complex tree3`` () =
    let treeData = create [2; 1; 3; 6; 7; 5]
    postorder treeData |> should equal [1; 2; 3; 5; 6; 7]
