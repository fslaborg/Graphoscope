module Louvain

open Xunit
open Graphoscope
open FSharpAux

[<Fact>]
let ``Louvain DiGraph works corectly`` () =
    let rng = System.Random(123)

    let t1Edges = [|
        (0, 2, 1.)
        (0, 1, 1.)
        (1, 0, 1.)
        (2, 1, 1.)
        (2, 0, 1.)
        (3, 4, 1.)
        (4, 3, 1.)
        (7, 8, 1.)
        (8, 7, 1.)
        (9, 10, 1.)
        (10, 9, 1.)
    |]

    let t1 =
        DiGraph.createFromNodes (Array.init 11 (fun i -> i,i))
        |> DiGraph.addEdges t1Edges
    let actualT1 = Algorithms.Louvain.louvainCommunities(t1, id, rng = rng.NextDouble)

    let expectedT1 = [|[|0; 1; 2|]; [|3; 4|]; [|5|]; [|6|]; [|7; 8|]; [|9; 10|]|]

    let t2Edges = [|
        (1, 2, 1.)
        (1, 6, 1.)
        (1, 9, 1.)
        (2, 3, 1.)
        (2, 4, 1.)
        (2, 5, 1.)
        (3, 4, 1.)
        (4, 3, 1.)
        (4, 5, 1.)
        (5, 4, 1.)
        (6, 7, 1.)
        (6, 8, 1.)
        (9, 10, 1.)
        (9, 11, 1.)
        (10, 11, 1.)
        (11, 10, 1.)
    |]

    let t2 =
        DiGraph.createFromNodes (Array.init 11 (fun i -> i+1, i+1))
        |> DiGraph.addEdges t2Edges
    let actualT2 = Algorithms.Louvain.louvainCommunities(t2, id, rng = rng.NextDouble)

    let expectedT2 = [|[|1; 6; 7; 8|]; [|2; 3; 4; 5|]; [|9; 10; 11|]|]

    Assert.Equal<int [] []>(expectedT1, actualT1)
    Assert.Equal<int [] []>(expectedT2, actualT2)
