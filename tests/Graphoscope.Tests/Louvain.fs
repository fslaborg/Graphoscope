module Louvain

open Xunit
open Graphoscope
open FSharpAux

[<Fact>]
let ``Louvain UndirectedGraph works corectly with `string` node keys`` () =
    let rng = System.Random(123)
    let t3Edges =
        [|
            ("a", "b", 1.0)
            ("a", "c", 1.0)
            ("b", "c", 1.0)
            ("b", "d", 1.0)  // inter-community edge
            ("d", "e", 1.0)
            ("d", "f", 1.0)
            ("d", "g", 1.0)
            ("f", "g", 1.0)
            ("f", "e", 1.0)
        |]

    let t3 = UndirectedGraph.createFromEdges t3Edges

    let actual = Algorithms.Louvain.louvainCommunities (t3, rng = rng.NextDouble)
    let expected = [|set ["a"; "b"; "c"]; set ["d"; "e"; "f"; "g"]|]

    Assert.Equal<string Set []>(expected, actual)


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

    let expectedT1 = [|set [0; 1; 2]; set [3; 4]; set [5]; set [6]; set [7; 8]; set [9; 10]|]

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

    let expectedT2 = [|set [1; 6; 7; 8]; set [2; 3; 4; 5]; set [9; 10; 11]|]

    Assert.Equal<int Set []>(expectedT1, actualT1)
    Assert.Equal<int Set []>(expectedT2, actualT2)


