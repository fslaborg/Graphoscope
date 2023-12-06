module Louvain

open Xunit
open Graphoscope
open Graphoscope.Graphs
open FSharpAux

[<Fact>]
let ``Louvain UndirectedGraph works correctly on `KarateClub` `` () =
    // Adapted from Networkx with 1-indexed node keys.
    // https://github.com/networkx/networkx/blob/1c5272054f71f9484347f7e4246ae3d5da367f7b/networkx/algorithms/community/tests/test_louvain.py#L27

    let rng = System.Random(42)
    let karateFile= __SOURCE_DIRECTORY__ + "/ReferenceGraphs/zachary.txt"
    let karateGraph = 
        System.IO.File.ReadAllLines (karateFile)
        |> Array.skip 2
        |> Array.map(fun x ->
          let cols = x.Split " "
          int cols[0], int cols[1], 1.0
        )
        |> Undirected.UndirectedGraph.createFromEdges

    let actual = Algorithms.Louvain.louvainPartitions (karateGraph,  rng = rng.NextDouble)
    let expected =
        [|
            set [1; 2; 3; 4; 8; 10; 12; 13; 14; 18; 20; 22]
            set [5; 6; 7; 11; 17]
            set [9; 15; 16; 19; 21; 23; 27; 30; 31; 33; 34]
            set [24; 25; 26; 28; 29; 32]
        |]

    Assert.Equal<int Set []>(expected, actual |> Array.last)

[<Fact>]
let ``Louvain UndirectedGraph works corectly with `string` node`` () =
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

    let t3 = Undirected.UndirectedGraph.createFromEdges t3Edges

    let actual = Algorithms.Louvain.louvainCommunities (t3, rng = rng.NextDouble)
    let expected = [|set ["a"; "b"; "c"]; set ["d"; "e"; "f"; "g"]|]

    Assert.Equal<string Set []>(expected, actual)


[<Fact>]
let ``Louvain LilMatrix works corectly`` () =
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
        Directed.LilMatrix.createFromNodes (Array.init 11 (fun i -> i,i))
        |> Directed.LilMatrix.addEdges t1Edges
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
        Directed.LilMatrix.createFromNodes (Array.init 11 (fun i -> i+1, i+1))
        |> Directed.LilMatrix.addEdges t2Edges
    let actualT2 = Algorithms.Louvain.louvainCommunities(t2, id, rng = rng.NextDouble)

    let expectedT2 = [|set [1; 6; 7; 8]; set [2; 3; 4; 5]; set [9; 10; 11]|]

    Assert.Equal<int Set []>(expectedT1, actualT1)
    Assert.Equal<int Set []>(expectedT2, actualT2)
