module Dijkstra

open Xunit
open Graphoscope
open FSharpAux



[<Fact>]
let ``Dijkstra simple example on FGraph works correctly`` () =

    let dijkstraTestGraph2 =
        FGraph.empty
        |> FGraph.addElement 0 "Node 0" 4 "Node 4" 7. 
        |> FGraph.addElement 3 "Node 3" 0 "Node 0" 3.
        |> FGraph.addElement 4 "Node 4" 3 "Node 3" 1.
        |> FGraph.addElement 4 "Node 4" 1 "Node 1" 2.
        |> FGraph.addElement 2 "Node 2" 4 "Node 4" 5.
        |> FGraph.addElement 2 "Node 2" 3 "Node 3" 9.
        |> FGraph.addElement 1 "Node 1" 2 "Node 2" 4.

    let actual = Algorithms.Dijkstra.ofFGraph 2 dijkstraTestGraph2




    // DFS Traversal Result (starting node 1):
    let expected = [(0, 9.0); (4, 5.0); (3, 6.0); (1, 7.0); (2, 0.0)] |> Set.ofList 

    Assert.Equal<Set<int*float>>(expected, actual |> Dictionary.toSeq |> Set.ofSeq)
    