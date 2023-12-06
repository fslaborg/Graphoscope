module Dijkstra

open Xunit
open Graphoscope
open Graphoscope.Graphs
open FSharpAux



[<Fact>]
let ``Dijkstra simple example on FContextMap works correctly`` () =

    let dijkstraTestGraph2 =
        Directed.FContextMap.empty
        |> Directed.FContextMap.addElement 0 "Node 0" 4 "Node 4" 7. 
        |> Directed.FContextMap.addElement 3 "Node 3" 0 "Node 0" 3.
        |> Directed.FContextMap.addElement 4 "Node 4" 3 "Node 3" 1.
        |> Directed.FContextMap.addElement 4 "Node 4" 1 "Node 1" 2.
        |> Directed.FContextMap.addElement 2 "Node 2" 4 "Node 4" 5.
        |> Directed.FContextMap.addElement 2 "Node 2" 3 "Node 3" 9.
        |> Directed.FContextMap.addElement 1 "Node 1" 2 "Node 2" 4.

    let actual = Algorithms.Dijkstra.ofFContextMap 2 id dijkstraTestGraph2

    // DFS Traversal Result (starting node 1):
    let expected = [(0, 9.0); (4, 5.0); (3, 6.0); (1, 7.0); (2, 0.0)] |> Set.ofList 

    Assert.Equal<Set<int*float>>(expected, actual |> Dictionary.toSeq |> Set.ofSeq)
    
[<Fact>]
let ``Dijkstra simple example on LilMatrix works correctly`` () =

    let dijkstraTestGraph2 =
        Directed.LilMatrix.empty
        |> Directed.LilMatrix.addElement 0 "Node 0" 4 "Node 4" 7. 
        |> Directed.LilMatrix.addElement 3 "Node 3" 0 "Node 0" 3.
        |> Directed.LilMatrix.addElement 4 "Node 4" 3 "Node 3" 1.
        |> Directed.LilMatrix.addElement 4 "Node 4" 1 "Node 1" 2.
        |> Directed.LilMatrix.addElement 2 "Node 2" 4 "Node 4" 5.
        |> Directed.LilMatrix.addElement 2 "Node 2" 3 "Node 3" 9.
        |> Directed.LilMatrix.addElement 1 "Node 1" 2 "Node 2" 4.

    let actual = Algorithms.Dijkstra.ofLilMatrix 2 id dijkstraTestGraph2

    // DFS Traversal Result (starting node 1):
    let expected = [(0, 9.0); (4, 5.0); (3, 6.0); (1, 7.0); (2, 0.0)] |> Set.ofList 

    Assert.Equal<Set<int*float>>(expected, actual |> Set.ofArray)
    