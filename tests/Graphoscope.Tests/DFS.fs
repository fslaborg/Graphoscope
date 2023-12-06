module DFS

open Xunit
open Graphoscope
open Graphoscope.Graphs
open FSharpAux



[<Fact>]
let ``BFS simple example on FContextMap works correctly`` () =

    let actual =
        Directed.FContextMap.empty
        |> Directed.FContextMap.addElement 1 "Node 1" 2 "Node 2" 0.1  
        |> Directed.FContextMap.addElement 1 "Node 1" 3 "Node 3" 0.1
        |> Directed.FContextMap.addElement 2 "Node 2" 4 "Node 4" 0.1  
        |> Directed.FContextMap.addElement 2 "Node 2" 5 "Node 5" 0.1
        |> Directed.FContextMap.addElement 3 "Node 3" 6 "Node 6" 0.1
        |> Directed.FContextMap.addElement 5 "Node 5" 7 "Node 7" 0.1
        |> Algorithms.DFS.ofFContextMap 1
    


    // DFS Traversal Result (starting node 1):
    let expected = [( 1, "Node 1"); ( 3, "Node 3"); ( 6, "Node 6"); ( 2, "Node 2");
                        ( 5, "Node 5"); ( 7, "Node 7"); ( 4, "Node 4");]

    Assert.Equal<seq<int*string>>(expected, actual)       


[<Fact>]
let ``DFS works on LilMatrix`` () =

    let actual =
        Directed.LilMatrix.empty
        |> Directed.LilMatrix.addNode 1 1
        |> Directed.LilMatrix.addNode 2 2 
        |> Directed.LilMatrix.addNode 3 3 
        |> Directed.LilMatrix.addNode 4 4 
        |> Directed.LilMatrix.addNode 5 5 
        |> Directed.LilMatrix.addNode 6 6 
        |> Directed.LilMatrix.addNode 7 7
        |> Directed.LilMatrix.addEdge (1, 2, 1.0)
        |> Directed.LilMatrix.addEdge (1, 3, 1.0)
        |> Directed.LilMatrix.addEdge (2, 4, 1.0)
        |> Directed.LilMatrix.addEdge (2, 5, 1.0)
        |> Directed.LilMatrix.addEdge (3, 6, 1.0)
        |> Directed.LilMatrix.addEdge (5, 7, 1.0)
        |> Algorithms.DFS.ofLilMatrix 1


    // DFS Traversal Result (starting node 1):
    let expected = [( 1, 1); ( 3, 3); ( 6, 6); ( 2, 2);
                        ( 5, 5); ( 7, 7); ( 4, 4);]

    Assert.Equal<seq<int*int>>(expected, actual)      

