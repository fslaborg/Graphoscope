module DFS

open Xunit
open Graphoscope
open FSharpAux



[<Fact>]
let ``BFS simple example on FGraph works correctly`` () =

    let actual =
        FGraph.empty
        |> FGraph.addElement 1 "Node 1" 2 "Node 2" 0.1  
        |> FGraph.addElement 1 "Node 1" 3 "Node 3" 0.1
        |> FGraph.addElement 2 "Node 2" 4 "Node 4" 0.1  
        |> FGraph.addElement 2 "Node 2" 5 "Node 5" 0.1
        |> FGraph.addElement 3 "Node 3" 6 "Node 6" 0.1
        |> FGraph.addElement 5 "Node 5" 7 "Node 7" 0.1
        |> Algorithms.DFS.ofFGraph 1
    


    // DFS Traversal Result (starting node 1):
    let expected = [( 1, "Node 1"); ( 3, "Node 3"); ( 6, "Node 6"); ( 2, "Node 2");
                        ( 5, "Node 5"); ( 7, "Node 7"); ( 4, "Node 4");]

    Assert.Equal<seq<int*string>>(expected, actual)       


[<Fact>]
let ``DFS works on DiGraph`` () =

    let actual =
        DiGraph.empty
        |> DiGraph.addNode 1 1
        |> DiGraph.addNode 2 2 
        |> DiGraph.addNode 3 3 
        |> DiGraph.addNode 4 4 
        |> DiGraph.addNode 5 5 
        |> DiGraph.addNode 6 6 
        |> DiGraph.addNode 7 7
        |> DiGraph.addEdge (1, 2, 1.0)
        |> DiGraph.addEdge (1, 3, 1.0)
        |> DiGraph.addEdge (2, 4, 1.0)
        |> DiGraph.addEdge (2, 5, 1.0)
        |> DiGraph.addEdge (3, 6, 1.0)
        |> DiGraph.addEdge (5, 7, 1.0)
        |> Algorithms.DFS.ofDiGraph 1


    // DFS Traversal Result (starting node 1):
    let expected = [( 1, 1); ( 3, 3); ( 6, 6); ( 2, 2);
                        ( 5, 5); ( 7, 7); ( 4, 4);]

    Assert.Equal<seq<int*int>>(expected, actual)      

