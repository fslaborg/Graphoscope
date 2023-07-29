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


