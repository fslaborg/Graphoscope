module Bfs

open Xunit
open Graphoscope
open FSharpAux



[<Fact>]
let ``BFS simple example on FGraph works correctly`` () =

    let actual =
        FGraph.empty
        |> FGraph.addElement 0 'A' 1 'B' 0.1
        |> FGraph.addElement 0 'A' 2 'C' 0.1
        |> FGraph.addElement 1 'B' 2 'C' 0.1
        |> FGraph.addElement 2 'C' 0 'A' 0.1
        |> FGraph.addElement 2 'C' 3 'D' 0.1
        |> FGraph.addElement 3 'D' 3 'D' 0.1
        |> Algorithms.BFS.ofFGraph 2
    


    // Following is Breadth First Traversal (starting from vertex 2) 
    // 2 0 3 1
    let expected = [(2,'C');(0,'A');(3,'D');(1,'B')]

    Assert.Equal<seq<int*char>>(expected, actual)