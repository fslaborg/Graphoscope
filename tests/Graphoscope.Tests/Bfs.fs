module BFS

open Xunit
open Graphoscope
open Graphoscope.Graphs
open FSharpAux



[<Fact>]
let ``BFS simple example on Directed.FContextMap works correctly`` () =

    let actual =
        Directed.FContextMap.empty
        |> Directed.FContextMap.addElement 0 'A' 1 'B' 0.1
        |> Directed.FContextMap.addElement 0 'A' 2 'C' 0.1
        |> Directed.FContextMap.addElement 1 'B' 2 'C' 0.1
        |> Directed.FContextMap.addElement 2 'C' 0 'A' 0.1
        |> Directed.FContextMap.addElement 2 'C' 3 'D' 0.1
        |> Directed.FContextMap.addElement 3 'D' 3 'D' 0.1
        |> Algorithms.BFS.ofFContextMap 2
    


    // Following is Breadth First Traversal (starting from vertex 2) 
    // 2 0 3 1
    let expected = [(2,'C');(0,'A');(3,'D');(1,'B')]

    Assert.Equal<seq<int*char>>(expected, actual)