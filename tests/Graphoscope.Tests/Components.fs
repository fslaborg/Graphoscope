
module Components

open Xunit
open Graphoscope
open Graphoscope.Graphs
open FSharpAux


let testGraph =
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
    |> Directed.LilMatrix.addEdge (5, 6, 1.0)
    |> Directed.LilMatrix.addEdge (5, 7, 1.0)


let testGraphGiant =
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
    |> Directed.LilMatrix.addEdge (4, 5, 1.0)
    |> Directed.LilMatrix.addEdge (5, 6, 1.0)
    |> Directed.LilMatrix.addEdge (5, 7, 1.0)

[<Fact>]
let ``Can detect no giant compenent`` () =
    testGraph
    |> Algorithms.Components.isWeakComponentOfLilMatrix
    |> Assert.False

[<Fact>]
let ``Can detect giant compenent`` () =
    testGraphGiant
    |> Algorithms.Components.isWeakComponentOfLilMatrix
    |> Assert.True

[<Fact>]
let ``Can get components`` () =
    let components = 
        testGraph
        |> Algorithms.Components.getWeakComponentsOfLilMatrix
        |> Set.count
    
    Assert.True (2 = components)
    
[<Fact>]
let ``Can get new graph of largest component`` () =
    let newGraph = 
        testGraph
        |> Algorithms.Components.getLargestWeakComponentOfLilMatrix
      
    Assert.True (3 = (Directed.LilMatrix.countEdges newGraph) )
