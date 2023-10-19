
module Components

open Xunit
open Graphoscope
open FSharpAux


let testGraph =
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
    |> DiGraph.addEdge (5, 6, 1.0)
    |> DiGraph.addEdge (5, 7, 1.0)


let testGraphGiant =
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
    |> DiGraph.addEdge (4, 5, 1.0)
    |> DiGraph.addEdge (5, 6, 1.0)
    |> DiGraph.addEdge (5, 7, 1.0)

[<Fact>]
let ``Can detect no giant compenent`` () =
    testGraph
    |> Algorithms.Components.isWeakComponentOfDiGraph
    |> Assert.False

[<Fact>]
let ``Can detect giant compenent`` () =
    testGraphGiant
    |> Algorithms.Components.isWeakComponentOfDiGraph
    |> Assert.True

[<Fact>]
let ``Can get components`` () =
    let components = 
        testGraph
        |> Algorithms.Components.getWeakComponentsOfDiGraph
        |> Set.count
    
    Assert.True (2 = components)
    
[<Fact>]
let ``Can get new graph of largest component`` () =
    let newGraph = 
        testGraph
        |> Algorithms.Components.getLargestWeakComponentOfDiGraph
      
    Assert.True (3 = (DiGraph.countEdges newGraph) )
