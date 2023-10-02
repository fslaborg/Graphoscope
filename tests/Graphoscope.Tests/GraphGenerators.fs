module GraphGenerators


open Xunit
open Graphoscope.Graph
open Graphoscope.RandomModels

[<Fact>]
let ``Watts Strogatz generates correctly`` () =

    let g = WattsStrogatz.initWattsStrogatz 16 4 0.2

    let edgesCount = UndirectedGraph.getAllEdges g |> Array.length
    let nodeCount = UndirectedGraph.getNodes g |> Array.length

    Assert.Equal(16, nodeCount) 
    Assert.Equal(32, edgesCount) 