module GraphGenerators


open Xunit
open Graphoscope
open Graphoscope.Graphs
open Graphoscope.RandomModels

[<Fact>]
let ``Watts Strogatz generates correctly`` () =

    let g = WattsStrogatz.initWattsStrogatz 16 4 0.2

    let edgesCount = Undirected.UndirectedGraph.getAllEdges g |> Array.length
    let nodeCount = Undirected.UndirectedGraph.getNodes g |> Array.length

    Assert.Equal(16, nodeCount) 
    Assert.Equal(32, edgesCount) 

[<Fact>]
let ``Stargraph generates correctly`` () =

    let g = StarGraph.initStarFContextMap 100 id id (fun a b -> 1.)

    let edgesCount = Measures.Volume.volumeOfFContextMap g
    let nodeCount = Measures.Size.sizeOfFContextMap g

    let degreeMin = Measures.Degree.minimum g
    let degreeMax = Measures.Degree.maximum g

    Assert.Equal(100, nodeCount) 
    Assert.Equal(99, edgesCount) 
    Assert.Equal(1,degreeMin)
    Assert.Equal(99,degreeMax)