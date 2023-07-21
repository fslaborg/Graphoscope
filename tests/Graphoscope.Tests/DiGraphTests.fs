module DiGraphTests

open System
open Xunit
open Graphoscope
open Graphoscope.DiGraph
open System.IO

[<Fact>]
let ``Can create empty graph and add nodes and edges`` () =
    let emptyGraph = Builders.create<int, float>()
    addNode emptyGraph 1
    addNode emptyGraph 2
    addNode emptyGraph 3
    let edge = (1,3, 1.0)
    addEdge emptyGraph edge
   
    Assert.Equal(1.0, (Measures.getVolume emptyGraph))
    Assert.Equal(3.0, (Measures.getSize emptyGraph))
    Assert.Equal(0.6666666666666666, (Measures.getMeanDegree emptyGraph))

[<Fact>]
let ``Monkey graph import has correct measures`` () =
    //measures taken from http://konect.cc/networks/moreno_rhesus/
    let file = Path.Combine(Environment.CurrentDirectory, "ReferenceGraphs/out.moreno_rhesus_rhesus.txt")
    printfn "%s" file
    let monkeyGraph = Import.createFromEdgeList file " " 2 false
    
    Assert.Equal(111.0, (DiGraph.Measures.getVolume monkeyGraph))
    Assert.Equal(16, (DiGraph.Measures.getSize monkeyGraph)) 
    Assert.Equal(13.8750,(DiGraph.Measures.getMeanDegree monkeyGraph)) 
        
[<Fact>]
let ``Node removal updates correctly`` () =
    let g =
        [|(0, 1, 1.0); (0, 2, 1.0); (1, 3, 1.0); (3, 1, 1.0); (3, 2, 1.0); (4, 0, 1.0)|]
        |> Builders.createFromEdges

    removeNode g 2

    Assert.Equal(true, [|(0,1.)|] = (getOutEdges g 4))
    Assert.Equal(true, [|(0, 1, 1.0); (1, 3, 1.0); (3, 1, 1.0); (4, 0, 1.0)|] = (getAllOutEdges g))

