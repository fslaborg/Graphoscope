module DiGraphTests

open System
open Xunit
open Graphoscope
open Graphoscope.GraphImport.Importer
open System.IO

[<Fact>]
let ``Can create empty graph and add nodes and edges`` () =
    let emptyGraph = DiGraph.create<int, float>()
    DiGraph.addNode 1 emptyGraph
    DiGraph.addNode 2 emptyGraph
    DiGraph.addNode 3 emptyGraph
    let edge = (1,3, 1.0)
    DiGraph.addEdge edge emptyGraph
   
    Assert.Equal(1.0, (DiGraph.getVolume emptyGraph))
    Assert.Equal(3.0, (DiGraph.getSize emptyGraph))
    Assert.Equal(0.6666666666666666, (DiGraph.getMeanDegree emptyGraph))

[<Fact>]
let ``Monkey graph import has correct measures`` () =
    //measures taken from http://konect.cc/networks/moreno_rhesus/
    let file = Path.Combine(Environment.CurrentDirectory, "ReferenceGraphs/out.moreno_rhesus_rhesus.txt")
    printfn "%s" file
    let monkeyGraph = createFromEdgeList file " " 2 false
    
    Assert.Equal(111.0, (DiGraph.getVolume monkeyGraph))
    Assert.Equal(16, (DiGraph.getSize monkeyGraph)) 
    Assert.Equal(13.8750,(DiGraph.getMeanDegree monkeyGraph)) 
        
