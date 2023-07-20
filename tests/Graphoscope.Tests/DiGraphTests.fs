module DiGraphTests

open System
open Xunit
open Graphoscope
open Graphoscope.Import
open System.IO

[<Fact>]
let ``Can create empty graph and add nodes and edges`` () =
    let emptyGraph = DiGraph.create<int, float>()
    DiGraph.Nodes.add emptyGraph 1
    DiGraph.Nodes.add emptyGraph 2
    DiGraph.Nodes.add emptyGraph 3
    let edge = (1,3, 1.0)
    DiGraph.Edges.add emptyGraph edge
   
    Assert.Equal(1.0, (DiGraph.Measures.getVolume emptyGraph))
    Assert.Equal(3.0, (DiGraph.Measures.getSize emptyGraph))
    Assert.Equal(0.6666666666666666, (DiGraph.Measures.getMeanDegree emptyGraph))

[<Fact>]
let ``Monkey graph import has correct measures`` () =
    //measures taken from http://konect.cc/networks/moreno_rhesus/
    let file = Path.Combine(Environment.CurrentDirectory, "ReferenceGraphs/out.moreno_rhesus_rhesus.txt")
    printfn "%s" file
    let monkeyGraph = createFromEdgeList file " " 2 false
    
    Assert.Equal(111.0, (DiGraph.Measures.getVolume monkeyGraph))
    Assert.Equal(16, (DiGraph.Measures.getSize monkeyGraph)) 
    Assert.Equal(13.8750,(DiGraph.Measures.getMeanDegree monkeyGraph)) 
        
