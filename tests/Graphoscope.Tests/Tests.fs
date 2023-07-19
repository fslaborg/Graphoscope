module Tests

open System
open System.Collections.Generic
open Xunit
open Graphoscope
open Graphoscope.GraphImport.Importer
open System.IO

[<Fact>]
let ``Undirected unweighted graph created from tab delimited edgelist`` () =
    let file = Path.Combine(Environment.CurrentDirectory, "ReferenceGraphs/powergrid.edgelist.txt")
    printfn "%s" file
    let powerGridGraph = createFromEdgeList file "\t" 0 false
    
    Assert. NotNull powerGridGraph
        
[<Fact>]
let ``powergrid import has correct volume`` () =
    let file = Path.Combine(Environment.CurrentDirectory, "ReferenceGraphs/powergrid.edgelist.txt")
    printfn "%s" file
    let powerGridGraph = createFromEdgeList file "\t" 0 false
    
    Assert.Equal((DiGraph.getVolume powerGridGraph), 6594) 
        

[<Fact>]
let ``Karate graph import has correct measures`` () =
    //measures taken from http://konect.cc/networks/ucidata-zachary/
    let file = Path.Combine(Environment.CurrentDirectory, "ReferenceGraphs/zachary.txt")
    printfn "%s" file
    let karateGraph = createFromEdgeList file "\t" 2 false
    
    Assert.Equal((DiGraph.getVolume karateGraph), 78) 
    Assert.Equal((DiGraph.getSize karateGraph), 34) 
    Assert.Equal((DiGraph.getMeanDegree karateGraph), 4.58824) 
        
