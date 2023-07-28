module UndirectedTests

open System
open System.Collections.Generic
open Xunit
open Graphoscope
open Graphoscope.Undirected.UndirectedHelpers
open System.IO
open Graphoscope.Undirected.UndirectedGraph

//[<Fact>]
//let ``Undirected unweighted graph created from tab delimited edgelist`` () =
//   let file = Path.Combine(Environment.CurrentDirectory, "ReferenceGraphs/powergrid.edgelist.txt")
//   printfn "%s" file
//   let powerGridGraph = createFromEdgeList file "\t" 0 false

//   Assert.Equal(powerGridGraph.Volume(), 6594)

//[<Fact>]
//let ``Undirected unweighted karate graph created from space delimited edgelist`` () =
//   let file = Path.Combine(Environment.CurrentDirectory, "ReferenceGraphs/zachary.txt")
//   printfn "%s" file
//   let powerGridGraph = createFromEdgeList file "\t" 0 false

//   Assert.Equal(powerGridGraph.Size(), 34)

//[<Fact>]
//let ``Karate Graph has correct basic nmeasures after import`` () =
//   let file = Path.Combine(Environment.CurrentDirectory, "ReferenceGraphs/zachary.txt")
//   printfn "%s" file
//   let powerGridGraph = createFromEdgeList file "\t" 0 false
   
//   Assert.Equal(powerGridGraph.Volume(), 34)
//   Assert.Equal(powerGridGraph.Size(), 78)
//   Assert.Equal(powerGridGraph.MeanDegree(), 4.588235294)


//[<Fact>]
//let ``Can create empty graph and add nodes and edges`` () =
//   let emptyGraph = UndirectedGraph<int>()
//   emptyGraph.AddNode 1
//   emptyGraph.AddNode 2
//   emptyGraph.AddNode 3
//   let edge = (1,3)
//   emptyGraph.AddEdge edge
   
//   Assert.Equal(emptyGraph.Volume(), 1)
//   Assert.Equal(emptyGraph.Size(), 3)
//   Assert.Equal(emptyGraph.MeanDegree(), 0.666)