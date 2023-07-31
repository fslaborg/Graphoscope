module FGraph

open System
open System.Collections.Generic
open Xunit
open Graphoscope

[<Fact>]
let ``Can create empty graph and add nodes and edges`` () =
    let graph =
        FGraph.empty
        |> FGraph.Node.add 1 'A'
        |> FGraph.Node.add 2 'B'
        |> FGraph.Node.add 3 'C'

        |> FGraph.Edge.add 1 3 1.0
   
    Assert.Equal(1.0, (FGraph.Edge.count graph))
    Assert.Equal(3.0, (FGraph.Node.count graph))
    Assert.Equal(0.6666666666666666, (Measures.Degree.average graph))


[<Fact>]
let ``Can create a graph with multiple nodes and edges including loops`` () =
    let graph =
        FGraph.empty
        |> FGraph.addElement 0 0 1 0 true
        |> FGraph.addElement 0 0 2 0 true
        |> FGraph.addElement 1 0 2 0 true    
        |> FGraph.addElement 2 0 0 0 true
        |> FGraph.addElement 2 0 3 0 true
        // Tests loops where node exists
        |> FGraph.addElement 3 0 3 0 true
        |> FGraph.addElement 5 0 3 0 true
        |> FGraph.addElement 6 0 6 0 true
   
    Assert.Equal(8.0, (FGraph.Edge.count graph))
    Assert.Equal(6.0, (FGraph.Node.count graph))
    Assert.Equal(2.6666666666666666, (Measures.Degree.average graph))

[<Fact>]
let ``Can reverse a given FGraph's Edges`` () =
    let originalGraph = 
        FGraph.empty
        |> FGraph.addElement 0 0 1 0 true
        |> FGraph.addElement 0 0 2 0 true
        |> FGraph.addElement 1 0 2 0 true
        |> FGraph.addElement 2 0 0 0 true
        |> FGraph.addElement 2 0 3 0 true

    let originalGraphManuallyReversed =
        FGraph.empty
        |> FGraph.addElement 1 0 0 0 true
        |> FGraph.addElement 2 0 0 0 true
        |> FGraph.addElement 2 0 1 0 true
        |> FGraph.addElement 0 0 2 0 true
        |> FGraph.addElement 3 0 2 0 true

    let revGraph = FGraph.reverseEdges originalGraph

    Assert.Equal<int seq>(originalGraphManuallyReversed.Keys |> Seq.sort, revGraph.Keys |> Seq.sort)