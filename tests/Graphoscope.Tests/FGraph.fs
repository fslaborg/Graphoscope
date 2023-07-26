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
    //Assert.Equal(0.6666666666666666, (Measures.getMeanDegree emptyGraph))

