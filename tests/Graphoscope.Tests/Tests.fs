module Tests

open System
open System.Collections.Generic
open Xunit
open Graphoscope

[<Fact>]
let ``My test`` () =
    Assert.True(true)

//[<Fact>]
//let ``DiGraph Creation test`` () =
//    Assert.Equal(DiGraph.create<int>(),
//        {
//            IdMap = Dictionary()
//            Nodes = ResizeArray()
//            OutEdges = ResizeArray()
//            // InEdges = ResizeArray()
//        })
