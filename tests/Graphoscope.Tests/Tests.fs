module Tests

open System
open System.Collections.Generic
open Xunit
open Graphoscope

[<Fact>]
let ``My test`` () =
    Assert.True(true)

[<Fact>]
let ``DiGraph Creation Nodes Equality`` () =
    Assert.Equal((DiGraph.create<int>()).Nodes,
        new Vec<int>()
    )
[<Fact>]
let ``DiGraph Creation OutEdges Equality`` () =
    Assert.Equal((DiGraph.create<int>()).OutEdges,
        new Vec<Vec<(int * float)>>()
    )
