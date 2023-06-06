module Tests

open System
open Xunit
open Graphoscope

[<Fact>]
let ``My test`` () =
    Assert.True(true)

[<Fact>]
let ``A sample test using a function from the referenced Graphoscope project`` () =
    Assert.Equal(GraphPlaceholder.hello "world", "Hello, world!")
