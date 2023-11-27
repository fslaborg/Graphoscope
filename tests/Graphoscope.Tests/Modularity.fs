module Modularity

open Xunit
open Graphoscope
open FSharpAux

[<Fact>]
let ``UndirectedGraph modularity works correctly `` () =
    let e =
        [|
            ("a", "b", 1.0)
            ("a", "c", 1.0)
            ("b", "c", 1.0)
            ("b", "d", 1.0)  // inter-community edge
            ("d", "e", 1.0)
            ("d", "f", 1.0)
            ("d", "g", 1.0)
            ("f", "g", 1.0)
            ("f", "e", 1.0)
        |]

    let g = UndirectedGraph.createFromEdges e
    let partition = [|set ["a"; "b"; "c"]; set ["d"; "e"; "f"; "g"]|]
    
    let actual =  Measures.Modularity.ofUndirectedGraph id 1. partition g
    let expected = ((3./9.) - (7./18.)**2) + ((5./9.) - (11./18.)**2)

    Assert.Equal(expected, actual, 15)

[<Fact>]
let ``DiGraph modularity works correctly `` () =
    let g = DiGraph.createFromEdges [|(2,1,1.); (2,3,1.); (3,4,1.)|]
    let partition = [|set [1; 2]; set [3; 4]|]

    let actual = Measures.Modularity.ofDiGraph id 1. partition g
    let expected = 2. / 9.

    Assert.Equal(expected, actual)
