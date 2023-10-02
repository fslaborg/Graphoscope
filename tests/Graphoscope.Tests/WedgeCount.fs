module WedgeCount

open Xunit
open Graphoscope

[<Fact>]
let ``Wedge Count algorithm test graph without cycles`` () =
    // the wedges in the graph below are
    // 1, 8 and 1, 3
    // 1, 3 and 1, 2
    // 1, 2 and 2, 5
    // 1, 2 and 2, 4
    // 1, 3 and 3, 6
    // 1, 2 and 1, 8
    // 2, 5 and 2, 4
    // 2, 5 and 5, 7
    let dummyEdgeData = 0.1
    let actual =
            FGraph.empty
            |> FGraph.addElement 1 "Node 1" 2 "Node 2" dummyEdgeData
            |> FGraph.addElement 1 "Node 1" 3 "Node 3" dummyEdgeData
            |> FGraph.addElement 1 "Node 1" 8 "Node 8" dummyEdgeData
            |> FGraph.addElement 2 "Node 2" 4 "Node 4" dummyEdgeData
            |> FGraph.addElement 2 "Node 2" 5 "Node 5" dummyEdgeData
            |> FGraph.addElement 3 "Node 3" 6 "Node 6" dummyEdgeData
            |> FGraph.addElement 5 "Node 5" 7 "Node 7" dummyEdgeData
            |> Algorithms.WedgeCount.ofGraph
    let expected = 8
    Assert.Equal(expected, actual)

[<Fact>]
let ``Wedge Count algorithm test triangle cycle`` ()=
    // the wedges in the graph below are
    // 1, 2 and 2, 3
    // 1, 2 and 1, 3
    // 2, 3 and 1, 3
    let actual =
        FGraph.empty
        |> FGraph.addElement 1 "Node 1" 2 "Node 2" 0.1
        |> FGraph.addElement 2 "Node 2" 3 "Node 3" 0.1
        |> FGraph.addElement 3 "Node 3" 1 "Node 1" 0.1
        |> Algorithms.WedgeCount.ofGraph

    let expected = 3
    Assert.Equal(expected, actual)