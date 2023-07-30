module BellmanFord

open Xunit
open Graphoscope
open FSharpAux

let bfTest =
    FGraph.empty
    |> FGraph.addElement 0 "Node A" 1 "Node B" -1.
    |> FGraph.addElement 0 "Node A" 2 "Node C" 4.
    |> FGraph.addElement 1 "Node B" 2 "Node C" 3.
    |> FGraph.addElement 1 "Node B" 3 "Node D" 2.
    |> FGraph.addElement 1 "Node B" 4 "Node E" 2.
    |> FGraph.addElement 3 "Node D" 2 "Node C" 5.
    |> FGraph.addElement 3 "Node D" 1 "Node B" 1.
    |> FGraph.addElement 4 "Node E" 3 "Node D" -3.


[<Fact>]
let ``BellmanFord simple example on FGraph works correctly`` () =

    let actual = Algorithms.BellmanFord.ofFGraph 0 bfTest

    // Vertex   Distance from Source
    // 0          0
    // 1          -1
    // 2          2
    // 3          -2
    // 4          1

    let expected = [(0, 0.0); (1, -1.); (2, 2.0); (3, -2.0); (4, 1.0)] |> Set.ofList 

    Assert.Equal<Set<int*float>>(expected, actual |> Dictionary.toSeq |> Set.ofSeq)



[<Fact>]
let ``BellmanFord without cycle`` () =

    let bf = Algorithms.BellmanFord.ofFGraph 0 bfTest
    let actual = Algorithms.BellmanFord.hasNegativeCycles bfTest bf

    let expected = false 

    Assert.Equal<bool>(expected, actual)


[<Fact>]
let ``BellmanFord with cycle`` () =

    let bfTestWithCycle =
        FGraph.empty
        |> FGraph.addElement 0 "Node A" 1 "Node B" -1.
        |> FGraph.addElement 0 "Node A" 2 "Node C" 4.
        |> FGraph.addElement 1 "Node B" 2 "Node C" 3.
        |> FGraph.addElement 1 "Node B" 3 "Node D" 2.
        |> FGraph.addElement 1 "Node B" 4 "Node E" 2.
        |> FGraph.addElement 3 "Node D" 2 "Node C" 5.
        |> FGraph.addElement 3 "Node D" 1 "Node B" 1.
        |> FGraph.addElement 4 "Node E" 3 "Node D" -3.
        |> FGraph.addElement 3 "Node D" 0 "Node A" -1.
        |> FGraph.addElement 0 "Node A" 4 "Node E" -1.

    let bf = Algorithms.BellmanFord.ofFGraph 0 bfTestWithCycle
    let actual = Algorithms.BellmanFord.hasNegativeCycles bfTestWithCycle bf

    let expected = true 

    Assert.Equal<bool>(expected, actual)

