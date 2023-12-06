module BellmanFord

open Xunit
open Graphoscope
open Graphoscope.Graphs
open FSharpAux

let bfTest =
    Directed.FContextMap.empty
    |> Directed.FContextMap.addElement 0 "Node A" 1 "Node B" -1.
    |> Directed.FContextMap.addElement 0 "Node A" 2 "Node C" 4.
    |> Directed.FContextMap.addElement 1 "Node B" 2 "Node C" 3.
    |> Directed.FContextMap.addElement 1 "Node B" 3 "Node D" 2.
    |> Directed.FContextMap.addElement 1 "Node B" 4 "Node E" 2.
    |> Directed.FContextMap.addElement 3 "Node D" 2 "Node C" 5.
    |> Directed.FContextMap.addElement 3 "Node D" 1 "Node B" 1.
    |> Directed.FContextMap.addElement 4 "Node E" 3 "Node D" -3.


[<Fact>]
let ``BellmanFord simple example on Directed.FContextMap works correctly`` () =

    let actual = Algorithms.BellmanFord.ofFContextMap 0 bfTest

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

    let bf = Algorithms.BellmanFord.ofFContextMap 0 bfTest
    let actual = Algorithms.BellmanFord.hasNegativeCycles bfTest bf

    let expected = false 

    Assert.Equal<bool>(expected, actual)


[<Fact>]
let ``BellmanFord with cycle`` () =

    let bfTestWithCycle =
        Directed.FContextMap.empty
        |> Directed.FContextMap.addElement 0 "Node A" 1 "Node B" -1.
        |> Directed.FContextMap.addElement 0 "Node A" 2 "Node C" 4.
        |> Directed.FContextMap.addElement 1 "Node B" 2 "Node C" 3.
        |> Directed.FContextMap.addElement 1 "Node B" 3 "Node D" 2.
        |> Directed.FContextMap.addElement 1 "Node B" 4 "Node E" 2.
        |> Directed.FContextMap.addElement 3 "Node D" 2 "Node C" 5.
        |> Directed.FContextMap.addElement 3 "Node D" 1 "Node B" 1.
        |> Directed.FContextMap.addElement 4 "Node E" 3 "Node D" -3.
        |> Directed.FContextMap.addElement 3 "Node D" 0 "Node A" -1.
        |> Directed.FContextMap.addElement 0 "Node A" 4 "Node E" -1.

    let bf = Algorithms.BellmanFord.ofFContextMap 0 bfTestWithCycle
    let actual = Algorithms.BellmanFord.hasNegativeCycles bfTestWithCycle bf

    let expected = true 

    Assert.Equal<bool>(expected, actual)

