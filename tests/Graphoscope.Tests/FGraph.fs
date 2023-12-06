module FContextMap

open System
open System.Collections.Generic
open Xunit
open Graphoscope
open Graphoscope.Graphs

[<Fact>]
let ``Can create empty graph and add nodes and edges`` () =
    let graph =
        Directed.FContextMap.empty
        |> Directed.FContextMap.addNode 1 'A'
        |> Directed.FContextMap.addNode 2 'B'
        |> Directed.FContextMap.addNode 3 'C'

        |> Directed.FContextMap.addEdge 1 3 1.0
   
    Assert.Equal(1.0, (Directed.FContextMap.countEdges graph))
    Assert.Equal(3.0, (Directed.FContextMap.countNodes graph))
    Assert.Equal(0.6666666666666666, (Measures.Degree.average graph))


[<Fact>]
let ``Can create a graph with multiple nodes and edges including loops`` () =
    let graph =
       Directed.FContextMap.empty
        |> Directed.FContextMap.addElement 0 0 1 0 true
        |> Directed.FContextMap.addElement 0 0 2 0 true
        |> Directed.FContextMap.addElement 1 0 2 0 true
        |> Directed.FContextMap.addElement 2 0 0 0 true
        |> Directed.FContextMap.addElement 2 0 3 0 true
        // Tests loops where node exists
        |> Directed.FContextMap.addElement 3 0 3 0 true
        |> Directed.FContextMap.addElement 5 0 3 0 true
        |> Directed.FContextMap.addElement 6 0 6 0 true
   
    Assert.Equal(8.0, (Directed.FContextMap.countEdges graph))
    Assert.Equal(6.0, (Directed.FContextMap.countNodes graph))
    Assert.Equal(2.6666666666666666, (Measures.Degree.average graph))

[<Fact>]
let ``Can reverse a given FContextMap's Edges`` () =
    let originalGraph = 
        Directed.FContextMap.empty
        |> Directed.FContextMap.addElement 0 0 1 0 true
        |> Directed.FContextMap.addElement 0 0 2 0 true
        |> Directed.FContextMap.addElement 1 0 2 0 true
        |> Directed.FContextMap.addElement 2 0 0 0 true
        |> Directed.FContextMap.addElement 2 0 3 0 true

    let originalGraphManuallyReversed =
        Directed.FContextMap.empty
        |> Directed.FContextMap.addElement 1 0 0 0 true
        |> Directed.FContextMap.addElement 2 0 0 0 true
        |> Directed.FContextMap.addElement 2 0 1 0 true
        |> Directed.FContextMap.addElement 0 0 2 0 true
        |> Directed.FContextMap.addElement 3 0 2 0 true

    let revGraph = Directed.FContextMap.reverseEdges originalGraph

    let manRevGraphKeys = originalGraphManuallyReversed.Keys |> Seq.sort
    let revGraphKeys = revGraph.Keys |> Seq.sort
    //let manRevGraphVals = originalGraphManuallyReversed.Values |> Seq.map (fun (n1,e,n2) -> List.ofSeq n1.Keys, e, Seq.toList n2.Keys)
    //let revGraphVals = revGraph.Values |> Seq.map (fun (n1,e,n2) -> Seq.toList n1.Keys, e, List.ofSeq n2.Keys)
    let manRevGraphVals = originalGraphManuallyReversed.Values |> Seq.map (fun (n1,e,n2) -> List.ofSeq n1.Keys, e, Seq.toList n2.Keys) |> Seq.sort
    let revGraphVals = revGraph.Values |> Seq.map (fun (n1,e,n2) -> Seq.toList n1.Keys, e, List.ofSeq n2.Keys) |> Seq.sort

    Assert.Equal<int seq>(manRevGraphKeys, revGraphKeys)
    Assert.Equal<int list * int * int list>(manRevGraphVals, revGraphVals)
    Assert.Equal(5.0, (Directed.FContextMap.countEdges revGraph))
    Assert.Equal(4.0, (Directed.FContextMap.countNodes revGraph))