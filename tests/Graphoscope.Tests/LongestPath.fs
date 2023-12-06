module LongestPath

open System
open Xunit
open Graphoscope
open Graphoscope.Graphs
open System.IO

open Xunit

[<Fact>]
let ``smallCyclicGraphReturnsExceptedResults``() =
    let cyclicGraphExample = 
        seq{
            "a","b",1.
            "a","c",1.
            "a","d",15.
            "a","e",1.
            "c","e",1.
            "b","d",13.
            "d","e",1.
            "e","b",1.
            "d","d",2.
        }    
        |> Seq.map(fun (s, t, w) -> (s, s, t, t, w))
        |> Directed.FContextMap.ofSeq

    let ex = new System.Exception("The given FContextMap is not a Directed Acyclic Graph!")

    // Assert
    Assert.Throws<Exception>(fun () -> Measures.LongestPath.compute("a", cyclicGraphExample) |> ignore)
        |> fun thrown -> Assert.Equal(ex.Message, thrown.Message)

[<Fact>]
let smallAcyclicGraphReturnsExceptedResults () =
    
    let acyclicGraphExample = 
        seq{
            "A","B",2.
            "A","D",1. 
            "B","C",2.
            "D","C",17.2
            "A","C",18.

        }    
        |>Seq.map(fun (s,t,w) -> (s,s,t,t,w))
        |>Directed.FContextMap.ofSeq

    Assert.Equal((["A"; "D"; "C"], 18.2),Measures.LongestPath.computeByEdgeData("A",acyclicGraphExample))
    Assert.Equal((["A"; "B"; "C"], 2.),Measures.LongestPath.compute("A",acyclicGraphExample))
