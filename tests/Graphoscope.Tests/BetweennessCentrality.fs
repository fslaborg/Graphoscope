module BetweennessCentrality

open System
open Xunit
open Graphoscope
open Graphoscope.DiGraph
open System.IO
open FSharpAux

//Contains Betweenness, Closeness Eccentricity and Distance Metrics
[<Fact>]
let ``Simple example yields the correct metrics`` () =

    //Simple graph example
    let edges =
        seq{
            1,1,2,2,1.
            1,1,4,4,1.
            1,1,5,5,1.
            1,1,6,6,1.
            2,2,3,3,1.
        }

    let graph = FGraph.ofSeq edges

    let betweennessMap =
        seq {
            1,0.600
            2,0.267
            3,0.000
            4,0.000
            5,0.000
            6,0.000
        }
        |>Map.ofSeq        

    let betweennessPaths =
        Measures.BetweennessCentrality.returnPaths (Algorithms.Dijkstra.ofUndirectedFGraphIncludingPath) id id graph

    let betweennessTest =
        graph.Keys
        |> Seq.map(fun x -> 
            let predicted = Measures.BetweennessCentrality.getBetweennessOfPathsAndNode betweennessPaths id x |> Math.round 3
            let actual = betweennessMap.Item x
            predicted=actual
            )

    Assert.DoesNotContain(false,betweennessTest)
   