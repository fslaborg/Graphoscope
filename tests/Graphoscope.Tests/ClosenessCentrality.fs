module ClosenessCentrality

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

    let closeness =
        seq {
            1,0.833
            2,0.625
            3,0.417
            4,0.500
            5,0.500
            6,0.500
        }|>Map.ofSeq  

    let closenessPredicted =
        Measures.ClosenessCentrality.ofFGraphNormalised (Algorithms.Dijkstra.ofUndirectedFGraph) id graph
        |> Seq.map(fun x -> x.Key, Math.round 3 x.Value)|>Map.ofSeq
    
    let closenessTest =
        seq{
            for i in graph.Keys do
                let close = closeness.Item i 
                let closePred = closenessPredicted.Item i
                (close-closePred)
                |>abs
                |>fun x -> x<0.001
        }

    Assert.DoesNotContain(false,closenessTest)
    