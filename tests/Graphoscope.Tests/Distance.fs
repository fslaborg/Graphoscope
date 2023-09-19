module Distance

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

    let graph = UndirectedFGraph.ofSeq edges

    let graph2d = FGraph.toArray2D(fun x -> x-1) graph

    let floydWarshall = Algorithms.FloydWarshall.fromArray2D graph2d

    let distanceAverage,distanceMax,distanceMin =
        2.3333333333333335,
        3.,
        1.
          
    let distanceAveragePred,distanceMaxPred,distanceMinPred =
        Measures.Distance.averageOfFloydWarshall floydWarshall,
        Measures.Distance.maxOfFloydWarshall floydWarshall,
        Measures.Distance.minOfFloydWarshall floydWarshall

    Assert.Equal<float>(distanceAverage,distanceAveragePred)
    Assert.Equal<float>(distanceMax,distanceMaxPred)
    Assert.Equal<float>(distanceMin,distanceMinPred)
