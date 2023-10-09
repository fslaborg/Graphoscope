module EccentricityCentrality

open System
open Xunit
open Graphoscope
open Graphoscope.DiGraph
open System.IO
open FSharpAux

// //Contains Betweenness, Closeness Eccentricity and Distance Metrics
// [<Fact>]
// let ``Simple example yields the correct metrics`` () =
    // <Compile Include="Distance.fs" />
    // <Compile Include="BetweennessCentrality.fs" />
    // <Compile Include="Eccentricity.fs" />
    // <Compile Include="ClosenessCentrality.fs" />
//     //Simple graph example
//     let edges =
//         seq{
//             1,1,2,2,1.
//             1,1,4,4,1.
//             1,1,5,5,1.
//             1,1,6,6,1.
//             2,2,3,3,1.
//         }

//     let graph = UndirectedFGraph.ofSeq edges

//     let graph2d = UndirectedFGraph.toArray2D (fun x -> x-1) graph

//     let floydWarshall = Algorithms.FloydWarshall.fromArray2D graph2d

//     let eccentricityMap =
//         seq {
//             1,2
//             2,2
//             3,3
//             4,3
//             5,3
//             6,3
//         }
//         |>Map.ofSeq   

//     let eccentricityPredicted = 
//         Measures.Eccentricity.ofFGraph2D floydWarshall 
    
//     let eccentricityTest =
//         seq{
//             for (kvp) in eccentricityPredicted do
//                 let key,valu = kvp.Key,kvp.Value
//                 let actualVal = eccentricityMap.Item (key+1)
//                 valu=actualVal
//         }

//     Assert.DoesNotContain(false,eccentricityTest)
   