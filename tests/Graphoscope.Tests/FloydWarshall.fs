module FloydWarshall

open System
open Xunit
open Graphoscope
open System.IO
open FSharpAux

let edges =
    [|(0, 3, 1.0); (0, 5, 1.0); (0, 6, 1.0); (0, 7, 1.0); (0, 8, 1.0);
        (0, 9, 1.0); (1, 0, 1.0); (1, 4, 1.0); (1, 9, 1.0); (2, 1, 1.0);
        (2, 4, 1.0); (2, 6, 1.0); (3, 1, 1.0); (3, 2, 1.0); (3, 4, 1.0);
        (3, 5, 1.0); (3, 6, 1.0); (3, 7, 1.0); (4, 0, 1.0); (4, 1, 1.0);
        (4, 3, 1.0); (4, 6, 1.0); (4, 7, 1.0); (4, 8, 1.0); (5, 1, 1.0);
        (5, 2, 1.0); (5, 4, 1.0); (5, 6, 1.0); (6, 2, 1.0); (6, 3, 1.0);
        (6, 4, 1.0); (6, 5, 1.0); (6, 8, 1.0); (7, 0, 1.0); (7, 3, 1.0);
        (7, 5, 1.0); (7, 8, 1.0); (7, 9, 1.0); (8, 2, 1.0); (8, 3, 1.0);
        (8, 4, 1.0); (8, 5, 1.0); (8, 9, 1.0); (9, 0, 1.0); (9, 3, 1.0);
        (9, 4, 1.0); (9, 5, 1.0)|]

let expected =
    [|
        [|0.0; 2.0; 2.0; 1.0; 2.0; 1.0; 1.0; 1.0; 1.0; 1.0|]
        [|1.0; 0.0; 3.0; 2.0; 1.0; 2.0; 2.0; 2.0; 2.0; 1.0|]
        [|2.0; 1.0; 0.0; 2.0; 1.0; 2.0; 1.0; 2.0; 2.0; 2.0|]
        [|2.0; 1.0; 1.0; 0.0; 1.0; 1.0; 1.0; 1.0; 2.0; 2.0|]
        [|1.0; 1.0; 2.0; 1.0; 0.0; 2.0; 1.0; 1.0; 1.0; 2.0|]
        [|2.0; 1.0; 1.0; 2.0; 1.0; 0.0; 1.0; 2.0; 2.0; 2.0|]
        [|2.0; 2.0; 1.0; 1.0; 1.0; 1.0; 0.0; 2.0; 1.0; 2.0|]
        [|1.0; 2.0; 2.0; 1.0; 2.0; 1.0; 2.0; 0.0; 1.0; 1.0|]
        [|2.0; 2.0; 1.0; 1.0; 1.0; 1.0; 2.0; 2.0; 0.0; 1.0|]
        [|1.0; 2.0; 2.0; 1.0; 1.0; 1.0; 2.0; 2.0; 2.0; 0.0|]
    |]

[<Fact>]
let ``All Pairs Floyd Warshall for DiGraph works correctly`` () =

    let actual = 
        DiGraph.createFromEdges edges
        |> DiGraph.toMatrix
        |> Algorithms.FloydWarshall.fromJaggedArray 

    Assert.Equal<float[]>(expected, actual)

[<Fact>]
let ``All Pairs Floyd Warshall for FGraph works correctly`` () =

    let actual = 
        edges
        |> Seq.map (fun (s, t, w) -> s,s,t,t,w)
        |> FGraph.ofSeq
        |> FGraph.toArray2D (id)
        |> Algorithms.FloydWarshall.fromArray2D 
    printfn "%A" actual

    let expected = Array2D.init 10 10 (fun n m -> expected.[n].[m])
    Assert.Equal<float[,]>(expected, actual)