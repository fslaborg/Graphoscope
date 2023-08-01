module Loop

open System
open Xunit
open Graphoscope
open Graphoscope.DiGraph
open System.IO
open FSharpAux

[<Fact>]
let ``Monkey graph import has correct measures`` () =
    //measures taken from http://konect.cc/networks/moreno_rhesus/
    let file = Path.Combine(Environment.CurrentDirectory, "ReferenceGraphs/out.moreno_rhesus_rhesus.txt")
    
    let monkeyGraph = 
        File.ReadLines file
        |> Seq.skip 2 
        |> Seq.map 
            (fun str -> 
                let arr = str.Split(' ')
                int arr.[0], arr.[0], int arr.[1], arr.[1], float arr.[2])
        |> FGraph.ofSeq
    
    Assert.Equal(0.,(Measures.Loop.loopCountFGraph monkeyGraph)) 
    
[<Fact>]
let ``Fully looped graph has correct measures`` () =
    let nodes = [0 .. 10]
    let edges = 
        nodes|>List.map(fun x -> x,x,1.)

    let counts: obj = nodes.Length

    let g = FGraph.empty|>FGraph.addNodes (nodes|>List.map(fun x -> x,x))|>FGraph.addEdges edges
 
    Assert.Equal(counts,(Measures.Loop.loopCountFGraph g)) 
