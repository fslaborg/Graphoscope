module Degree

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
    
    
    //Assert.Equal(111.0, (Measures.getVolume monkeyGraph))
    //Assert.Equal(16, (Measures.getSize monkeyGraph)) 
    Assert.Equal(13.8750,(Measures.Degree.average monkeyGraph)) 