module GraphDensity

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
    Assert.Equal(0.925,(Measures.GraphDensity.ofFGraph monkeyGraph)) 
    //Assert.Equal(13.8750,(Measures.GraphDensity.ofFGraph monkeyGraph)) 
    
[<Fact>]
let ``Fully connected graph has correct measures`` () =
    let nodes = [0 .. 10]
    let edges = 
        [for n=0 to 10 do
            for i=n+1 to 10 do
                nodes.[n],nodes.[i],1.
        ]

    let g = FGraph.empty|>FGraph.Node.addMany (nodes|>List.map(fun x -> x,x))|>FGraph.Edge.addMany edges
 
    Assert.Equal(1.,(Measures.GraphDensity.ofFGraph g)) 
