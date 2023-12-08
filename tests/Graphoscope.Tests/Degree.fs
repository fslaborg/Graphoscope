module Degree

open System
open Xunit
open Graphoscope
open Graphoscope.Graphs
open System.IO
open FSharpAux

[<Fact>]
let ``Monkey FContextMap has correct measures`` () =
    
    //measures taken from http://konect.cc/networks/moreno_rhesus/
    let file = Path.Combine(Environment.CurrentDirectory, "ReferenceGraphs/out.moreno_rhesus_rhesus.txt")
     
    let monkeyGraph: Directed.FContextMap<int,string,float> = 
        File.ReadLines file
        |> Seq.skip 2 
        |> Seq.map 
            (fun str -> 
                let arr = str.Split(' ')
                int arr.[0], arr.[0], int arr.[1], arr.[1], float arr.[2])
        |> Directed.FContextMap.ofSeq

    let degreeAverage = 13.8750
    let degreeMax = 20
    let degreeMin = 4
    let degreeDist:Set<float> = 
        seq{
            4.000000
            7.000000
            8.000000
            8.000000
            9.000000
            11.000000
            15.000000
            15.000000
            15.000000
            16.000000
            18.000000
            18.000000
            19.000000
            19.000000
            20.000000
            20.000000
        }
        |>Set.ofSeq

    let monkeyDist = Measures.Degree.sequence (monkeyGraph:Directed.FContextMap<'NodeKey,'NodeData,'EdgeData>) |>Set.ofSeq

    Assert.Equal(degreeAverage,(Measures.Degree.average monkeyGraph)) 
    Assert.Equal(degreeMax,(Measures.Degree.maximum monkeyGraph)) 
    Assert.Equal(degreeMin,(Measures.Degree.minimum monkeyGraph)) 
    Assert.True(((Set.intersect degreeDist monkeyDist) = degreeDist))
    Assert.True(((Set.intersect monkeyDist degreeDist) = degreeDist))
    Assert.Equal(12,(Measures.InDegree.maximum monkeyGraph)) 
    Assert.Equal(10,(Measures.OutDegree.maximum monkeyGraph)) 

[<Fact>]
let ``Monkey LilMatrix has correct measures`` () =
    
    //measures taken from http://konect.cc/networks/moreno_rhesus/
    let file = Path.Combine(Environment.CurrentDirectory, "ReferenceGraphs/out.moreno_rhesus_rhesus.txt")
     
    let monkeyGraph = 
        File.ReadLines file
        |> Seq.skip 2 
        |> Seq.map 
            (fun str -> 
                let arr = str.Split(' ')
                int arr.[0], arr.[0], int arr.[1], arr.[1], float arr.[2])
        |> Directed.LilMatrix.ofSeq

    let degreeAverage = 13.8750
    let degreeMax = 20
    let degreeMin = 4
    let degreeSequence = [|20; 20; 19; 19; 18; 18; 16; 15; 15; 15; 11; 9; 8; 8; 7; 4|]

    let monkeySequence = Measures.Degree.sequence monkeyGraph

    Assert.Equal(degreeAverage,(Measures.Degree.average monkeyGraph)) 
    Assert.Equal(degreeMax,(Measures.Degree.maximum monkeyGraph)) 
    Assert.Equal(degreeMin,(Measures.Degree.minimum monkeyGraph)) 
    Assert.Equal<int>(degreeSequence, monkeySequence)
    Assert.Equal(12,(Measures.InDegree.maximum monkeyGraph)) 
    Assert.Equal(10,(Measures.OutDegree.maximum monkeyGraph)) 

[<Fact>]
let `` Simple FContextMap has correct In and Out Degree Measures`` () =
    // InDegree //

    let elementsInGraph = 
        seq
            {
                0,0,1,1,1
                1,1,2,2,1
                2,2,3,3,1
                3,3,4,4,1
                4,4,0,0,1
            }

    let inGraph = Directed.FContextMap.ofSeq elementsInGraph
    
    let averageDegreeIn = 1.
    let maxIn = 1.
    let minIn = 1.
    let distIn = 
        Seq.init 5 (fun x -> 1.)|>Set.ofSeq

    let distInDegree = Measures.InDegree.sequence inGraph |>Set.ofSeq

    Assert.Equal(averageDegreeIn,(Measures.InDegree.average inGraph)) 
    Assert.Equal(maxIn,(Measures.InDegree.maximum inGraph)) 
    Assert.Equal(minIn,(Measures.InDegree.minimum inGraph)) 
    Assert.True(((Set.intersect distInDegree distIn) = distIn))
    Assert.True(((Set.intersect distIn distInDegree) = distIn))


    // OutDegree //

    let elementsOutGraph = 
        elementsInGraph
        |>Seq.map(fun (s,s1,t,t1,w) -> 
            t,t1,s,s1,w
        )

    let outGraph = Directed.FContextMap.ofSeq elementsOutGraph
    
    let distOutDegree = Measures.OutDegree.sequence inGraph |>Set.ofSeq

    Assert.Equal(averageDegreeIn,(Measures.OutDegree.average outGraph)) 
    Assert.Equal(maxIn,(Measures.OutDegree.maximum outGraph)) 
    Assert.Equal(minIn,(Measures.OutDegree.minimum outGraph)) 
    Assert.True(((Set.intersect distOutDegree distIn) = distIn))
    Assert.True(((Set.intersect distIn distOutDegree) = distIn))



[<Fact>]
let `` Simple LilMatrix has correct In and Out Degree Measures`` () =
    // InDegree //

    let elementsInGraph = 
        seq
            {
                0,0,1,1,1
                1,1,2,2,1
                2,2,3,3,1
                3,3,4,4,1
                4,4,0,0,1
            }

    let graph = Directed.LilMatrix.ofSeq elementsInGraph
    
    let averageDegreeIn = 1.
    let maxIn = 1.
    let minIn = 1.
    let distIn = [|1; 1; 1; 1; 1;|]
    let distOut = [|1; 1; 1; 1; 1;|]

    let distInDegree = Measures.InDegree.sequence graph
    let distOutDegree = Measures.OutDegree.sequence graph

    Assert.Equal(averageDegreeIn,(Measures.InDegree.average graph)) 
    Assert.Equal(maxIn,(Measures.InDegree.maximum graph)) 
    Assert.Equal(minIn,(Measures.InDegree.minimum graph)) 
    Assert.Equal<int>(distInDegree, distIn)
    Assert.Equal<int>(distOutDegree, distOut)

