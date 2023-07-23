module DiGraphTests

open System
open Xunit
open Graphoscope
open Graphoscope.DiGraph
open System.IO

[<Fact>]
let ``Can create empty graph and add nodes and edges`` () =
    let emptyGraph = Builders.create<int, float>()
    addNode emptyGraph 1
    addNode emptyGraph 2
    addNode emptyGraph 3
    let edge = (1,3, 1.0)
    addEdge emptyGraph edge
   
    Assert.Equal(1.0, (Measures.getVolume emptyGraph))
    Assert.Equal(3.0, (Measures.getSize emptyGraph))
    Assert.Equal(0.6666666666666666, (Measures.getMeanDegree emptyGraph))

[<Fact>]
let ``Monkey graph import has correct measures`` () =
    //measures taken from http://konect.cc/networks/moreno_rhesus/
    let file = Path.Combine(Environment.CurrentDirectory, "ReferenceGraphs/out.moreno_rhesus_rhesus.txt")
    printfn "%s" file
    let monkeyGraph = Import.createFromEdgeList file " " 2 false
    
    Assert.Equal(111.0, (Measures.getVolume monkeyGraph))
    Assert.Equal(16, (Measures.getSize monkeyGraph)) 
    Assert.Equal(13.8750,(Measures.getMeanDegree monkeyGraph)) 
        
[<Fact>]
let ``Node removal updates correctly`` () =
    let g =
        [|(0, 1, 1.0); (0, 2, 1.0); (1, 3, 1.0); (3, 1, 1.0); (3, 2, 1.0); (4, 0, 1.0)|]
        |> Builders.createFromEdges

    removeNode g 2

    Assert.Equal(4, Measures.getSize g)
    Assert.Equal<int * float>([|(0, 1.)|], getOutEdges g 4)
    Assert.Equal<int * float>([|(1, 1.)|], getInEdges g 3)
    Assert.Equal<(int * int * float)[]>([|(0, 1, 1.0); (1, 3, 1.0); (3, 1, 1.0); (4, 0, 1.0)|], getAllEdges g)

[<Fact>]
let ``All Pairs Dijkstra for DiGraph works correctly`` () =
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

    let g = Builders.createFromEdges edges

    let actual = Algorithms.Dijkstra.ComputeAllPairs g |> snd
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

    Assert.Equal<float[][]>(expected, actual)


[<Fact>]
let ``All Pairs Floyd Warshall for DiGraph works correctly`` () =
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

    let g = Builders.createFromEdges edges

    let actual = Algorithms.FloydWarshall.Compute g |> snd
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

    Assert.Equal<float[][]>(expected, actual)

[<Fact>]
let ``Existing edge cannot be added`` () =
    let g =
        [|(0, 1, 1.0); (0, 2, 1.0); (1, 1, 1.0); (1, 3, 1.0); (3, 2, 1.0); (4, 0, 1.0)|]
        |> Builders.createFromEdges

    Assert.ThrowsAny<Exception>(fun _ -> addEdge g (0, 1, 0.5))
