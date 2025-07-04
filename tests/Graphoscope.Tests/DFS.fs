module DFS


open Xunit
open Graphoscope
open Graphoscope.Algorithms
open ReferenceObjects



[<Fact>]
let ``DFS simple example on FGraph works correctly`` () =

    let actual =
        FGraph.empty
        |> FGraph.addElement 1 "Node 1" 2 "Node 2" 0.1  
        |> FGraph.addElement 1 "Node 1" 3 "Node 3" 0.1
        |> FGraph.addElement 2 "Node 2" 4 "Node 4" 0.1  
        |> FGraph.addElement 2 "Node 2" 5 "Node 5" 0.1
        |> FGraph.addElement 3 "Node 3" 6 "Node 6" 0.1
        |> FGraph.addElement 5 "Node 5" 7 "Node 7" 0.1
        |> DFS.ofFGraph 1


    // DFS Traversal Result (starting node 1):
    let expected = [( 1, "Node 1"); ( 3, "Node 3"); ( 6, "Node 6"); ( 2, "Node 2");
                        ( 5, "Node 5"); ( 7, "Node 7"); ( 4, "Node 4");]

    Assert.Equal<seq<int*string>>(expected, actual)


[<Fact>]
let ``DFS.ofFGraphBy returns correct result`` () =

    let actual = DFS.ofFGraphBy 1 (fun nk _ _ -> nk <> 4) fGraph1 |> Seq.toList
    let expected = [(1, ""); (7, ""); (2, ""); (3, "")]

    Assert.Equal<(int * string) list>(expected, actual)


[<Fact>]
let ``DFS.ofFGraphWithDepth returns correct result`` () =

    let actual = DFS.ofFGraphWithDepth 1 3 fGraph1 |> Seq.toList
    let expected = [(1, ""); (7, ""); (2, ""); (3, ""); (4, "")]

    Assert.Equal<(int * string) list>(expected, actual)


[<Fact>]
let ``DFS.ofFGraphWithDepthBy returns correct result`` () =

    let actual1 = DFS.ofFGraphWithDepthBy 1 3 (fun _ _ _ -> true) fGraph1 |> Seq.toList
    let actual2 = DFS.ofFGraphWithDepthBy 1 System.Int32.MaxValue (fun _ _ _ -> true) fGraph1 |> Seq.toList
    let expected1 = [(1, ""); (7, ""); (2, ""); (3, ""); (4, "")]
    let expected2 = [(1, ""); (7, ""); (2, ""); (3, "")]

    Assert.Equal<(int * string) list>(expected1, actual1)
    Assert.Equal<(int * string) list>(expected2, actual2)


[<Fact>]
let ``DFS works on DiGraph`` () =

    let actual =
        DiGraph.empty
        |> DiGraph.addNode 1 1
        |> DiGraph.addNode 2 2 
        |> DiGraph.addNode 3 3 
        |> DiGraph.addNode 4 4 
        |> DiGraph.addNode 5 5 
        |> DiGraph.addNode 6 6 
        |> DiGraph.addNode 7 7
        |> DiGraph.addEdge (1, 2, 1.0)
        |> DiGraph.addEdge (1, 3, 1.0)
        |> DiGraph.addEdge (2, 4, 1.0)
        |> DiGraph.addEdge (2, 5, 1.0)
        |> DiGraph.addEdge (3, 6, 1.0)
        |> DiGraph.addEdge (5, 7, 1.0)
        |> DFS.ofDiGraph 1


    // DFS Traversal Result (starting node 1):
    let expected = [( 1, 1); ( 3, 3); ( 6, 6); ( 2, 2);
                        ( 5, 5); ( 7, 7); ( 4, 4);]

    Assert.Equal<seq<int*int>>(expected, actual)

