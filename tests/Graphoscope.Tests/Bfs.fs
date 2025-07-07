module BFS

open Xunit
open Graphoscope
open Graphoscope.Algorithms



[<Fact>]
let ``BFS simple example on FGraph works correctly`` () =

    let actual =
        FGraph.empty
        |> FGraph.addElement 0 'A' 1 'B' 0.1
        |> FGraph.addElement 0 'A' 2 'C' 0.1
        |> FGraph.addElement 1 'B' 2 'C' 0.1
        |> FGraph.addElement 2 'C' 0 'A' 0.1
        |> FGraph.addElement 2 'C' 3 'D' 0.1
        |> FGraph.addElement 3 'D' 3 'D' 0.1
        |> Algorithms.BFS.ofFGraph 2
        |> Seq.toList

    // Following is Breadth First Traversal (starting from vertex 2) 
    // 2 0 3 1
    let expected = [(2,'C');(0,'A');(3,'D');(1,'B')]

    Assert.Equal<seq<int*char>>(expected, actual)


[<Fact>]
let ``BFS.ofFGraphBy returns correct result`` () =

    let actual = BFS.ofFGraphBy 1 (fun nk _ _ -> nk < 5) ReferenceObjects.fGraph1 |> Seq.toList
    let expected = [(1, ""); (2, ""); (3, ""); (4, "")]

    Assert.Equal<(int * string) list>(expected, actual)


[<Fact>]
let ``BFS.ofFGraphWithDepths returns correct result`` () =

    let actual = BFS.ofFGraphWithDepth 1 3 ReferenceObjects.fGraph1 |> Seq.toList
    let expected = [(1, ""); (2, ""); (7, ""); (3, ""); (4, "")]

    Assert.Equal<(int * string) list>(expected, actual)


[<Fact>]
let ``BFS.ofFGrapWithDepthBy returns correct result`` () =

    let actual1 = BFS.ofFGraphWithDepthBy 1 3 (fun _ _ _ -> true) ReferenceObjects.fGraph1 |> Seq.toList
    let actual2 = BFS.ofFGraphWithDepthBy 1 System.Int32.MaxValue (fun nk _ _ -> nk < 5) ReferenceObjects.fGraph1 |> Seq.toList
    let expected1 = [(1, ""); (2, ""); (3, ""); (4, "")]
    let expected2 = [(1, ""); (2, ""); (7, ""); (3, ""); (4, "")]

    Assert.Equal<(int * string) list>(expected1, actual1)
    Assert.Equal<(int * string) list>(expected2, actual2)