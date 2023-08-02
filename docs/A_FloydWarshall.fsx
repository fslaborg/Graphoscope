(**
---
title: Floyd-Warshall
category: Algorithms
categoryindex: 3
index: 2
---
*)


(*** hide ***)

(*** condition: prepare ***)
#r "nuget: FSharpAux.Core, 2.0.0"
#r "nuget: FSharpAux.IO, 2.0.0"
#r "nuget: Cytoscape.NET, 0.2.0"
#r "../src/Graphoscope/bin/Release/netstandard2.0/Graphoscope.dll"

(*** condition: ipynb ***)
#if IPYNB
#r "nuget: Graphoscope, {{fsdocs-package-version}}"
#endif // IPYNB

(**
# Introducing the Floyd-Warshall Algorithm for All-Pairs Shortest Path in Graphs
The Floyd-Warshall algorithm is a widely used algorithm in graph theory and computer science for finding the shortest paths between all pairs of vertices in a weighted graph. It is named after its inventors, Robert Floyd and Stephen Warshall, who independently proposed it in the early 1960s.

The algorithm works on both directed and undirected graphs, where edges have non-negative weights (can be zero or positive but not negative). The goal is to find the shortest path distance between all pairs of vertices in the graph.


Floyd-Warshall and Dijkstra's algorithm are both used to find the shortest paths in a graph, but they serve different purposes and have different use cases. 

They are not direct alternatives to each other; rather, they are used in different scenarios based on the problem requirements and the characteristics of the graph.
*)

open Graphoscope
open Cytoscape.NET

let dwgDiGraph =
    let nodes = [|0;1;2;3;4;5|]
    let edges = [|0,1,7.;0,2,12.;1,2,2.;1,3,9.;2,4,10.;4,3,4.;3,5,1.;4,5,5.|]
    let g = DiGraph.createFromNodes nodes
    DiGraph.addEdges g edges 
    g

let dijDiGraph = Algorithms.FloydWarshall.fromJaggedArray  (DiGraph.toMatrix dwgDiGraph)

let vizDiGraph =
    CyGraph.initEmpty ()
    |> CyGraph.withElements [
            for (sk,tk,el) in (DiGraph.getAllEdges dwgDiGraph) do
                let sk, tk = (string sk), (string tk)
                yield Elements.node sk [ CyParam.label sk ]
                yield Elements.node tk [ CyParam.label tk ]
                yield Elements.edge  (sprintf "%s_%s" sk tk) sk tk [ CyParam.label el ]
        ]
    |> CyGraph.withStyle "node"
        [
            CyParam.content =. CyParam.label
            CyParam.color "#A00975"
        ]
    |> CyGraph.withStyle "edge"
        [
            CyParam.content =. CyParam.label
            CyParam.Curve.style "bezier"
            CyParam.Target.Arrow.shape "triangle"
            CyParam.Source.Arrow.shape "circle"
            CyParam.color "#438AFE"
        ]

(***hide***)
vizDiGraph
|> CyGraph.withZoom(CytoscapeModel.Zoom.Init(ZoomingEnabled=false))
    |> CyGraph.withLayout (
        Layout.initCose (Layout.LayoutOptions.Cose(ComponentSpacing=40))
        )
|> CyGraph.withSize(800, 400)
|> Cytoscape.NET.HTML.toGraphHTML() 
(*** include-it-raw ***)
