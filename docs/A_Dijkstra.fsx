(**
---
title: Dijkstra
category: Algorithms
categoryindex: 3
index: 1
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
# Shortest path between all the vertices using Dijkstra�a Algorithm on an FGraph.

Dijkstra�s algorithm, given by a brilliant Dutch computer scientist and software engineer Dr. Edsger Dijkstra in 1959.
Dijkstra�s algorithm is a greedy algorithm that solves the single-source shortest path problem for a directed and undirected
graph that has non-negative edge weight.

Let's start with a directed weighted graph. We will find shortest path between all the vertices using Dijkstra�a Algorithm.

*)

open Graphoscope

let dwg =
    let nodes = [|0,"A";1,"B";2,"C";3,"D";4,"E";5,"F"|]
    let edges = [|0,1,7.;0,2,12.;1,2,2.;1,3,9.;2,4,10.;4,3,4.;3,5,1.;4,5,5.|]
    FGraph.create(nodes,edges)

let dij = Algorithms.Dijkstra.compute(0,dwg)


open Cytoscape.NET

let vizGraph =
    CyGraph.initEmpty ()
    |> CyGraph.withElements [
            for (sk,s,tk,t,el) in (FGraph.toSeq dwg) do
                let sk, tk = (string sk), (string tk)
                yield Elements.node sk [ CyParam.label s ]
                yield Elements.node tk [ CyParam.label t ]
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
vizGraph
|> CyGraph.withZoom(CytoscapeModel.Zoom.Init(ZoomingEnabled=false))
    |> CyGraph.withLayout (
        Layout.initCose (Layout.LayoutOptions.Cose(ComponentSpacing=40))
        )
|> CyGraph.withSize(800, 400)
|> Cytoscape.NET.HTML.toGraphHTML() 
(*** include-it-raw ***)

(**
# Shortest path between all the vertices using Dijkstra�a Algorithm on DiGraph.
Computation of the shortest paths is also available using the DiGraph structure.
Lets compare them using the same graph as above:
*)


let dwgDiGraph =
    let nodes = [|0;1;2;3;4;5|]
    let edges = [|0,1,7.;0,2,12.;1,2,2.;1,3,9.;2,4,10.;4,3,4.;3,5,1.;4,5,5.|]
    let g = DiGraph.createFromNodes nodes
    DiGraph.addEdges g edges 
    g

let dijDiGraph = Algorithms.Dijkstra.compute(0,dwgDiGraph)

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
