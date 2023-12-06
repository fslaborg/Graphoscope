(**
---
title: Centrality
category: Measures 
categoryindex: 2
index: 4 
---
*)

(*** hide ***)

(*** condition: prepare ***)
#r "nuget: FSharpAux.Core, 2.0.0"
#r "nuget: FSharpAux.IO, 2.0.0"
#r "nuget: FSharp.Data, 6.2.0"
#r "nuget: Cytoscape.NET, 0.2.0"
#r "nuget: OptimizedPriorityQueue, 5.1.0"
#r "../src/Graphoscope/bin/Release/netstandard2.0/Graphoscope.dll"

(*** condition: ipynb ***)
#if IPYNB
#r "nuget: Graphoscope, {{fsdocs-package-version}}"
#endif // IPYNB

(**
# Intoduction to GraphCentrality using FGraph
Graph centrality is a concept used in network analysis to identify and measure the relative importance or significance of nodes within a graph. 
It helps us understand which nodes play a central role in the network, indicating their influence, importance, or prominence. 
Centrality measures can be applied to various types of networks, including social networks, transportation networks, communication networks, and more.
## Creating a graph by reading a complete graph representation as one.
Step 1 is the creation of a graph example where we can visualise the graph centrality.
We will visualise the graph via Cytopscape
*)
open Graphoscope
open Cytoscape.NET
open FSharpAux

let centralityEdges =
    seq{
        1,1,2,2,1.
        1,1,4,4,1.
        1,1,5,5,1.
        1,1,6,6,1.
        2,2,3,3,1.
    }

let centralityGraph = AdjGraph.ofSeq centralityEdges

let renderCyGraph (nodeLabelF:int -> CyParam.CyStyleParam ) =

    CyGraph.initEmpty ()
    |> CyGraph.withElements [
            for (sk,s,tk,t,el) in (AdjGraph.toSeq centralityGraph) do
                let sk, tk = (string sk), (string tk)
                yield Elements.node sk [ nodeLabelF s ]
                yield Elements.node tk [ nodeLabelF t ]
                yield Elements.edge  (sprintf "%s_%s" sk tk) sk tk [ ]
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
            CyParam.color "#438AFE"
        ]
    |> CyGraph.withLayout  (
        Layout.initBreadthfirst(Layout.LayoutOptions.Generic())
            )  
    |> CyGraph.withZoom(CytoscapeModel.Zoom.Init(ZoomingEnabled=false))
    |> CyGraph.withSize(800, 400)
    |> Cytoscape.NET.HTML.toGraphHTML() 

renderCyGraph (fun x -> CyParam.label $"Node: {x}")
(*** include-it-raw ***)
(**
## Closeness Centrality
Closeness centrality assesses how quickly a node can reach all other nodes in the network. 
Nodes with high closeness centrality are considered central because they are close to many other nodes in terms of geodesic distance (the shortest path).
*)

let closenessCentrality =
    Measures.ClosenessCentrality.computeWithEdgeData centralityGraph


renderCyGraph (fun x -> CyParam.label ($"Node: {x};Closeness: {((closenessCentrality.Item x)|>Math.round 3)}"))
(*** include-it-raw ***)

(**
## Betweenness Centrality
Betweenness centrality measures how often a node lies on the shortest path between pairs of other nodes. 
Nodes with high betweenness centrality act as bridges or intermediaries in the network.
*)

let betweenness = 
    Measures.BetweennessCentrality.computeWithEdgeData centralityGraph

renderCyGraph (fun x -> CyParam.label ($"Node: {x};Betweenness: {betweenness.Item x}"))
(*** include-it-raw ***)

(**
## Node Eccentricity
Node eccentricity is a concept used in graph theory and network analysis to measure the centrality or importance of a node within a graph. 
It quantifies how far a node is from the farthest other node in the network in terms of the shortest path length. 
In other words, it represents the maximum distance between a node and any other node in the graph.
*)

let eccentricity (node:int) =
    Measures.Eccentricity.computeOfNodeWithEdgeData(centralityGraph,node)

renderCyGraph (fun x -> CyParam.label ($"Node: {x};Eccentricity: {eccentricity x}"))
(*** include-it-raw ***)


(**
## Distances
Another important metric to take into account involves statistics related to all the shortest paths within a graph. 
These statistics encompass the Diameter (which represents the longest among the shortest paths), the Radius (representing the shortest of the shortest paths), and the average path length.
As these metrics rely on information from all the shortest paths within a graph, they are fundamentally derived from the results of the Floyd-Warshall algorithm for calculating shortest paths.
Therefore it is wise to calculate this once and reuse it for the calulations.
*)


let diameter =
    Measures.Diameter.ofAdjGraph id centralityGraph

let radius =
    Measures.Radius.ofAdjGraph id  centralityGraph

(***hide***)
$"The given graph has a diameter of {diameter} and a radius of {radius}."
(*** include-it***)
