(**
---
title: Louvain-Algorithm
category: Algorithms
categoryindex: 3
index: 3
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
# The Louvain-Algorithm for Community Detection and Modularity Optimization
The Louvain algorithm is a popular and efficient method for community detection and modularity optimization in complex networks. 
Community detection is the task of partitioning a network into groups of nodes, known as communities or clusters, where nodes within a community are densely connected to each other while having fewer connections to nodes in other communities. 
Modularity is a measure used to quantify the quality of a given network partition.
Let´s open an example graph and show the community detection per color:
*)
open Graphoscope
open Cytoscape.NET

let louvainExampleGraph =
    let edgeSeq =
        seq{
            0,1,1
            0,3,1
            0,4,1
            1,2,1
            1,4,1
            2,3,1
            3,4,1
            2,5,1
            5,6,1
            5,7,1
            5,8,1
            6,7,1
            7,8,1
        }
        |> Seq.map(fun (s,t,w) ->
            s,s,t,t,(float w)
        )
    FGraph.ofSeq edgeSeq
(**
This graph is an adaptation of an example graph in the [networksciencebook](http://networksciencebook.com/chapter/9#modularity). 
Next we use a pre-generated color-pallet and Cytoscape.NET to visualise the graph and its communites:
*)

let colors =    
    [
        "#3F51B5"
        "#FFC107"
        "#F44336"
        "##E91E63"
        "#2196F3"
        "#00BCD4"
        "#C8E6C9"
        "#9C27B0"
        "#FFEB3B"

    ]

let renderCyGraph (nodeLabelF) (graph:FGraph<'NodeKey,'NodeData,'EdgeData>) =

    CyGraph.initEmpty ()
    |> CyGraph.withElements [
            for (sk,s,tk,t,el) in (FGraph.toSeq graph) do
                let sk, tk = (string sk), (string tk)
                yield Elements.node sk (nodeLabelF s )
                yield Elements.node tk (nodeLabelF t )
                yield Elements.edge  (sprintf "%s_%s" sk tk) sk tk [ ]
        ]
    |> CyGraph.withStyle "node"
        [
            CyParam.color "black"
            CyParam.label =. CyParam.label
            CyParam.Text.Align.center
            CyParam.Text.Outline.width 0.5
            CyParam.Background.color   =. CyParam.color
            CyParam.weight 100
        ]

    |> CyGraph.withLayout  (
        Layout.initBreadthfirst(Layout.LayoutOptions.Generic())
            )  
    |> CyGraph.withZoom(CytoscapeModel.Zoom.Init(ZoomingEnabled=false))
    |> CyGraph.withSize(800, 400)
    |> Cytoscape.NET.HTML.toGraphHTML() 


renderCyGraph (fun x -> [CyParam.label x;CyParam.color colors.[x]]) louvainExampleGraph
(*** include-it-raw ***)


(**
Now lets apply our Louvain Algorithm and color the nodes in accordance to their optimised community:
*)


let louvainGraph =
    Algorithms.Louvain.louvain 0.0001 louvainExampleGraph


renderCyGraph (fun (a,x) -> [CyParam.label x;CyParam.color colors.[x]]) louvainGraph
(*** include-it-raw ***)