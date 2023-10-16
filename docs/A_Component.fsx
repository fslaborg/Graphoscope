(**
---
title: Components
category: Algorithms
categoryindex: 3
index: 4
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
# Graph components
In graph theory, a graph component refers to a subset of vertices in a graph, where each vertex is connected to every other vertex in the subset through a path of edges. 
Let´s open an example graph and show the community detection per color:
*)
open Graphoscope
open Cytoscape.NET

let componentExampleGraph =
    let edgeSeq =
        seq{
            0,1,1
            0,2,1
            0,3,1
            4,5,1
            4,6,1
            4,7,1
            8,9,1
            6,10,1
            8,11,1
        }
        |> Seq.map(fun (s,t,w) ->
            s,s,t,t,(float w)
        )
    AdjGraph.ofSeq edgeSeq

(**
This graph is seperated into 3 distinct components.
Next we use a pre-generated color-pallet and Cytoscape.NET to visualise the graph and its components:
*)

let colors =    
    [
        "#3F51B5"
        "#3F51B5"
        "#3F51B5"
        "#3F51B5"
        "#FFC107"        
        "#FFC107"        
        "#FFC107"        
        "#FFC107"
        "#FFEB3B"
        "#FFEB3B"
        "#FFEB3B"
        "#FFEB3B"
    ]

let renderCyGraph (nodeLabelF) (graph:AdjGraph<'NodeKey,'NodeData,'EdgeData>) =

    CyGraph.initEmpty ()
    |> CyGraph.withElements [
            for (sk,s,tk,t,el) in (AdjGraph.toSeq graph) do
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


renderCyGraph (fun x -> [CyParam.label x;CyParam.color colors.[x]]) componentExampleGraph
(*** include-it-raw ***)


(**
We can use Algorithms.Components to seperate the components from each other, where each components gets its own subgraph:
*)

let components = 
    Algorithms.Components.getGraphComponentsOfAdjGraph componentExampleGraph


renderCyGraph (fun (x) -> [CyParam.label x;CyParam.color colors.[x]]) (components|>Seq.head) 
(*** include-it-raw ***)