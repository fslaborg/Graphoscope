(**
---
title: AdjGraph
category: Graphoscope 
categoryindex: 1
index: 4 
---
*)
(*** hide ***)

(*** condition: prepare ***)
#r "nuget: FSharpAux.Core, 2.0.0"
#r "nuget: FSharpx.Collections, 3.1.0"
#r "nuget: FSharp.Data, 6.2.0"
#r "nuget: FSharpAux.IO, 2.0.0"
#r "nuget: Cytoscape.NET, 0.2.0"
#r "nuget: Plotly.NET, 4.1.0"
#r "nuget: Plotly.NET.Interactive, 4.1.0"
#r "../src/Graphoscope/bin/Release/netstandard2.0/Graphoscope.dll"

(*** condition: ipynb ***)
#if IPYNB
#r "nuget: Graphoscope, {{fsdocs-package-version}}"
#endif // IPYNB

(**
# What is AdjGraph

The AdjGraph is an undirected adaptation of the FGraph.
It combines the predecessors and successors into just one element called neighbours.

[![](https://mermaid.ink/img/pako:eNpdjssOgjAQRX-FzAoT-AEWJgjIjo3urIuxHR7yKCltDCH8u7VijM5qcs7NzF2AS0EQQdnJB69Rae-cssGzE19icc8VjvXVC8O9d_ALG21p3m3e0cRRm-Q08I9KnEqdSlHjD878gpqqvkmjpk1kThz_Hrxp7mei-l6BAHpSPTbCll5ehIGuqScGkV0FqpYBG1abQ6PlaR44RFoZCsCMAjWlDVYKe4hK7CZan7YATRE?type=png)](https://mermaid.live/edit#pako:eNpdjssOgjAQRX-FzAoT-AEWJgjIjo3urIuxHR7yKCltDCH8u7VijM5qcs7NzF2AS0EQQdnJB69Rae-cssGzE19icc8VjvXVC8O9d_ALG21p3m3e0cRRm-Q08I9KnEqdSlHjD878gpqqvkmjpk1kThz_Hrxp7mei-l6BAHpSPTbCll5ehIGuqScGkV0FqpYBG1abQ6PlaR44RFoZCsCMAjWlDVYKe4hK7CZan7YATRE)

# Quickstart 
## Creating an empty graph and filling it with single elements
Begin by creating an empty graph,meaning a data structure with no nodes or edges. 
Then populate the graph with single elements, individual nodes are added one by one, and edges can be introduced to establish connections between them.
*)

open Graphoscope
open AdjGraph

let graphToFillAdjGraph =

    AdjGraph.empty
    |> AdjGraph.addNode 1 "1"
    |> AdjGraph.addNode 2 "2"
    |> AdjGraph.addEdge 1 2 1.


(***hide***)
let graphToFillOutput = sprintf "You have created a graph with %i nodes and %i edges" (AdjGraph.countNodes graphToFillAdjGraph) (AdjGraph.countEdges graphToFillAdjGraph)
(*** include-value: graphToFillOutput ***)

(**    
# Working with Graphs

## Creating a Graph using AdjGraph
### Creating an empty graph and add collections of elements
Another way of creating a graph is by filling it with collections of nodes and edges as seen below:

*)
let graphToFillAdjGraph' =
    
    let nodes = List.init 100 (fun x -> x,$"{x}")

    let edges = List.init 45 (fun x -> x,x*2,1.)

    AdjGraph.empty
    |> AdjGraph.addNodes nodes
    |> AdjGraph.addEdges edges

(***hide***)
let graphToFill2Output = sprintf "You have created a graph with %i nodes and %i edges"(AdjGraph.countNodes graphToFillAdjGraph') (AdjGraph.countEdges graphToFillAdjGraph')
(*** include-value: graphToFill2Output ***)

(**
### Removing Nodes and Edges
To remove Nodes or Edges you can just use the remove functions provided:
*)
let graphWithRemovedElementsAdjGraph =
    graphToFillAdjGraph'
    |> AdjGraph.removeEdge 1 2
    |> AdjGraph.removeNode 0

(***hide***)
let removing = sprintf "You have reduced the graph to %i nodes and %i edges" (AdjGraph.countNodes graphWithRemovedElementsAdjGraph) (AdjGraph.countEdges graphWithRemovedElementsAdjGraph)
(*** include-value: removing ***)

(**
##

# From Data
## Import a graph  

This is the well-known and much-used Zachary karate club network. The data was collected from the members of a university karate club by Wayne Zachary in 1977. 
Each node represents a member of the club, and each edge represents a tie between two members of the club. 
The network is undirected. 
*)

open FSharp.Data

let getElementOfFile (fullpath: string) (delimiter: string) (headerRows: int) (weightsIncluded: bool)  = 
        let rows  = CsvFile.Load(fullpath, delimiter, skipRows = headerRows, hasHeaders = false).Rows
        rows
        |> Seq.map (fun row -> int row[0],int row[0], int row[1],int row[1], if weightsIncluded then float row[2] else 1.0)


let karateFileAdj= __SOURCE_DIRECTORY__ + "/../tests/Graphoscope.Tests/ReferenceGraphs/zachary.txt"

let karateGraphAdj = 
  let g = AdjGraph.empty<int,int,float>
  getElementOfFile karateFileAdj " " 2 false
  |> Seq.iter(fun (s1,s2,t1,t2,w: float) -> AdjGraph.addElement s1 s2 t1 t2 w g|>ignore)
  g


(**
let's use Cytoscape.NET for visualization:
*)

open Cytoscape.NET
let vizGraph =
    CyGraph.initEmpty ()
    |> CyGraph.withElements [
            for (sk,s,tk,t,el) in (AdjGraph.toSeq karateGraphAdj) do
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
    |> CyGraph.withLayout(Cytoscape.NET.Layout.initBreadthfirst(id))

(***hide***)
vizGraph 
|> CyGraph.withZoom(CytoscapeModel.Zoom.Init(ZoomingEnabled=false)) 
|> CyGraph.withSize(800, 400) 
|> HTML.toGraphHTML()
(*** include-it-raw ***)

