(**
---
title: DiGraph
category: Graphoscope 
categoryindex: 1
index: 2 
---
*)
(*** hide ***)

(*** condition: prepare ***)
#r "nuget: FSharpAux.Core, 2.0.0"
#r "nuget: FSharpx.Collections, 3.1.0"
#r "nuget: FSharp.Data, 6.2.0"
#r "nuget: Plotly.NET, 4.1.0"
Plotly.NET.Defaults.DefaultDisplayOptions <-
    Plotly.NET.DisplayOptions.init (PlotlyJSReference = Plotly.NET.PlotlyJSReference.NoReference)
#r "../src/Graphoscope/bin/Release/netstandard2.0/Graphoscope.dll"

(*** condition: ipynb ***)
#if IPYNB
#r "nuget: Graphoscope, {{fsdocs-package-version}}"
#endif // IPYNB

(**
# Datastructure 
The DiGraph is a graph representation that uses a fast node-index-based lookup of edge data, utilizing ResizeArrays for efficient storage and retrieval of information.
The structure is visualised here:

[![](https://mermaid.ink/img/pako:eNpdkM8KwjAMxl9FclLQF9hBUDt1iF70ZLtDWKMrunbUDhHx3U39Q8Wevv6-JB_JHSqnCTI4nN21qtGH3k4o2-M3kcIsPLZ1ORqNp32j19gOPhYTIV0Xcn2kS5lgLo39ZVNmM7nhiBXdPkwwK94yTzLSuYy9AgOWyV_8w5iz_M78Bi1jUMosXn4hog1DaMg3aDSveY81CkJNDSnIWGr0JwXKPrgOu-C2N1tBFnxHQ-hajYGEwaPHBrIDni9MW7R759KftAnOr993fJ3z8QQLO2iF?type=png)](https://mermaid.live/edit#pako:eNpdkM8KwjAMxl9FclLQF9hBUDt1iF70ZLtDWKMrunbUDhHx3U39Q8Wevv6-JB_JHSqnCTI4nN21qtGH3k4o2-M3kcIsPLZ1ORqNp32j19gOPhYTIV0Xcn2kS5lgLo39ZVNmM7nhiBXdPkwwK94yTzLSuYy9AgOWyV_8w5iz_M78Bi1jUMosXn4hog1DaMg3aDSveY81CkJNDSnIWGr0JwXKPrgOu-C2N1tBFnxHQ-hajYGEwaPHBrIDni9MW7R759KftAnOr993fJ3z8QQLO2iF)

# Quickstart 

We can build a graph from scratch. Either by passing an array of edges:
*)
open Graphoscope
open DiGraph

[|(0, 1, 1.0); (0, 2, 1.0); (1, 1, 1.0); (1, 3, 1.0); (3, 2, 1.0); (4, 0, 1.0)|]
|> DiGraph.createFromEdges

(**
or by creating an empty graph and adding the nodes and edges one by one. 
The int and float after the "create" define the type of the nodes and edges.
*)

let emptyGraph :DiGraph<int, int, float> = DiGraph.empty

let edge = (1,3, 1.0)

let filledGraph =
  emptyGraph
  |> DiGraph.addNode 1 1
  |> DiGraph.addNode 2 2
  |> DiGraph.addNode 3 3
  |> DiGraph.addEdge edge

(***hide***)
let filledGraphNodes = filledGraph |> DiGraph.countNodes|> sprintf "Manually created a graph with %i nodes"
(*** include-value: filledGraphNodes ***)

(**
# Working with Graphs

# From Data
For illustration, we will import a directed graph from The [KONECT Project](http://konect.cc/) website. This is an excellent resource with many graphs in an edge list based format which is simple
to import and analyse using Graphoscope. We will start by importing a [graph](http://konect.cc/networks/moreno_rhesus/) describing the grooming interactions between rhesus monkeys.

Create a monkeys.fsx and run the following code to import and print some basic measures. Further documention of DiGraph functionality is [here](reference/graphoscope-digraph.html)
*)

open FSharp.Data

let getElementOfFile (fullpath: string) (delimiter: string) (headerRows: int) (weightsIncluded: bool)  = 
        let rows  = CsvFile.Load(fullpath, delimiter, skipRows = headerRows, hasHeaders = false).Rows
        rows
        |> Seq.map (fun row -> int row[0],int row[0], int row[1],int row[1], if weightsIncluded then float row[2] else 1.0)


let file = __SOURCE_DIRECTORY__ + "/../tests/Graphoscope.Tests/ReferenceGraphs/out.moreno_rhesus_rhesus.txt"
let monkeyGraph = 
  
  CsvFile.Load(file, " ", skipRows = 2, hasHeaders = false).Rows
  |> Seq.map (fun row -> 
              int row[0],int row[0], int row[1],int row[1], float row[2])
  |> DiGraph.ofSeq



(***hide***)
let outputMonkeyGraph = sprintf "Successfully imported the graph! It has %i nodes and %i edges. The average degree is %f " (DiGraph.countNodes monkeyGraph) (DiGraph.countEdges monkeyGraph) (Measures.Degree.average monkeyGraph)
(*** include-value: outputMonkeyGraph ***)


(**
We can also import undirected graphs using the [Graph](reference/graphoscope-graph.html) namespace. These examples use the [Karate club](http://konect.cc/networks/ucidata-zachary/) graph.
*)

let karateFile= __SOURCE_DIRECTORY__ + "/../tests/Graphoscope.Tests/ReferenceGraphs/zachary.txt"
let karateGraph = 
  let g = DiGraph.empty<int,int,float>
  getElementOfFile karateFile " " 2 false
  |> Seq.iter(fun (s1,s2,t1,t2,w: float) -> DiGraph.addElement s1 s2 t1 t2 w g|>ignore)
  g

(***hide***)
let karateOutput = sprintf "Successfully imported the undirected karate graph! It has %i nodes and %i edges. The average degree is %f " (DiGraph.countNodes karateGraph) (DiGraph.countEdges karateGraph) (Measures.Degree.average karateGraph)
(*** include-value: karateOutput ***)

(**
A conversion into an Adjacency Matrix is also very easily achievable. It can be executed as follows. 
*)
let monkeyAdjacencyMatrix = DiGraph.toAdjacencyMatrix id monkeyGraph
(***include-value: monkeyAdjacencyMatrix***)

(**
## Charting
Consider using [Plotly.NET](https://plotly.net/) for charting. Built on top of plotly.js, it is a mature library offering a wide range of customisable charts. 
Here is a basic example showing degree distribution within the Karate club. 
*)
#r "nuget: Plotly.NET, 4.1.0"
open Plotly.NET

Measures.Degree.sequence karateGraph
|> Chart.Histogram
|> GenericChart.toChartHTML
(***include-it-raw***)
