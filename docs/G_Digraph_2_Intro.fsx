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

let emptyGraph :DiGraph<int, float> = DiGraph.empty
DiGraph.addNode 1 emptyGraph
DiGraph.addNode 2 emptyGraph
DiGraph.addNode 3 emptyGraph
let edge = (1,3, 1.0)
DiGraph.addEdge edge emptyGraph

printf "Manually created a graph with %i nodes" (DiGraph.countNodes emptyGraph)
(*** include-output ***)

(**
# Working with Graphs

# From Data
For illustration, we will import a directed graph from The [KONECT Project](http://konect.cc/) website. This is an excellent resource with many graphs in an edge list based format which is simple
to import and analyse using Graphoscope. We will start by importing a [graph](http://konect.cc/networks/moreno_rhesus/) describing the grooming interactions between rhesus monkeys.

Create a monkeys.fsx and run the following code to import and print some basic measures. Further documention of DiGraph functionality is [here](reference/graphoscope-digraph.html)
*)

let file = __SOURCE_DIRECTORY__ + "/data/out.moreno_rhesus_rhesus.txt"
let monkeyGraph =  Import.importDirectedGraph file " " 2 false

printf "Successfully imported the graph! It has %i nodes and %i edges. The average degree is %f " 
  (DiGraph.countNodes monkeyGraph) (DiGraph.countEdges monkeyGraph) (Measures.Degree.average monkeyGraph)
(*** include-output ***)

(**
We can also import undirected graphs using the [Graph](reference/graphoscope-graph.html) namespace. These examples use the [Karate club](http://konect.cc/networks/ucidata-zachary/) graph.
*)


let karateFile= __SOURCE_DIRECTORY__ + "/data/zachary.txt"
let karateGraph = Import.importUnDirectedGraph karateFile " " 2 false

printf "Successfully imported the undirected karate graph! It has %i nodes and %i edges. The average degree is %f " 
  (DiGraph.countNodes karateGraph) (DiGraph.countEdges karateGraph) (Measures.Degree.average karateGraph)
(*** include-output ***)
(**
## Algorithms
A key aim of Graposcope is to provide highly performant graph analysis. 
Currently it contains implementations of Dijkstra, including a parallelised version, and Floyd-Warshall.

We can get the shortest path between two nodes as follows 
*)
open Graphoscope.Algorithms

Dijkstra.getShortestPath 26 16 karateGraph
(**
Alternatively you can return all the shortest paths for every pair of nodes using parrallel Dijkstra as follows 
*)
let paths = Dijkstra.Compute karateGraph
(***include-value:paths***)
(**
Or with Floyd-Warshall
*)
FloydWarshall.fromJaggedArray (DiGraph.toMatrix karateGraph)
(**

These algorthms both operate on an Adjacency Matrix and a function for converting graphs to this data structure is provided in the Converters modules. 
It can be executed as follows. 
*)
let monkeyAdjacencyMatrix = DiGraph.toMatrix monkeyGraph
(***include-value:monkeyAdjacencyMatrix***)

(**
## Charting
Consider using [Plotly.NET](https://plotly.net/) for charting. Built on top of plotly.js, it is a mature library offering a wide range of customisable charts. 
Here is a basic example showing degree distribution within the Karate club. 
*)
#r "nuget: Plotly.NET, 4.1.0"
open Plotly.NET

Measures.Degree.distribution karateGraph
|> Chart.Histogram
|> GenericChart.toEmbeddedHTML
(***include-it-raw***)
