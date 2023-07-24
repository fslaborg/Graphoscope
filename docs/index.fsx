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
# Graphoscope

The Graphoscope project aims to provide a rigorous and performant tool for Network Science. 
It is aimed at anyone who works with Graphs/Networks and does not require a strong knowledge of F# to get started. 

# Getting Started 

## Prerequisites 

To set up a dev environment, we recommend [VSCode](https://code.visualstudio.com/) with the [Ionide](https://marketplace.visualstudio.com/items?itemName=Ionide.Ionide-fsharp) plugin  
You will also need [DotNet 6](https://dotnet.microsoft.com/en-us/download/dotnet/6.0) installed.

The library is designed primarily for use in an fsharp scripting environment using .fsx files. 
But it also works well in [notebooks](https://marketplace.visualstudio.com/items?itemName=ms-dotnettools.dotnet-interactive-vscode).
This [video](https://www.youtube.com/watch?v=1ROKvmcOloo&list=PLdo4fOcmZ0oUFghYOp89baYFBTGxUkC7Z&index=3) has a good walk through of setting your environment. 

## Importing a graph
The library is organised by Graph type. Currently it supports Directed and Undirected graphs, and other types, such as multiplex, will be supported in future. 

For illustration, we will import a directed graph from The [KONECT Project](http://konect.cc/) website. This is an excellent resource with many graphs in an edge list based format which is simple
to import and analyse using Graphoscope. We will start by importing a [graph](http://konect.cc/networks/moreno_rhesus/) describing the grooming interactions between rhesus monkeys.

Create a monkeys.fsx and run the following code to import and print some basic measures. Further documention of DiGraph functionality is [here](reference/graphoscope-digraph.html)
*)

open Graphoscope.Utility.Import
open Graphoscope.DiGraph.Measures

let file = __SOURCE_DIRECTORY__ + "/data/out.moreno_rhesus_rhesus.txt"
let monkeyGraph = importDirectedGraph file " " 2 false

printf "Successfully imported the graph! It has %i nodes and %i edges. The average degree is %f " 
  (getSize monkeyGraph) (getVolume monkeyGraph) (getMeanDegree monkeyGraph)
(*** include-output ***)

(**
We can also import undirected graphs using the [Graph](reference/graphoscope-graph.html) namespace. These examples use the [Karate club](http://konect.cc/networks/ucidata-zachary/) graph.
*)

open Graphoscope.Graph
open Graphoscope.Graph.Measures

let karateFile= __SOURCE_DIRECTORY__ + "/data/zachary.txt"
let karateGraph = importUnDirectedGraph karateFile " " 2 false
getNodes karateGraph

printf "Successfully imported the undirected karate graph! It has %i nodes and %i edges. The average degree is %f " 
  (getSize karateGraph) (getVolume karateGraph) (getMeanDegree karateGraph)
(*** include-output ***)
(**
## Creating a Graph
We can also build a graph from scratch. Either by passing an array of edges:
*)
[|(0, 1, 1.0); (0, 2, 1.0); (1, 1, 1.0); (1, 3, 1.0); (3, 2, 1.0); (4, 0, 1.0)|]
|> Builders.createFromEdges
(**
or by creating an empty graph and adding the nodes and edges one by one. 
The int and float after the "create" define the type of the nodes and edges.
*)

let emptyGraph = Builders.create<int, float>()
addNode emptyGraph 1
addNode emptyGraph 2
addNode emptyGraph 3
let edge = (1,3, 1.0)
addEdge emptyGraph edge

printf "Manually created a graph with %i nodes" (getSize emptyGraph)
(*** include-output ***)
(**
## Algorithms
A key aim of Graposcope is to provide highly performant graph analysis. 
Currently it contains implementations of Dijkstra, including a parallelised version, and Floyd-Warshall.

We can get the shortest path between two nodes as follows 
*)
getShortestPath 26 16 karateGraph
(**
Alternatively you can return all the shortest paths for every pair of nodes using parrallel Dijkstra as follows 
*)
open Graphoscope.Graph.Algorithms
let paths = Dijkstra.ComputeAllPairs karateGraph
(***include-value:paths***)
(**
Or with Floyd-Warshall
*)
FloydWarshall.Compute karateGraph
(**
Implementations of both algorithms are also available in the Digraph namespace.

These algorthms both operate on an Adjacency Matrix and a function for converting graphs to this data structure is provided in the Converters modules. 
It can be executed as follows. 
*)
open Graphoscope.DiGraph.Converters
let monkeyAdjacencyMatrix = toAdjacencyMatrix monkeyGraph
(***include-value:monkeyAdjacencyMatrix***)

(**
## Charting
Consider using [Plotly.NET](https://plotly.net/) for charting. Built on top of plotly.js, it is a mature library offering a wide range of customisable charts. 
Here is a basic example showing degree distribution within the Karate club. 
*)
#r "nuget: Plotly.NET, 4.1.0"
open Plotly.NET

getDegreeDistribution karateGraph
|> Chart.Histogram
|> GenericChart.toChartHTML
(***include-it-raw***)
(**
# Contributing and copyright

The project is hosted on [GitHub][gh] where you can [report issues][issues], fork 
the project and submit pull requests. If you're adding a new public API, please also 
consider adding [samples][docs] that can be turned into a documentation. You might
also want to read the [library design notes][readme] to understand how it works.

The library is available under the OSI-approved MIT license, which allows modification and 
redistribution for both commercial and non-commercial purposes. For more information see the 
[License file][license] in the GitHub repository. 

  [docs]: https://github.com/fslaborg/Graphoscope/tree/main/docs
  [gh]: https://github.com/fslaborg/Graphoscope
  [issues]: https://github.com/fslaborg/Graphoscope/issues
  [readme]: https://github.com/fslaborg/Graphoscope/blob/main/README.md
  [license]: https://github.com/fslaborg/Graphoscope/blob/main/LICENSE
*)
