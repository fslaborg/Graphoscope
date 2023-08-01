(**
---
title: DiGraph
category: Graphoscope 
categoryindex: 2
index: 1 
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
  (DiGraph.countNodes monkeyGraph) (DiGraph.getVolume monkeyGraph) (Measures.Degree.average monkeyGraph)
(*** include-output ***)


