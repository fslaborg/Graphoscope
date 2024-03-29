(**
---
title: Network Density
category: Measures 
categoryindex: 2
index: 2 
---
*)

(*** hide ***)

(*** condition: prepare ***)
#r "nuget: FSharpAux.Core, 2.0.0"
#r "nuget: FSharpAux.IO, 2.0.0"
#r "nuget: FSharp.Data, 6.2.0"
#r "../src/Graphoscope/bin/Release/netstandard2.0/Graphoscope.dll"

(*** condition: ipynb ***)
#if IPYNB
#r "nuget: Graphoscope, {{fsdocs-package-version}}"
#endif // IPYNB

(**
# Intoduction to Measures using FGraph
Graphoscope provides a comprehensive set of measurement tools designed to analyze, quantify, and interpret the features of graphs. 
These measurements offer valuable insights into the topology, connectivity, and dynamics of your networks. 
Whether you are exploring social connections, optimizing communication pathways, or studying the spread of diseases, our graph measurement functionalities are here to simplify your analysis and decision-making processes.
## Creating a graph by reading a complete graph representation as one.
Step 1 is the loading of our [example graph](http://konect.cc/networks/moreno_rhesus/), sourced from [The KONECT Project](http://konect.cc) describing the grooming interactions between rhesus monkeys.
*)
(***hide***)
open Graphoscope
open FSharpAux.IO
open FSharpAux.IO.SchemaReader.Attribute
open FSharp.Data

let file = __SOURCE_DIRECTORY__ + "/../tests/Graphoscope.Tests/ReferenceGraphs/out.moreno_rhesus_rhesus.txt"

let monkeyGraphDens =
    CsvFile.Load(file, " ", skipRows = 2, hasHeaders = false).Rows
    |> Seq.map (fun row -> 
                int row[0],int row[0], int row[1],int row[1], float row[2])
    |> FGraph.ofSeq

(**

## NetworkDensity
Network density measures the proportion of connections or edges present in a network relative to the total possible number of connections. 
It quantifies the level of interconnectedness between nodes in the network and carries several key implications for the Connectivity of the graph.
*)
let networkDensity = Measures.GraphDensity.compute monkeyGraphDens

(***hide***)
let density = sprintf "The network density of the monkey graph is %f" networkDensity
(*** include-value: density ***)
