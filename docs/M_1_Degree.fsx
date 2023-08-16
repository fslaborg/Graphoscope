(**
---
title: Degree
category: Measures 
categoryindex: 2
index: 1 
---
*)

(*** hide ***)

(*** condition: prepare ***)
#r "nuget: FSharpAux.Core, 2.0.0"
#r "nuget: FSharpAux.IO, 2.0.0"
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
# Intoduction to Measures using FGraph
Graphoscope provides a comprehensive set of measurement tools designed to analyze, quantify, and interpret the features of graphs. 
These measurements offer valuable insights into the topology, connectivity, and dynamics of your networks. 
Whether you are exploring social connections, optimizing communication pathways, or studying the spread of diseases, our graph measurement functionalities are here to simplify your analysis and decision-making processes.
## Reading a complete graph representation
Step 1 is the loading of our [example graph](http://konect.cc/networks/moreno_rhesus/), sourced from [The KONECT Project](http://konect.cc) describing the grooming interactions between rhesus monkeys.
*)
open Graphoscope
open Plotly.NET
open FSharpAux.IO
open FSharp.Data
open FSharpAux.IO.SchemaReader.Attribute

(***hide***)
let file = __SOURCE_DIRECTORY__ + "/../tests/Graphoscope.Tests/ReferenceGraphs/out.moreno_rhesus_rhesus.txt"

let monkeyGraphDeg =
    CsvFile.Load(file, " ", skipRows = 2, hasHeaders = false).Rows
    |> Seq.map (fun row -> 
                int row[0],int row[0], int row[1],int row[1], float row[2])
    |> FGraph.ofSeq

let monkeyGraph2Deg = 
  
  CsvFile.Load(file, " ", skipRows = 2, hasHeaders = false).Rows
  |> Seq.map (fun row -> 
              int row[0],int row[0], int row[1],int row[1], float row[2])
  |> DiGraph.ofSeq

(**
## Degree
In graph science, a degree is a fundamental concept that plays a crucial role in understanding the structure and properties of graphs. 
The degree of a node in a graph is defined as the number of edges incident to that node, i.e., the number of connections that node has with other nodes in the graph. 
The degree is a basic measure that provides valuable information about the topology and connectivity of the graph.

### Average Degree
The average degree (also known as the average node degree or average connectivity) of a graph is a measure that indicates, on average, how many connections each node has in the network.
*)

let averageDegreeMokeyGraph     = Measures.Degree.average monkeyGraphDeg

let averageDegreeMokeyGraph2    = Measures.Degree.average monkeyGraph2Deg


(***hide***)
let avD = sprintf "The average degree is %f for FGraph and %f fOr DiGraph" (averageDegreeMokeyGraph) (averageDegreeMokeyGraph2)
(*** include-value: avD ***)

(**
### Max Degree
The maximum degree of a graph provides insights into the importance of highly connected nodes (hubs) within the network. 
Understanding hubs is crucial for analyzing network resilience, efficiency, and vulnerability
*)

let maxDregree = Measures.Degree.maximum monkeyGraphDeg

(***hide***)
let maxD = sprintf "The maximal degree is %i" (maxDregree)
(*** include-value: maxD ***)

(**
### Degree Distribution
Degree distribution is an important concept in graph theory and network science that describes the statistical pattern of node degrees in a graph. 
It provides valuable insights into the connectivity and structure of networks and plays a crucial role in understanding various aspects of complex systems.
*)

Measures.Degree.sequence monkeyGraphDeg
|> Chart.Histogram
|> GenericChart.toChartHTML
(***include-it-raw***)
