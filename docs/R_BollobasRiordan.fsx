(**
---
title: Bollobas-Riordan
category: Random Graph 
categoryindex: 4
index: 3 
---
*)

(*** hide ***)

(*** condition: prepare ***)
#r "nuget: FSharpAux.Core, 2.0.0"
#r "nuget: FSharpx.Collections, 3.1.0"
#r "nuget: FSharpAux.IO, 2.0.0"
#r "nuget: FSharp.Data, 6.2.0"
#r "nuget: Plotly.NET, 4.1.0"
#r "nuget: Plotly.NET.Interactive, 4.1.0"
#r "../src/Graphoscope/bin/Release/netstandard2.0/Graphoscope.dll"

(*** condition: ipynb ***)
#if IPYNB
#r "nuget: Graphoscope, {{fsdocs-package-version}}"
#endif // IPYNB

(**
# Random graph models
## Bollobás-Riordan methode
The Bollobás-Riordan method, proposed by mathematicians Béla Bollobás and Oliver Riordan, is a procedure for generating random graphs with a given degree sequence
It allows researchers to explore and analyze the properties of large-scale networks and complex systems in a stochastic setting.
*)
open Graphoscope
open Graphoscope.RandomModels
let N = 50

let myBollobasRiordan = RandomModels.BollobasRiordan.initDirectedFGraph  N 0.1 0.6 0.3 0.6 0.4 FGraph.empty

(***hide***)
let g = sprintf "You have created a graph with %i nodes and %i edges"(FGraph.countNodes myBollobasRiordan) (FGraph.countEdges myBollobasRiordan)
(*** include-value: g ***)
