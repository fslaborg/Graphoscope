(**
---
title: Gilbert
category: Random Graph 
categoryindex: 4
index: 2 
---
*)

(*** hide ***)

(*** condition: prepare ***)
#r "nuget: FSharpAux.Core, 2.0.0"
#r "nuget: FSharpx.Collections, 3.1.0"
#r "nuget: FSharpAux.IO, 2.0.0"
#r "nuget: FSharp.Data, 6.2.0"
#r "../src/Graphoscope/bin/Release/netstandard2.0/Graphoscope.dll"

(*** condition: ipynb ***)
#if IPYNB
#r "nuget: Graphoscope, {{fsdocs-package-version}}"
#endif // IPYNB

(**
# Random graph models
## Gilbert model

The gilbert model (or G(N,p) model) was introduced by Edgar Gilbert in 1959. In this model, you assign a fixed amount of vertices N and a probability p.
p denotes the probality, that edge between two vertices exists or not.  
*)
open Graphoscope
open Graphoscope.RandomModels
let N = 50
let p = 0.5

let myGilbertGraph = Gilbert.initDirectedFGraph N p

printfn"You have created a graph with %i nodes and %i edges"(FGraph.countNodes myGilbertGraph) (FGraph.countEdges myGilbertGraph)
(*** include-output ***)
