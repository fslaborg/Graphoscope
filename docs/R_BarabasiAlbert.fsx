(**
---
title: Barabási-Albert
category: Random Graph 
categoryindex: 4
index: 4 
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
## Barabási-Albert model

The Barabási-Albert (BA) model is a popular network growth model used to generate random scale-free networks.  
It provides a valuable tool for generating synthetic networks that exhibit similar properties to many natural and man-made networks, mainly a scale-free character.
*)

open Graphoscope
open Graphoscope.RandomModels
let N = 50

let edgesPerIteration = 5

// let myBarabasiAlbert = RandomModels.BarabasiAlbert.initFGraph  N edgesPerIteration id id (fun x -> 1.) FGraph.empty

//printfn"You have created a graph with %i nodes and %i edges"(FGraph.countNodes myBarabasiAlbert) (FGraph.countEdges myBarabasiAlbert)

