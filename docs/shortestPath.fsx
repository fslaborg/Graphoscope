(**
---
title: Graph Algorithms
category: Graphoscope 
categoryindex: 1
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
# Shortest Path 

*)

open Graphoscope

let testGraph_ChatGPT =
    FGraph.empty
    |> FGraph.addElement 1 "Node 1" 2 "Node 2" 1  
    |> FGraph.addElement 1 "Node 1" 3 "Node 3" 1
    |> FGraph.addElement 2 "Node 2" 4 "Node 4" 1  
    |> FGraph.addElement 2 "Node 2" 5 "Node 5" 1
    |> FGraph.addElement 3 "Node 3" 6 "Node 6" 1
    |> FGraph.addElement 5 "Node 5" 7 "Node 7" 1

