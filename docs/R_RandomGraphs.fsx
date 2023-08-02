(**
---
title: Random Graph generation
category: Random Graph 
categoryindex: 4
index: 1 
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
#Random graph models
*)
open Graphoscope
open Graphoscope.RandomModels
(**
## Motivation and overview
In every implementation workflow, there comes the point where you have to test wether everything works as expexted or not.
For this, a matching test set is necessary. In some cases (e.g. List sorting algorithms) creating those test sets is done in a matter of seconds.
In other cases, especially if the data you work with is more than one dimensional, it can get quite tedious.
To this effect, FSharp.FGl comes equipped with implementations of random graph generation models.  
In this tutorial I want to introduce you to the models implemented and how to generate graphs with the given functions:
    
* Gilbert model

* Barabási-Albert model

* Bollobás-Riordan methode 
*)