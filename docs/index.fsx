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

*)

