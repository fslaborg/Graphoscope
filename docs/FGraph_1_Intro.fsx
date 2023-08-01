(**
---
title: FGraph
category: Graphoscope 
categoryindex: 3
index: 1 
---
*)
(*** hide ***)

(*** condition: prepare ***)
#r "nuget: FSharpAux.Core, 2.0.0"
#r "nuget: FSharpx.Collections, 3.1.0"
#r "nuget: FSharp.Data, 6.2.0"
#r "nuget: FSharpAux.IO, 2.0.0"
#r "nuget: Cytoscape.NET, 0.2.0"
#r "nuget: Plotly.NET, 4.1.0"
#r "nuget: Plotly.NET.Interactive, 4.1.0"
#r "../src/Graphoscope/bin/Release/netstandard2.0/Graphoscope.dll"

(*** condition: ipynb ***)
#if IPYNB
#r "nuget: Graphoscope, {{fsdocs-package-version}}"
#endif // IPYNB

(**
# What is FGraph

The FGraph is an adaptation of a functional graph. 
In programming, a functional graph typically refers to a data structure or representation used to model and analyze the flow of data or the dependencies between functions in a functional programming paradigm. 
Functional programming emphasizes the use of pure functions and avoids mutable state, making functional graphs particularly useful for understanding and optimizing the execution of functional programs.
It is compromised of a Dictionary, containing the nodeIndex as Key and the so called FContext as Value. The structure is visualised here:


[![](https://mermaid.ink/img/pako:eNpNj80KwjAQhF9F9qSgL9CDoI2tIoigJxMPS7Jaf5qUJEWL-O5utaJ7mv1mmGUfoJ0hSOBwdTddoI-9rVC2xzORWe6xKvaj0XjaX3HsQs2g85il_Sx1NtI9djBlKOTakyFNITgf9j9jJtsGgRH_YCY3tdaB_rKCcS4n5oyarG46mrX0I3OW83fZkr52yxZyZo6fAzCEknyJJ8OPPdqIglhQSQoSlgb9RYGyT85hHd2msRqS6GsaQl0ZjCROePRYQnLAa2Baod05992fLxMxYKE?type=png)](https://mermaid.live/edit#pako:eNpNj80KwjAQhF9F9qSgL9CDoI2tIoigJxMPS7Jaf5qUJEWL-O5utaJ7mv1mmGUfoJ0hSOBwdTddoI-9rVC2xzORWe6xKvaj0XjaX3HsQs2g85il_Sx1NtI9djBlKOTakyFNITgf9j9jJtsGgRH_YCY3tdaB_rKCcS4n5oyarG46mrX0I3OW83fZkr52yxZyZo6fAzCEknyJJ8OPPdqIglhQSQoSlgb9RYGyT85hHd2msRqS6GsaQl0ZjCROePRYQnLAa2Baod05992fLxMxYKE)


# Quickstart 
## Creating an empty graph and filling it with single elements
Begin by creating an empty graph,meaning a data structure with no nodes or edges. 
Then populate the graph with single elements, individual nodes are added one by one, and edges can be introduced to establish connections between them.
*)

open Graphoscope
open FGraph

let graphToFill =

    FGraph.empty
    |> FGraph.addNode 1 "1"
    |> FGraph.addNode 2 "2"
    |> FGraph.addEdge 1 2 1.

printfn"You have created a graph with %i nodes and %i edges"(FGraph.countNodes graphToFill) (FGraph.countEdges graphToFill)
(*** include-output ***)

(**    
# Working with Graphs

## Creating a Graph using FGraph
### Creating an empty graph and add collections of elements
Another way of creating a graph is by filling it with collections of nodes and edges as seen below:

*)
let graphToFill' =
    
    let nodes = List.init 100 (fun x -> x,$"{x}")

    let edges = List.init 45 (fun x -> x,x*2,1.)

    FGraph.empty
    |> FGraph.addNodes nodes
    |> FGraph.addEdges edges

printfn"You have created a graph with %i nodes and %i edges"(FGraph.countNodes graphToFill') (FGraph.countEdges graphToFill')
(*** include-output ***)

(**
### Removing Nodes and Edges
To remove Nodes or Edges you can just use the remove functions provided:
*)
let graphWithRemovedElements =
    graphToFill'
    |> FGraph.removeEdge 1 2
    |> FGraph.removeNode 0

printfn"You have reduced the graph to %i nodes and %i edges"(FGraph.countNodes graphWithRemovedElements) (FGraph.countEdges graphWithRemovedElements)
(*** include-output ***)

(**
##

# From Data
## Import a graph  

This directed network contains observed grooming episodes between free ranging rhesus macaques (Macaca mulatta) 
in Cayo Santiago during a two month period in 1963. Cayo Santiago is an island off the coast of Puerto Rico, also 
known as Isla de los monos (Island of the monkeys). A node represents a monkey and a directed edge A → B denotes 
that the rhesus macaque A groomed rhesus macaque B. The integer edge weights indicate how often this behaviour was observed.


*)
open Graphoscope

open FSharpAux.IO
open FSharpAux.IO.SchemaReader.Attribute


(**
First we model the input domain as a reccord type and read a sequence of MonkeyEdges
*)
type MonkeyEdge = {
    [<Field(0)>] Source  : int
    [<Field(1)>] Target  : int
    [<Field(2)>] Groomed : int
}

let monkeyEdges =
    Seq.fromFileWithCsvSchema<MonkeyEdge>("D:/Source/Graphoscope/tests/Graphoscope.Tests/ReferenceGraphs/out.moreno_rhesus_rhesus.txt",' ',false,skipLines=2 )

(**
Convert a MonkeyEdge record to a sequence of graph elements (sourceKey * sourceData * targetKey * targetData * edgeData)
*)
let monkeyGraph =
    monkeyEdges
    |> Seq.map (fun mke ->
        mke.Source, sprintf "Monkey_%i" mke.Source,mke.Target,sprintf "Monkey_%i" mke.Target,float mke.Groomed)
    |> FGraph.ofSeq

(**
let's use Cytoscape.NET for visualization:
*)

open Cytoscape.NET
let vizGraph =
    CyGraph.initEmpty ()
    |> CyGraph.withElements [
            for (sk,s,tk,t,el) in (FGraph.toSeq monkeyGraph) do
                let sk, tk = (string sk), (string tk)
                yield Elements.node sk [ CyParam.label s ]
                yield Elements.node tk [ CyParam.label t ]
                yield Elements.edge  (sprintf "%s_%s" sk tk) sk tk [ CyParam.label el ]
        ]
    |> CyGraph.withStyle "node"     
        [
            CyParam.content =. CyParam.label
            CyParam.color "#A00975"
        ]

(***hide***)
vizGraph 
|> CyGraph.withZoom(CytoscapeModel.Zoom.Init(ZoomingEnabled=false)) 
|> CyGraph.withSize(800, 400) 
|> HTML.toGraphHTML()
(*** include-output ***)

