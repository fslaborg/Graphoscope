(**
---
title: Import 
category: Graphoscope 
categoryindex: 1
index: 2 
---
*)


(*** hide ***)

(*** condition: prepare ***)
#r "nuget: FSharpAux.Core, 2.0.0"
#r "nuget: FSharpAux.IO, 2.0.0"
#r "nuget: Cytoscape.NET, 0.2.0"
#r "../src/Graphoscope/bin/Release/netstandard2.0/Graphoscope.dll"

(*** condition: ipynb ***)
#if IPYNB
#r "nuget: Graphoscope, {{fsdocs-package-version}}"
#endif // IPYNB

(**
# Import a graph  

This directed network contains observed grooming episodes between free ranging rhesus macaques (Macaca mulatta) 
in Cayo Santiago during a two month period in 1963. Cayo Santiago is an island off the coast of Puerto Rico, also 
known as Isla de los monos (Island of the monkeys). A node represents a monkey and a directed edge A → B denotes 
that the rhesus macaque A groomed rhesus macaque B. The integer edge weights indicate how often this behaviour was observed.


*)

open FSharpAux.IO
open FSharpAux.IO.SchemaReader.Attribute

open Graphoscope

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
(*** include-it-raw ***)

