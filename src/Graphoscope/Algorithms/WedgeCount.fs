namespace Graphoscope.Algorithms

open Graphoscope
open Graphoscope.Graphs

type WedgeCount() =
    static member oFContextMap(graph: Directed.FContextMap<'NodeKey, 'NodeData, 'EdgeData>) =
        graph.Keys
        |> Seq.sumBy (fun key ->
            let node = graph[key]
            let successors = node |> Directed.FContext.successors |> Seq.map fst
            let predecessors = node |> Directed.FContext.predecessors |> Seq.map fst
            // exclude self loops and double edges
            let exclude = Seq.length (successors |> Seq.filter (fun x ->
                let isDoubleEdge = predecessors |> Seq.contains x
                let isSelfLoop = x = key
                isDoubleEdge || isSelfLoop))
            let n = Seq.length successors + Seq.length predecessors - exclude
            let wedges = n * (n - 1) / 2
            wedges)
