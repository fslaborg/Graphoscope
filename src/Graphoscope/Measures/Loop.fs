namespace Graphoscope.Measures

open Graphoscope
open Graphoscope.Graphs
open FSharpAux

type Loop() =

    /// <summary> 
    /// Get the amount of self loops. 
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An int of loop count</returns>
    static member loopCountFContextMap (graph : Directed.FContextMap<'NodeKey,'NodeData,'EdgeData>) = 
        [|
            for values in graph do
                values.Key,
                values.Value|>fun (p,n,s) -> p
        |]
        |> Array.sumBy(fun (nk,v) -> 
            v.Keys|>Seq.countIf (fun x -> x=nk)
        )

    /// <summary> 
    /// Get the amount of self loops. 
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An int of loop count</returns>
    static member loopCountAdjGraph (graph : AdjGraph<'NodeKey,'NodeData,'EdgeData>) = 
        [|
            for values in graph do
                values.Key,
                values.Value|>fun (n,s) -> s
        |]
        |> Array.sumBy(fun (nk,v) -> 
            v.Keys|>Seq.countIf (fun x -> x=nk)
        )
        
    /// <summary> 
    /// Get the amount of self loops. 
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An int of loop count</returns>
    static member loopCountOfLilMatrix (graph: Directed.LilMatrix<'NodeKey, 'NodeData, 'EdgeData>) = 
        graph.InEdges
        |> ResizeArray.mapi(fun i x ->
            match x |> ResizeArray.tryFind(fun (t,_) -> t = i) with
            | Some _ -> 1
            | None -> 0
        )
        |> Seq.sum

    /// <summary> 
    /// Get the amount of self loops. 
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An int of the mean degree</returns>
    static member loopCount (graph : Directed.FContextMap<'NodeKey,'NodeData,'EdgeData>) = 
        Loop.loopCountFContextMap graph

    /// <summary> 
    /// Get the amount of self loops. 
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An int of the mean degree</returns>
    static member loopCount (graph : AdjGraph<'NodeKey,'NodeData,'EdgeData>) = 
        Loop.loopCountAdjGraph graph
        
    /// <summary> 
    /// Get the amount of self loops. 
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An int of the mean degree</returns>
    static member loopCount (graph: Directed.LilMatrix<'NodeKey, 'NodeData, 'EdgeData>) = 
        Loop.loopCountOfLilMatrix graph