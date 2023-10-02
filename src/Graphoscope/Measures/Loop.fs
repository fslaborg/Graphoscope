namespace Graphoscope.Measures

open Graphoscope
open FSharpAux

type Loop() =

    /// <summary> 
    /// Get the amount of self loops. 
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An int of loop count</returns>
    static member loopCountFGraph (graph : FGraph<'NodeKey,'NodeData,'EdgeData>) = 
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
    static member loopCountOfDiGraph (graph: DiGraph<'NodeKey, 'NodeData, 'EdgeData>) = 
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
    static member loopCount (graph : FGraph<'NodeKey,'NodeData,'EdgeData>) = 
        Loop.loopCountFGraph graph
    
    /// <summary> 
    /// Get the amount of self loops. 
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An int of the mean degree</returns>
    static member loopCount (graph: DiGraph<'NodeKey, 'NodeData, 'EdgeData>) = 
        Loop.loopCountOfDiGraph graph