namespace Graphoscope.Measures

open Graphoscope
open FSharpAux

type Degree() =

    static member inward(graph : FGraph<'NodeKey,'NodeData,'EdgeData>) = 42.
    
    static member outward() = 42.
    
    static member degree() = 42.
    
    
    /// <summary> 
    /// Get the mean degree of the graph. 
    /// This is an undirected measure so inbound links add to a nodes degree.
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the mean degree</returns>
    static member mean(graph : FGraph<'NodeKey,'NodeData,'EdgeData>) = 42.
    
    /// <summary> 
    /// Get the mean degree of the graph. 
    /// This is an undirected measure so inbound links add to a nodes degree.
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the mean degree</returns>
    static member mean(graph : DiGraph<'NodeKey, 'EdgeData>) =
        graph.OutEdges
        |> ResizeArray.map(fun n -> (n |> ResizeArray.length) * 2 |> float)
        |> ResizeArray.toArray
        |> Array.average


    static member compute() = 42.
