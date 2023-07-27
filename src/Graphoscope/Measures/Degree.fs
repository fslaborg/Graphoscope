namespace Graphoscope.Measures

open Graphoscope
open FSharpAux

type Degree() =

    static member inwardDistributio(graph : FGraph<'NodeKey,'NodeData,'EdgeData>) = 42.
    static member inwardDistributio(graph : DiGraph<'NodeKey, 'EdgeData>) = 42.
    

    static member outwardDistribution(graph : FGraph<'NodeKey,'NodeData,'EdgeData>) = 42.
    static member outwardDistribution(graph : DiGraph<'NodeKey, 'EdgeData>) = 42.
    
    /// <summary> 
    /// Returns the degree distribution of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float seq of degree values</returns>
    static member distribution(graph : FGraph<'NodeKey,'NodeData,'EdgeData>) = 
        graph
        |> FGraph.mapContexts FContext.degree
        |> Seq.map (fun (_,d) -> float d)

    /// <summary> 
    /// Returns the degree distribution of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float seq of degree values</returns>
    static member distribution(graph : DiGraph<'NodeKey, 'EdgeData>) = 
        graph.OutEdges 
        |> ResizeArray.map(fun n -> n |> ResizeArray.length |> float)
        |> ResizeArray.toSeq    
    
    /// <summary> 
    /// Get the mean degree of the graph. 
    /// This is an undirected measure so inbound links add to a nodes degree.
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the mean degree</returns>
    static member average(graph : FGraph<'NodeKey,'NodeData,'EdgeData>) = 
        graph
        |> FGraph.mapContexts FContext.degree
        |> Seq.averageBy (fun (_,d) -> float d) 
    
    /// <summary> 
    /// Get the mean degree of the graph. 
    /// This is an undirected measure so inbound links add to a nodes degree.
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the mean degree</returns>
    static member average(graph : DiGraph<'NodeKey, 'EdgeData>) =
        graph.OutEdges
        |> ResizeArray.map(fun n -> (n |> ResizeArray.length) * 2 |> float)
        |> ResizeArray.toArray
        |> Array.average


    //static member compute() = 42.
