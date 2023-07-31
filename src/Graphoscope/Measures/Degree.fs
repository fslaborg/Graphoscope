namespace Graphoscope.Measures

open Graphoscope
open FSharpAux

type Degree() =
    /// <summary> 
    /// Returns the in-degree distribution of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float seq of in-degree values</returns>
    static member inwardDistributio(graph : FGraph<'NodeKey,'NodeData,'EdgeData>) = 
        graph
        |> FGraph.mapContexts FContext.inwardDegree
        |> Seq.map (fun (_,d) -> float d)

    static member inwardDistributio(graph : DiGraph<'NodeKey, 'EdgeData>) = 42.

    /// <summary> 
    /// Returns the out-degree distribution of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float seq of out-degree values</returns>
    static member outwardDistribution(graph : FGraph<'NodeKey,'NodeData,'EdgeData>) = 
        graph
        |> FGraph.mapContexts FContext.outwardDegree
        |> Seq.map (fun (_,d) -> float d)

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
    
    /// <summary> 
    /// Get the max degree of the graph. 
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An int of the max degree</returns>
    static member maximum(graph : FGraph<'NodeKey,'NodeData,'EdgeData>) = 
        graph 
        |> FGraph.mapContexts FContext.degree
        |> Seq.maxBy (fun (_,d) -> float d) 
        |> snd 
    