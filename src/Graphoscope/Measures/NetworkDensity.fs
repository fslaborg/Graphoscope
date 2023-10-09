namespace Graphoscope.Measures

open Graphoscope
open System.Collections.Generic

/// <summary> 
/// Computes the graph density
/// </summary>
type GraphDensity() =
    
    /// <summary> 
    /// Computes the graph density of the given graph <paramref name="graph"/>.
    /// </summary>
    /// <param name="graph">The graph for which to compute the graph density.</param>
    /// <remarks> This calculation only works on graphs without self loops </remarks>
    /// <returns>
    /// The graph density.
    /// </returns> 
    static member ofFGraph (graph :  FGraph<'NodeKey, 'NodeData, float>) =
        let nodesCount  = graph.Count|>float
        let edgeCount   = FGraph.countEdges graph |>float
        let potentialConnections = ((nodesCount) * (nodesCount-1.))
        let density = edgeCount / potentialConnections
        density

    /// <summary> 
    /// Computes the graph density of the given graph <paramref name="graph"/>.
    /// </summary>
    /// <param name="graph">The graph for which to compute the graph density.</param>
    /// <remarks> This calculation only works on graphs without self loops </remarks>
    /// <returns>
    /// The graph density.
    /// </returns> 
    static member ofAdjGraph (graph :  AdjGraph<'NodeKey, 'NodeData, float>) =
        let nodesCount  = graph.Count|>float
        let edgeCount   = AdjGraph.countEdges graph |> float |> fun x -> x*2.
        let potentialConnections = ((nodesCount) * (nodesCount-1.))
        let density = (edgeCount) / potentialConnections
        density

    //TODO
    /// <summary> 
    /// Computes the graph density of the given graph <paramref name="graph"/>.
    /// </summary>
    /// <param name="graph">The graph for which to compute the graph density.</param>
    /// <remarks> This calculation only works on graphs without self loops </remarks>
    /// <returns>
    /// The graph density.
    /// </returns> 
    static member ofDiGraph (graph: DiGraph<'NodeKey, 'NodeData, 'EdgeData>) = 
        System.NotImplementedException() |> raise  

    /// <summary> 
    /// Computes the graph density of the given graph <paramref name="graph"/>.
    /// </summary>
    /// <param name="graph">The graph for which to compute the graph density.</param>
    /// <remarks> This calculation only works on graphs without self loops </remarks>
    /// <returns>
    /// The graph density.
    /// </returns> 
    static member compute (graph :  FGraph<'NodeKey, 'NodeData, float>) =
        GraphDensity.ofFGraph graph

    /// <summary> 
    /// Computes the graph density of the given graph <paramref name="graph"/>.
    /// </summary>
    /// <param name="graph">The graph for which to compute the graph density.</param>
    /// <remarks> This calculation only works on graphs without self loops </remarks>
    /// <returns>
    /// The graph density.
    /// </returns> 
    static member compute (graph :  AdjGraph<'NodeKey, 'NodeData, float>) =
        GraphDensity.ofAdjGraph graph
        
    /// <summary> 
    /// Computes the graph density of the given graph <paramref name="graph"/>.
    /// </summary>
    /// <param name="graph">The graph for which to compute the graph density.</param>
    /// <remarks> This calculation only works on graphs without self loops </remarks>
    /// <returns>
    /// The graph density.
    /// </returns> 
    static member compute (graph: DiGraph<'NodeKey, 'NodeData, 'EdgeData>) = 
        GraphDensity.ofDiGraph graph
    