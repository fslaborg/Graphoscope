namespace Graphoscope.Algorithms

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
        let edgeCount   = FGraph.Edge.count graph |>float
        let potentialConnections = ((nodeCount) * (nodeCount-1.)) / 2.
        let density = edgeCount / potentialConnections
        density
        