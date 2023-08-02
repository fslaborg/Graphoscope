namespace Graphoscope.Measures
open Graphoscope

type Volume() =

    /// <summary> 
    /// Gets the total number of edges of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the total edges</returns>
    static member volumeOfDiGraph (graph: DiGraph<'NodeKey,'EdgeData>) =
        DiGraph.countEdges graph

    /// <summary> 
    /// Gets the total number of edges of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the total edges</returns>
    static member volumeOfFGraph (graph : FGraph<'NodeKey,'NodeData,'EdgeData>) = 
        FGraph.countEdges graph
    
    /// <summary> 
    /// Gets the total number of edges of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the total edges</returns>
    static member compute (graph : FGraph<'NodeKey,'NodeData,'EdgeData>) = 
        Volume.volumeOfFGraph graph

    /// <summary> 
    /// Gets the total number of edges of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the total edges</returns>
    static member compute (graph :DiGraph<'NodeKey,'EdgeData>) = 
        Volume.volumeOfDiGraph graph
    