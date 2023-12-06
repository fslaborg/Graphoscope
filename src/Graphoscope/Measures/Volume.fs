namespace Graphoscope.Measures
open Graphoscope
open Graphoscope.Graphs

type Volume() =

    /// <summary> 
    /// Gets the total number of edges of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the total edges</returns>
    static member volumeOfUndirected (graph: Undirected.UndirectedGraph<'NodeKey, 'NodeData, 'EdgeData>) =
        Undirected.UndirectedGraph.countEdges graph

    /// <summary> 
    /// Gets the total number of edges of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the total edges</returns>
    static member volumeOfLilMatrix (graph: Directed.LilMatrix<'NodeKey, _,'EdgeData>) =
        Directed.LilMatrix.countEdges graph

    /// <summary> 
    /// Gets the total number of edges of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the total edges</returns>
    static member volumeOfFContextMap (graph : Directed.FContextMap<'NodeKey,'NodeData,'EdgeData>) = 
        Directed.FContextMap.countEdges graph 

    /// <summary> 
    /// Gets the total number of edges of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the total edges</returns>
    static member volumeOfAdjGraph (graph : AdjGraph<'NodeKey,'NodeData,'EdgeData>) = 
        AdjGraph.countEdges graph / 2
       
    /// <summary> 
    /// Gets the total number of edges of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the total edges</returns>
    static member compute (graph : Directed.FContextMap<'NodeKey,'NodeData,'EdgeData>) = 
        Volume.volumeOfFContextMap graph
    
    /// <summary> 
    /// Gets the total number of edges of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the total edges</returns>
    static member compute (graph :Undirected.UndirectedGraph<'NodeKey, 'NodeData, 'EdgeData>) = 
        Volume.volumeOfUndirected graph
    
    /// <summary> 
    /// Gets the total number of edges of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the total edges</returns>
    static member compute (graph : AdjGraph<'NodeKey,'NodeData,'EdgeData>) = 
        Volume.volumeOfAdjGraph graph

    /// <summary> 
    /// Gets the total number of edges of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the total edges</returns>
    static member compute (graph :Directed.LilMatrix<'NodeKey, 'NodeData,'EdgeData>) = 
        Volume.volumeOfLilMatrix graph
    