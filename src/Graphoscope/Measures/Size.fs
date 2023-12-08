namespace Graphoscope.Measures
open Graphoscope
open Graphoscope.Graphs


type Size() =
    /// <summary> 
    /// Gets the total number of nodes of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the total nodes</returns>
    static member sizeOfUndirected (graph: Undirected.UndirectedGraph<'NodeKey, 'NodeData, 'EdgeData>) =
        Undirected.UndirectedGraph.countNodes graph

    /// <summary> 
    /// Gets the total number of nodes of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the total nodes</returns>
    static member sizeOfLilMatrix (graph: Directed.LilMatrix<'NodeKey, 'NodeData, 'EdgeData>) =
        Directed.LilMatrix.countNodes graph

    /// <summary> 
    /// Gets the total number of nodes of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the total nodes</returns>
    static member sizeOfFContextMap (graph : Directed.FContextMap<'NodeKey,'NodeData,'EdgeData>) = 
        Directed.FContextMap.countNodes graph

    /// <summary> 
    /// Gets the total number of nodes of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the total nodes</returns>
    static member sizeOfUndirectedFContextMap (graph : Undirected.FContextMapU<'NodeKey,'NodeData,'EdgeData>) = 
        Undirected.FContextMapU.countNodes graph

    /// <summary> 
    /// Gets the total number of nodes of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the total nodes</returns>
    static member compute (graph : Directed.FContextMap<'NodeKey,'NodeData,'EdgeData>) = 
        Size.sizeOfFContextMap graph

    /// <summary> 
    /// Gets the total number of nodes of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the total nodes</returns>
    static member compute (graph : Undirected.UndirectedGraph<'NodeKey, 'NodeData, 'EdgeData>) = 
        Size.sizeOfUndirected graph
    
    /// <summary> 
    /// Gets the total number of nodes of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the total nodes</returns>
    static member computeUndirected (graph : Undirected.FContextMapU<'NodeKey,'NodeData,'EdgeData>) = 
        Size.sizeOfUndirectedFContextMap graph
      
    /// <summary> 
    /// Gets the total number of nodes of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the total nodes</returns>
    static member compute (graph :Directed.LilMatrix<'NodeKey, 'NodeData, 'EdgeData>) = 
        Size.sizeOfLilMatrix graph