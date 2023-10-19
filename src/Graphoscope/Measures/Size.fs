namespace Graphoscope.Measures
open Graphoscope


type Size() =
    /// <summary> 
    /// Gets the total number of nodes of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the total nodes</returns>
    static member sizeOfUndirected (graph: UndirectedGraph<'NodeKey, 'NodeData, 'EdgeData>) =
        UndirectedGraph.countNodes graph

    /// <summary> 
    /// Gets the total number of nodes of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the total nodes</returns>
    static member sizeOfDiGraph (graph: DiGraph<'NodeKey, 'NodeData, 'EdgeData>) =
        DiGraph.countNodes graph

    /// <summary> 
    /// Gets the total number of nodes of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the total nodes</returns>
    static member sizeOfFGraph (graph : FGraph<'NodeKey,'NodeData,'EdgeData>) = 
        FGraph.countNodes graph

    /// <summary> 
    /// Gets the total number of nodes of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the total nodes</returns>
    static member sizeOfAdjGraph (graph : AdjGraph<'NodeKey,'NodeData,'EdgeData>) = 
        AdjGraph.countNodes graph

    /// <summary> 
    /// Gets the total number of nodes of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the total nodes</returns>
    static member compute (graph : FGraph<'NodeKey,'NodeData,'EdgeData>) = 
        Size.sizeOfFGraph graph

    /// <summary> 
    /// Gets the total number of nodes of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the total nodes</returns>
    static member compute (graph : UndirectedGraph<'NodeKey, 'NodeData, 'EdgeData>) = 
        Size.sizeOfUndirected graph
    
    /// <summary> 
    /// Gets the total number of nodes of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the total nodes</returns>
    static member compute (graph : AdjGraph<'NodeKey,'NodeData,'EdgeData>) = 
        Size.sizeOfAdjGraph graph
      
    /// <summary> 
    /// Gets the total number of nodes of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the total nodes</returns>
    static member compute (graph :DiGraph<'NodeKey, 'NodeData, 'EdgeData>) = 
        Size.sizeOfDiGraph graph