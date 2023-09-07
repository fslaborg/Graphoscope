namespace Graphoscope.Measures
open Graphoscope

type MatchingIndex() =

    /// <summary> 
    /// Get the matching index between two nodes in a FGraph. 
    /// </summary>
    /// <param name="nk1">The NodeKey of one of the nodes</param> 
    /// <param name="nk2">The NodeKey for the other node</param> 
    /// <param name="graph">The FGraph to be analysed</param> 
    /// <returns>A float of the matching index between two nodes</returns>
    static member betweenFGraphNodes (graph : FGraph<'NodeKey,'NodeData,'EdgeData>) (nk1:'NodeKey) (nk2:'NodeKey) = 

        let neighbours1 = graph.Item nk1 |> FContext.neighbours |> Set.ofSeq
        let neighbours2 = graph.Item nk2 |> FContext.neighbours |> Set.ofSeq
        
        let distinctCommonNeighbours    = Set.intersect neighbours1 neighbours2 |> Set.count |> float
        let totalNumberOfNeighbours     = Set.union neighbours1 neighbours2     |> Set.count |> float

        distinctCommonNeighbours / totalNumberOfNeighbours

    /// <summary> 
    /// Get the matching index between two nodes in a graph. 
    /// </summary>
    /// <param name="nk1">The NodeKey of one of the nodes</param> 
    /// <param name="nk2">The NodeKey for the other node</param> 
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the matching index between two nodes</returns>
    static member betweenNodes((graph : FGraph<'NodeKey,'NodeData,'EdgeData>),(nk1:'NodeKey),(nk2:'NodeKey)) =
        MatchingIndex.betweenFGraphNodes graph nk1 nk2
