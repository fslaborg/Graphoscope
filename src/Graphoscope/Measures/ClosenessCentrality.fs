namespace Graphoscope.Measures
open Graphoscope
open System.Collections.Generic


type ClosenessCentrality() =
    /// <summary> 
    /// Get the ClosenessCentrality of a node in a FGraph
    /// </summary>
    /// <param name="dijkstraF">Function to calculate the shortest Path via Dijksta Algorithm</param> 
    /// <param name="getEdgeWeightF">Function to get the edgeweight out of the 'EdgeData</param>     
    /// <param name="graph">The graph to be analysed</param>     
    /// <param name="nodeKey">The NodeKey to get the Eccentricity of</param> 
    /// <returns>A float of the ClosenessCentrality of the given node</returns>
    static member ofFGraphNode (dijkstraF:'NodeKey -> ('EdgeData -> float) ->  FGraph<'NodeKey,'NodeData,'EdgeData> -> Dictionary<'NodeKey,float>) (getEdgeWeightF:'EdgeData -> float) (graph :  FGraph<'NodeKey, 'NodeData, 'EdgeData>) (nodeKey:'NodeKey) =    
        let dic = dijkstraF nodeKey getEdgeWeightF graph
        let shortestPathSum = 
            seq {
                    for nv in dic do
                        nv.Value
            }
            |>Seq.sum
        1. / shortestPathSum

    /// <summary> 
    /// Get the normalised ClosenessCentrality of a node in a FGraph
    /// </summary>
    /// <param name="dijkstraF">Function to calculate the shortest Path via Dijksta Algorithm</param> 
    /// <param name="getEdgeWeightF">Function to get the edgeweight out of the 'EdgeData</param>     
    /// <param name="graph">The graph to be analysed</param>     
    /// <param name="nodeKey">The NodeKey to get the Eccentricity of</param> 
    /// <returns>A float of the ClosenessCentrality of the given node</returns>
    static member ofFGraphNodeNormalised (dijkstraF:'NodeKey -> ('EdgeData -> float) ->  FGraph<'NodeKey,'NodeData,'EdgeData> -> Dictionary<'NodeKey,float>) (getEdgeWeightF:'EdgeData -> float) (graph :  FGraph<'NodeKey, 'NodeData, 'EdgeData>) (nodeKey:'NodeKey) =    
        let nodeCount = Measures.Size.compute graph |> float
        let dic = dijkstraF nodeKey getEdgeWeightF graph
        let shortestPathSum = 
            seq {
                    for nv in dic do
                        nv.Value
            }
            |>Seq.sum
        (nodeCount-1.) / shortestPathSum

    /// <summary> 
    /// Get the ClosenessCentrality of all nodes in a FGraph
    /// </summary>
    /// <param name="dijkstraF">Function to calculate the shortest Path via Dijksta Algorithm</param> 
    /// <param name="getEdgeWeightF">Function to get the edgeweight out of the 'EdgeData</param>     
    /// <param name="graph">The graph to be analysed</param>     
    /// <returns>A float of the ClosenessCentrality of all nodes in the given graph</returns>
    static member ofFGraph (dijkstraF:'NodeKey -> ('EdgeData -> float) ->  FGraph<'NodeKey,'NodeData,'EdgeData> -> Dictionary<'NodeKey,float>) (getEdgeWeightF:'EdgeData -> float) (graph :  FGraph<'NodeKey, 'NodeData, 'EdgeData>) =    
        let dict = new Dictionary<'NodeKey,float>()
        for i in graph.Keys do
            let closeness = ClosenessCentrality.ofFGraphNode dijkstraF getEdgeWeightF graph i
            dict.Add(i,closeness)
        dict

    /// <summary> 
    /// Get the normalised ClosenessCentrality of all nodes in a FGraph
    /// </summary>
    /// <param name="dijkstraF">Function to calculate the shortest Path via Dijksta Algorithm</param> 
    /// <param name="getEdgeWeightF">Function to get the edgeweight out of the 'EdgeData</param>     
    /// <param name="graph">The graph to be analysed</param>     
    /// <returns>A float of the ClosenessCentrality of all nodes in the given graph</returns>
    static member ofFGraphNormalised (dijkstraF:'NodeKey -> ('EdgeData -> float) ->  FGraph<'NodeKey,'NodeData,'EdgeData> -> Dictionary<'NodeKey,float>) (getEdgeWeightF:'EdgeData -> float) (graph :  FGraph<'NodeKey, 'NodeData, 'EdgeData>) =    
        let dict = new Dictionary<'NodeKey,float>()
        for i in graph.Keys do
            let closeness = ClosenessCentrality.ofFGraphNodeNormalised dijkstraF getEdgeWeightF graph i
            dict.Add(i,closeness)
        dict
