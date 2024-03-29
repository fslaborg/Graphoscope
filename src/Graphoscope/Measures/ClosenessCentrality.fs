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
    static member ofFGraphNode (getEdgeWeightF:'EdgeData -> float) (graph :  FGraph<'NodeKey, 'NodeData, 'EdgeData>) (nodeKey:'NodeKey) =    
        let dic = Algorithms.Dijkstra.ofFGraph nodeKey getEdgeWeightF graph
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
    /// <param name="getEdgeWeightF">Function to get the edgeweight out of the 'EdgeData</param>     
    /// <param name="graph">The graph to be analysed</param>     
    /// <param name="nodeKey">The NodeKey to get the Eccentricity of</param> 
    /// <returns>A float of the ClosenessCentrality of the given node</returns>
    static member ofFGraphNodeNormalised  (getEdgeWeightF:'EdgeData -> float) (graph :  FGraph<'NodeKey, 'NodeData, 'EdgeData>) (nodeKey:'NodeKey) =    
        let nodeCount = Measures.Size.compute graph |> float
        let dic = Algorithms.Dijkstra.ofFGraph nodeKey getEdgeWeightF graph
        let shortestPathSum = 
            seq {
                    for nv in dic do
                        nv.Value
            }
            |>Seq.sum
        (nodeCount-1.) / shortestPathSum

    /// <summary> 
    /// Get the ClosenessCentrality of a node in an AdjGraph
    /// </summary>
    /// <param name="getEdgeWeightF">Function to get the edgeweight out of the 'EdgeData</param>     
    /// <param name="graph">The graph to be analysed</param>     
    /// <param name="nodeKey">The NodeKey to get the Eccentricity of</param> 
    /// <returns>A float of the ClosenessCentrality of the given node</returns>
    static member ofAdjGraphNode (getEdgeWeightF:'EdgeData -> float) (graph :  AdjGraph<'NodeKey, 'NodeData, 'EdgeData>) (nodeKey:'NodeKey) =    
        let dic = Algorithms.Dijkstra.ofAdjGraph nodeKey getEdgeWeightF graph
        let shortestPathSum = 
            seq {
                    for nv in dic do
                        nv.Value
            }
            |>Seq.sum
        1. / shortestPathSum

    /// <summary> 
    /// Get the normalised ClosenessCentrality of a node in an AdjGraph
    /// </summary>
    /// <param name="getEdgeWeightF">Function to get the edgeweight out of the 'EdgeData</param>     
    /// <param name="graph">The graph to be analysed</param>     
    /// <param name="nodeKey">The NodeKey to get the Eccentricity of</param> 
    /// <returns>A float of the ClosenessCentrality of the given node</returns>
    static member ofAdjGraphNodeNormalised  (getEdgeWeightF:'EdgeData -> float) (graph :  AdjGraph<'NodeKey, 'NodeData, 'EdgeData>) (nodeKey:'NodeKey) =    
        let nodeCount = Measures.Size.compute graph |> float
        let dic = Algorithms.Dijkstra.ofAdjGraph nodeKey getEdgeWeightF graph
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
    /// <param name="getEdgeWeightF">Function to get the edgeweight out of the 'EdgeData</param>     
    /// <param name="graph">The graph to be analysed</param>     
    /// <returns>A float of the ClosenessCentrality of all nodes in the given graph</returns>
    static member ofFGraph  (getEdgeWeightF:'EdgeData -> float) (graph :  FGraph<'NodeKey, 'NodeData, 'EdgeData>) =    
        let dict = new Dictionary<'NodeKey,float>()
        for i in graph.Keys do
            let closeness = ClosenessCentrality.ofFGraphNode getEdgeWeightF graph i
            dict.Add(i,closeness)
        dict

    /// <summary> 
    /// Get the normalised ClosenessCentrality of all nodes in a FGraph
    /// </summary>
    /// <param name="getEdgeWeightF">Function to get the edgeweight out of the 'EdgeData</param>     
    /// <param name="graph">The graph to be analysed</param>     
    /// <returns>A float of the ClosenessCentrality of all nodes in the given graph</returns>
    static member ofFGraphNormalised  (getEdgeWeightF:'EdgeData -> float) (graph :  FGraph<'NodeKey, 'NodeData, 'EdgeData>) =    
        let dict = new Dictionary<'NodeKey,float>()
        for i in graph.Keys do
            let closeness = ClosenessCentrality.ofFGraphNodeNormalised getEdgeWeightF graph i
            dict.Add(i,closeness)
        dict


    /// <summary> 
    /// Get the ClosenessCentrality of all nodes in a FGraph
    /// </summary>
    /// <param name="getEdgeWeightF">Function to get the edgeweight out of the 'EdgeData</param>     
    /// <param name="graph">The graph to be analysed</param>     
    /// <returns>A float of the ClosenessCentrality of all nodes in the given graph</returns>
    static member ofAdjGraph (getEdgeWeightF:'EdgeData -> float) (graph :  AdjGraph<'NodeKey, 'NodeData, 'EdgeData>) =    
        let dict = new Dictionary<'NodeKey,float>()
        for i in graph.Keys do
            let closeness = ClosenessCentrality.ofAdjGraphNode getEdgeWeightF graph i
            dict.Add(i,closeness)
        dict


    /// <summary> 
    /// Get the normalised ClosenessCentrality of all nodes in a FGraph
    /// </summary>
    /// <param name="getEdgeWeightF">Function to get the edgeweight out of the 'EdgeData</param>     
    /// <param name="graph">The graph to be analysed</param>     
    /// <returns>A float of the ClosenessCentrality of all nodes in the given graph</returns>
    static member ofAdjGraphNormalised  (getEdgeWeightF:'EdgeData -> float) (graph :  AdjGraph<'NodeKey, 'NodeData, 'EdgeData>) =    
        let dict = new Dictionary<'NodeKey,float>()
        for i in graph.Keys do
            let closeness = ClosenessCentrality.ofAdjGraphNodeNormalised getEdgeWeightF graph i
            dict.Add(i,closeness)
        dict

    /// <summary> 
    /// Get the normalised ClosenessCentrality of all nodes in a FGraph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param>     
    /// <returns>A float of the ClosenessCentrality of all nodes in the given graph</returns>
    static member computeNormalised (graph :  FGraph<'NodeKey, 'NodeData, 'EdgeData>) =  
        ClosenessCentrality.ofFGraphNormalised (fun x -> 1.) graph

    /// <summary> 
    /// Get the normalised ClosenessCentrality of all nodes in an AdjGraph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param>     
    /// <returns>A float of the ClosenessCentrality of all nodes in the given graph</returns>
    static member computeNormalised (graph :  AdjGraph<'NodeKey, 'NodeData, 'EdgeData>) =  
        ClosenessCentrality.ofAdjGraphNormalised (fun x -> 1.) graph

    /// <summary> 
    /// Get the normalised ClosenessCentrality of all nodes in a FGraph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param>     
    /// <returns>A float of the ClosenessCentrality of all nodes in the given graph</returns>
    static member computeNormalisedWithEdgeData (graph :  FGraph<'NodeKey, 'NodeData, float>) =  
        ClosenessCentrality.ofFGraphNormalised id graph

    /// <summary> 
    /// Get the normalised ClosenessCentrality of all nodes in an AdjGraph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param>     
    /// <returns>A float of the ClosenessCentrality of all nodes in the given graph</returns>
    static member computeNormalisedWithEdgeData (graph :  AdjGraph<'NodeKey, 'NodeData, float>) =  
        ClosenessCentrality.ofAdjGraphNormalised id graph

    /// <summary> 
    /// Get the normalised ClosenessCentrality of all nodes in a FGraph
    /// </summary>   
    /// <param name="weigthF">Function to get a float edgeweight of the EdgeData</param> 
    /// <param name="graph">The graph to be analysed</param>     
    /// <returns>A float of the ClosenessCentrality of all nodes in the given graph</returns>
    static member computeNormalisedWithEdgeDataBy ((weightF:'EdgeData -> float),(graph :  FGraph<'NodeKey, 'NodeData, 'EdgeData>)) =  
        ClosenessCentrality.ofFGraphNormalised weightF graph

    /// <summary> 
    /// Get the normalised ClosenessCentrality of all nodes in an AdjGraph
    /// </summary>
    /// <param name="weigthF">Function to get a float edgeweight of the EdgeData</param> 
    /// <param name="graph">The graph to be analysed</param>     
    /// <returns>A float of the ClosenessCentrality of all nodes in the given graph</returns>
    static member computeNormalisedWithEdgeDataBy ((weightF:'EdgeData -> float),(graph :  AdjGraph<'NodeKey, 'NodeData, 'EdgeData>)) =  
        ClosenessCentrality.ofAdjGraphNormalised weightF graph



    /// <summary> 
    /// Get the ClosenessCentrality of all nodes in a FGraph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param>     
    /// <returns>A float of the ClosenessCentrality of all nodes in the given graph</returns>
    static member compute (graph :  FGraph<'NodeKey, 'NodeData, 'EdgeData>) =  
        ClosenessCentrality.ofFGraph (fun x -> 1.) graph

    /// <summary> 
    /// Get the ClosenessCentrality of all nodes in an AdjGraph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param>     
    /// <returns>A float of the ClosenessCentrality of all nodes in the given graph</returns>
    static member compute (graph :  AdjGraph<'NodeKey, 'NodeData, 'EdgeData>) =  
        ClosenessCentrality.ofAdjGraph (fun x -> 1.) graph

    /// <summary> 
    /// Get the ClosenessCentrality of all nodes in a FGraph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param>     
    /// <returns>A float of the ClosenessCentrality of all nodes in the given graph</returns>
    static member computeWithEdgeData (graph :  FGraph<'NodeKey, 'NodeData, float>) =  
        ClosenessCentrality.ofFGraph id graph

    /// <summary> 
    /// Get the ClosenessCentrality of all nodes in an AdjGraph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param>     
    /// <returns>A float of the ClosenessCentrality of all nodes in the given graph</returns>
    static member computeWithEdgeData (graph :  AdjGraph<'NodeKey, 'NodeData, float>) =  
        ClosenessCentrality.ofAdjGraph id graph

    /// <summary> 
    /// Get the ClosenessCentrality of all nodes in a FGraph
    /// </summary>   
    /// <param name="weigthF">Function to get a float edgeweight of the EdgeData</param> 
    /// <param name="graph">The graph to be analysed</param>     
    /// <returns>A float of the ClosenessCentrality of all nodes in the given graph</returns>
    static member computWithEdgeDataBy ((weightF:'EdgeData -> float),(graph :  FGraph<'NodeKey, 'NodeData, 'EdgeData>)) =  
        ClosenessCentrality.ofFGraph weightF graph

    /// <summary> 
    /// Get the ClosenessCentrality of all nodes in an AdjGraph
    /// </summary>
    /// <param name="weigthF">Function to get a float edgeweight of the EdgeData</param> 
    /// <param name="graph">The graph to be analysed</param>     
    /// <returns>A float of the ClosenessCentrality of all nodes in the given graph</returns>
    static member computeWithEdgeDataBy ((weightF:'EdgeData -> float),(graph :  AdjGraph<'NodeKey, 'NodeData, 'EdgeData>)) =  
        ClosenessCentrality.ofAdjGraph weightF graph
