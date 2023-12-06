namespace Graphoscope.Measures

open Graphoscope
open Graphoscope.Graphs
open System.Collections.Generic
open FSharpAux

type Eccentricity() =
    
    /// <summary> 
    /// Get the Eccentricity of a node in an FContextMap
    /// </summary>
    /// <param name="getEdgeWeightF">Function to get the edgeweight out of the 'EdgeData</param>     
    /// <param name="graph">The graph to be analysed</param>     
    /// <param name="nodeKey">The NodeKey to get the Eccentricity of</param> 
    /// <returns>A float of the Eccentricity of the node</returns>
    static member ofFContextMapNode (getEdgeWeightF:'EdgeData -> float) (graph :  Directed.FContextMap<'NodeKey, 'NodeData, 'EdgeData>) (nodeKey:'NodeKey) =    
        //Get the collection of the shortest Paths by the given Dijkstra function and find the longest shortest path
        let dic = Algorithms.Dijkstra.ofFContextMap nodeKey getEdgeWeightF graph
        let eccentricity = Seq.max(dic.Values)
        eccentricity

    /// <summary> 
    /// Get the Eccentricity of a node in an FContextMap
    /// </summary>
    /// <param name="getEdgeWeightF">Function to get the edgeweight out of the 'EdgeData</param>     
    /// <param name="graph">The graph to be analysed</param>     
    /// <param name="nodeKey">The NodeKey to get the Eccentricity of</param> 
    /// <returns>A float of the Eccentricity of the node</returns>
    static member ofFContextMap (getEdgeWeightF:'EdgeData -> float) (graph :  Directed.FContextMap<'NodeKey, 'NodeData, 'EdgeData>) =    
        graph.Keys
        |> Seq.map(fun k ->
            k,
            Eccentricity.ofFContextMapNode getEdgeWeightF graph k
        )
      
    /// <summary> 
    /// Get the Eccentricity of a node in an AdjGraph
    /// </summary>
    /// <param name="getEdgeWeightF">Function to get the edgeweight out of the 'EdgeData</param>     
    /// <param name="graph">The graph to be analysed</param>     
    /// <param name="nodeKey">The NodeKey to get the Eccentricity of</param> 
    /// <returns>A float of the Eccentricity of the node</returns>
    static member ofAdjGraphNode (getEdgeWeightF:'EdgeData -> float) (graph :  AdjGraph<'NodeKey, 'NodeData, 'EdgeData>) (nodeKey:'NodeKey) =    
        //Get the collection of the shortest Paths by the given Dijkstra function and find the longest shortest path
        let dic = Algorithms.Dijkstra.ofAdjGraph nodeKey getEdgeWeightF graph
        let eccentricity = Seq.max(dic.Values)
        eccentricity

    /// <summary> 
    /// Get the Eccentricity of all nodes in an AdjGraph
    /// </summary>
    /// <param name="getEdgeWeightF">Function to get the edgeweight out of the 'EdgeData</param>     
    /// <param name="graph">The graph to be analysed</param>     
    /// <param name="nodeKey">The NodeKey to get the Eccentricity of</param> 
    /// <returns>A float of the Eccentricity of the node</returns>
    static member ofAdjGraph (getEdgeWeightF:'EdgeData -> float) (graph :  AdjGraph<'NodeKey, 'NodeData, 'EdgeData>) =    
        graph.Keys
        |> Seq.map(fun k ->
            k,
            Eccentricity.ofAdjGraphNode getEdgeWeightF graph k
        )

    /// <summary> 
    /// Get the Eccentricity of a graph of its FloydWarshall shortest Path result
    /// </summary>
    /// <param name="floydWarshallResult">Result of the FloydWarshall shortest Path calculation of a graph</param>  
    /// <returns>A dictionary with the nodeIndeces as Keys and the Eccentricity as value</returns>
    static member oFContextMap2D (floydWarshallResult : float [,]) =    
        let shortestPaths = floydWarshallResult
        // let indexToNode = graph.Keys|>Seq.map(fun x -> nodeIndexer x,x)|> Map.ofSeq
        let dict = new Dictionary<int,float>()
        let getDict (arr: _ [,])  =
            let n,m = arr |> Array2D.length1, arr |> Array2D.length2
            let rec getMax i j max =
                if j = m then max
                else
                    let value = arr.[i,j]
                    if value < max then getMax i (j+1) max 
                    else getMax i (j+1) value 
            
            for i=0 to (n-1) do
                let eccentricity = getMax i 0 arr.[i,0]
                let key = i//indexToNode.Item i
                dict.Add(key,eccentricity)
            
            dict
        getDict shortestPaths

    static member compute (graph :  Directed.FContextMap<'NodeKey, 'NodeData, 'EdgeData>) =
        Eccentricity.ofFContextMap (fun x -> 1.) graph

    static member computeWithEdgeData (graph :  Directed.FContextMap<'NodeKey, 'NodeData, float>) =
        Eccentricity.ofFContextMap id graph

    static member computeWithEdgeDataBy ((getEdgeWeightF:'EdgeData -> float),(graph :  Directed.FContextMap<'NodeKey, 'NodeData, 'EdgeData>)) =
        Eccentricity.ofFContextMap getEdgeWeightF graph


    static member compute (graph :  AdjGraph<'NodeKey, 'NodeData, 'EdgeData>) =
        Eccentricity.ofAdjGraph (fun x -> 1.) graph

    static member computeWithEdgeData (graph :  AdjGraph<'NodeKey, 'NodeData, float>) =
        Eccentricity.ofAdjGraph id graph

    static member computeWithEdgeDataBy ((getEdgeWeightF:'EdgeData -> float),(graph :  AdjGraph<'NodeKey, 'NodeData, 'EdgeData>)) =
        Eccentricity.ofAdjGraph getEdgeWeightF graph
    

    static member computeOfNode ((graph :  Directed.FContextMap<'NodeKey, 'NodeData, 'EdgeData>),(nodeKey:'NodeKey)) =
        Eccentricity.ofFContextMapNode (fun x -> 1.) graph nodeKey

    static member computeOfNodeWithEdgeData ((graph :  Directed.FContextMap<'NodeKey, 'NodeData, float>),(nodeKey:'NodeKey)) =
        Eccentricity.ofFContextMapNode id graph nodeKey

    static member computeOfNodeWithEdgeDataBy ((getEdgeWeightF:'EdgeData -> float),(graph :  Directed.FContextMap<'NodeKey, 'NodeData, 'EdgeData>),(nodeKey:'NodeKey)) =
        Eccentricity.ofFContextMapNode getEdgeWeightF graph nodeKey

    static member computeOfNode ((graph :  AdjGraph<'NodeKey, 'NodeData, 'EdgeData>),(nodeKey:'NodeKey)) =
        Eccentricity.ofAdjGraphNode (fun x -> 1.) graph nodeKey

    static member computeOfNodeWithEdgeData ((graph :  AdjGraph<'NodeKey, 'NodeData, float>),(nodeKey:'NodeKey)) =
        Eccentricity.ofAdjGraphNode id graph nodeKey

    static member computeOfNodeWithEdgeDataBy ((getEdgeWeightF:'EdgeData -> float),(graph :  AdjGraph<'NodeKey, 'NodeData, 'EdgeData>),(nodeKey:'NodeKey)) =
        Eccentricity.ofAdjGraphNode getEdgeWeightF graph nodeKey
