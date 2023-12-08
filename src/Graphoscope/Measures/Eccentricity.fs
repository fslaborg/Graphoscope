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
    /// Get the Eccentricity of a node in an UndirectedFContext
    /// </summary>
    /// <param name="getEdgeWeightF">Function to get the edgeweight out of the 'EdgeData</param>     
    /// <param name="graph">The graph to be analysed</param>     
    /// <param name="nodeKey">The NodeKey to get the Eccentricity of</param> 
    /// <returns>A float of the Eccentricity of the node</returns>
    static member OfUndirectedFContextMapNode (getEdgeWeightF:'EdgeData -> float) (graph :  Undirected.FContextMapU<'NodeKey, 'NodeData, 'EdgeData>) (nodeKey:'NodeKey) =    
        //Get the collection of the shortest Paths by the given Dijkstra function and find the longest shortest path
        let dic = Algorithms.Dijkstra.OfUndirectedFContextMap nodeKey getEdgeWeightF graph
        let eccentricity = Seq.max(dic.Values)
        eccentricity

    /// <summary> 
    /// Get the Eccentricity of all nodes in an UndirectedFContext
    /// </summary>
    /// <param name="getEdgeWeightF">Function to get the edgeweight out of the 'EdgeData</param>     
    /// <param name="graph">The graph to be analysed</param>     
    /// <param name="nodeKey">The NodeKey to get the Eccentricity of</param> 
    /// <returns>A float of the Eccentricity of the node</returns>
    static member OfUndirectedFContextMap (getEdgeWeightF:'EdgeData -> float) (graph :  Undirected.FContextMapU<'NodeKey, 'NodeData, 'EdgeData>) =    
        graph.Keys
        |> Seq.map(fun k ->
            k,
            Eccentricity.OfUndirectedFContextMapNode getEdgeWeightF graph k
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


    static member computeUndirected (graph :  Undirected.FContextMapU<'NodeKey, 'NodeData, 'EdgeData>) =
        Eccentricity.OfUndirectedFContextMap (fun x -> 1.) graph

    static member computeUndirectedWithEdgeData (graph :  Undirected.FContextMapU<'NodeKey, 'NodeData, float>) =
        Eccentricity.OfUndirectedFContextMap id graph

    static member computeUndirectedWithEdgeDataBy ((getEdgeWeightF:'EdgeData -> float),(graph :  Undirected.FContextMapU<'NodeKey, 'NodeData, 'EdgeData>)) =
        Eccentricity.OfUndirectedFContextMap getEdgeWeightF graph
    

    static member computeOfNode ((graph :  Directed.FContextMap<'NodeKey, 'NodeData, 'EdgeData>),(nodeKey:'NodeKey)) =
        Eccentricity.ofFContextMapNode (fun x -> 1.) graph nodeKey

    static member computeOfNodeWithEdgeData ((graph :  Directed.FContextMap<'NodeKey, 'NodeData, float>),(nodeKey:'NodeKey)) =
        Eccentricity.ofFContextMapNode id graph nodeKey

    static member computeOfNodeWithEdgeDataBy ((getEdgeWeightF:'EdgeData -> float),(graph :  Directed.FContextMap<'NodeKey, 'NodeData, 'EdgeData>),(nodeKey:'NodeKey)) =
        Eccentricity.ofFContextMapNode getEdgeWeightF graph nodeKey

    static member computeOfNodeUndirected ((graph :  Undirected.FContextMapU<'NodeKey, 'NodeData, 'EdgeData>),(nodeKey:'NodeKey)) =
        Eccentricity.OfUndirectedFContextMapNode (fun x -> 1.) graph nodeKey

    static member computeOfNodeWithEdgeDataUndirected ((graph :  Undirected.FContextMapU<'NodeKey, 'NodeData, float>),(nodeKey:'NodeKey)) =
        Eccentricity.OfUndirectedFContextMapNode id graph nodeKey

    static member computeOfNodeWithEdgeDataByUndirected ((getEdgeWeightF:'EdgeData -> float),(graph :  Undirected.FContextMapU<'NodeKey, 'NodeData, 'EdgeData>),(nodeKey:'NodeKey)) =
        Eccentricity.OfUndirectedFContextMapNode getEdgeWeightF graph nodeKey
