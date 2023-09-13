namespace Graphoscope.Measures

open Graphoscope
open System.Collections.Generic


type ShortestPath={
    target:int
    path:Set<int>
}

type BetweennessCentrality() =
    
    /// <summary> 
    /// Get all of the shortest paths of a FGraph
    /// </summary>
    /// <param name="shortestPathAlgorithm">Function to calculate the shortest Path via floydWarshall</param> 
    /// <param name="nodeIndexer">Function to index nodes based on their 'NodeKey</param>    
    /// <param name="getEdgeWeightF">Function to get the edgeweight out of the 'EdgeData</param>      
    /// <param name="graph">The graph to be analysed</param>     
    /// <returns>A Dictionary with the indexed Node as the key and the shortest Paths as Set as value </returns>
    static member returnPaths (shortestPathAlgorithm:'NodeKey->('EdgeData -> float) ->FGraph<'NodeKey,'NodeData,'EdgeData> -> Dictionary<'NodeKey,'NodeKey * float>) (nodeIndexer:'NodeKey->int) (getEdgeWeight : 'EdgeData -> float) (graph: FGraph<'NodeKey,'NodeData,'EdgeData>) = 
        //Opens a Dictionary for ease of use
        let paths :Dictionary<('NodeKey),Set<ShortestPath>> = Dictionary<('NodeKey),Set<ShortestPath>>()

        //Return the shortest Paths out of a Dijkstra-Result: The Result contains node; previously visited node, shortest Path. From there you can get the complete path to the node from the starting node.
        let getPathsOfDic key (dic:Dictionary<'NodeKey,('NodeKey*float)>) starting=
            let getPrev key =
                dic.Item key |> fst
            let rec getPath key (set:Set<'NodeKey>) =
                let prev = getPrev key
                //printfn $"Key:{key};prev:{prev}"
                if prev = starting then 
                    set
                else
                    let newSet = Set.add prev set
                    getPath prev newSet
            getPath key Set.empty

        //Function to build the path set of a given starting node 
        let rec buildSet (set:Set<ShortestPath>) (starting:'NodeKey) (sp: Dictionary<'NodeKey,('NodeKey * float)>) (keysList:'NodeKey list) counter =
            if counter=keysList.Length then
                set
            else
                let x = keysList.[counter] 
                if x=starting then 
                    buildSet set starting sp keysList (counter+1)
                else
                    let setToSave = {
                        target=x
                        path=getPathsOfDic x sp starting
                    }
                    let newSet = set.Add(setToSave)
                    buildSet newSet starting sp keysList (counter+1)

        //Get the shortest Paths Set collection for each node in the graph
        let rec getPaths (keysList:'NodeKey list)  counter =
            if counter = Seq.length keysList then
                paths
            else
                let starting = keysList.[counter]
                let sp = shortestPathAlgorithm (starting) getEdgeWeight graph
                let set = buildSet Set.empty starting sp keysList 0

                paths.Add(starting,set)
                    
                getPaths keysList (counter+1)
        getPaths (graph.Keys|>Seq.map(fun x -> nodeIndexer x)|>List.ofSeq) 0


    /// <summary> 
    /// Get all of the shortest paths of a FGraph
    /// </summary>
    /// <param name="paths">The result of the returnPaths function. A collection of all shortestPaths </param> 
    /// <param name="nodeIndexer">Function to index nodes based on their 'NodeKey</param>    
    /// <param name="node">The NodeKey to get the Betweenness of</param>     
    /// <returns>A float of the betweenness of the given node </returns>
    static member getBetweennessOfPathsAndNode (paths:Dictionary<int,Set<ShortestPath>>) (nodeIndexer:'NodeKey->int) (node:'NodeKey)=
        //Returns how many shortest paths can exist by combinatorics
        let pathCount = paths.Count*(paths.Count-1)|>float
        
        //Filter all shortest paths that are started or go to the node of interest
        paths
        |> Seq.choose(fun nkv -> 
            let nodeV = nodeIndexer node
            if nkv.Key<>(nodeV) then
                Some (nkv.Value|>Seq.sumBy(fun (x:ShortestPath) -> if x.target<>nodeV && x.path.Contains nodeV then 1. else 0.))
            else    
                None
        )
        |> Seq.sum
        |> fun x -> x/pathCount
