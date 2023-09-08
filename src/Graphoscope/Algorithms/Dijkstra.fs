namespace Graphoscope.Algorithms

open FSharpAux
open Graphoscope
open System.Collections.Generic

/// <summary> 
/// Computes Dijkstra's shortest path
/// </summary>
type Dijkstra() =
    
    
    // Function to perform Dijkstra's shortest path algorithm
    static member ofDirectedFGraph (starting : 'NodeKey) (getEdgeWeight : 'EdgeData -> float) (graph :  FGraph<'NodeKey, 'NodeData, 'EdgeData> ) =
        let distance = Dictionary<'NodeKey, float>()
        let priorityQueue = SortedSet<'NodeKey * float>(Comparer<'NodeKey * float>.Create(fun (_, d1) (_, d2) -> compare d1 d2))
        let infinity = System.Double.MaxValue

        // Initialize distances to infinity for all nodes except the starting node
        // TODO: this can be improved by getOrDefault
        for nodeKey in graph.Keys do
            if nodeKey = starting then
                distance.[nodeKey] <- 0.
            else
                distance.[nodeKey] <- infinity

        priorityQueue.Add((starting, 0)) |> ignore

        while priorityQueue.Count > 0 do
            let (currentNode, currentDistance) = priorityQueue.Min
            priorityQueue.Remove(priorityQueue.Min) |> ignore
        
            let (_, _, predecessors) = graph.[currentNode]

            for kv in predecessors do
                let kvValue = kv.Value |> getEdgeWeight
                if kvValue < 0. then failwithf "Dijkstra does not handle neg. edge weigth"
                let totalDistance = (currentDistance + kvValue) // Assuming edgeWeight is always 1 in this example
                // Impove getValue
                if totalDistance < distance.[kv.Key] then
                    distance.[kv.Key] <- totalDistance
                    priorityQueue.Add((kv.Key, totalDistance)) |> ignore
        

        distance

    // Function to perform Dijkstra's shortest path algorithm
    static member ofUndirectedFGraph (starting : 'NodeKey) (getEdgeWeight : 'EdgeData -> float) (graph :  FGraph<'NodeKey, 'NodeData, 'EdgeData> ) =
        let distance = Dictionary<'NodeKey, float>()
        let priorityQueue = SortedSet<'NodeKey * float>(Comparer<'NodeKey * float>.Create(fun (_, d1) (_, d2) -> compare d1 d2))
        let infinity = System.Double.MaxValue

        // Initialize distances to infinity for all nodes except the starting node
        // TODO: this can be improved by getOrDefault
        for nodeKey in graph.Keys do
            if nodeKey = starting then
                distance.[nodeKey] <- 0.
            else
                distance.[nodeKey] <- infinity

        priorityQueue.Add((starting, 0)) |> ignore

        while priorityQueue.Count > 0 do
            let (currentNode, currentDistance) = priorityQueue.Min
            priorityQueue.Remove(priorityQueue.Min) |> ignore
        
            let neighbours = graph.[currentNode] |> FContext.neighbours

            for node,rawDistance in neighbours do
                let weightedDistance = rawDistance |> getEdgeWeight
                if weightedDistance < 0. then failwithf "Dijkstra does not handle neg. edge weigth"
                let totalDistance = (currentDistance + weightedDistance) // Assuming edgeWeight is always 1 in this example
                // Impove getValue
                if totalDistance < distance.[node] then
                    distance.[node] <- totalDistance
                    priorityQueue.Add((node, totalDistance)) |> ignore
        
        distance
    // Function to perform Dijkstra's shortest path algorithm
    static member ofUndirectedFGraphIncludingPath (starting : 'NodeKey) (getEdgeWeight : 'EdgeData -> float) (graph :  FGraph<'NodeKey, 'NodeData, 'EdgeData> ) =
        let distance = Dictionary<'NodeKey,('NodeKey*float)>()
        let priorityQueue = SortedSet<'NodeKey * float>(Comparer<'NodeKey * float>.Create(fun (_, d1) (_, d2) -> compare d1 d2))
        let infinity = System.Double.MaxValue

        // Initialize distances to infinity for all nodes except the starting node
        // TODO: this can be improved by getOrDefault
        for nodeKey in graph.Keys do
            if nodeKey = starting then
                distance.[nodeKey] <- (starting,0.)
            else
                distance.[nodeKey] <- (starting,infinity)

        priorityQueue.Add((starting, 0)) |> ignore

        while priorityQueue.Count > 0 do
            let (currentNode, currentDistance) = priorityQueue.Min
            priorityQueue.Remove(priorityQueue.Min) |> ignore
        
            let neighbours = graph.[currentNode] |> FContext.neighbours

            for node,rawDistance in neighbours do
                let weightedDistance = rawDistance |> getEdgeWeight
                if weightedDistance < 0. then failwithf "Dijkstra does not handle neg. edge weigth"
                let totalDistance = (currentDistance + weightedDistance) // Assuming edgeWeight is always 1 in this example
                // Impove getValue
                let prevNode,prevDistance = distance.[node]
                if totalDistance < prevDistance then
                    distance.[node] <- (currentNode,totalDistance)
                    priorityQueue.Add((node, totalDistance)) |> ignore
        
        distance

    /// Computes the shortest path
    static member internal getAdjacencyArrayFor (graph: DiGraph<'NodeKey, 'NodeData, 'EdgeData>) (getEdgeWeight : 'EdgeData -> float) (nodeIx: int) =
            let dist =
                Array.init (graph.NodeKeys.Count) (fun x -> if x = nodeIx then 0. else infinity)
            graph.OutEdges[nodeIx]
            |> ResizeArray.iter(fun (target, w) -> dist[target] <- getEdgeWeight w)
            dist

    /// <summary> 
    /// Computes shortest paths from <paramref name="source"/> for <paramref name="graph"/> using Dijkstra's algorithm in parallel.
    /// </summary>
    /// <param name="graph"> The graph for which to compute the shortest path.</param>
    /// <param name="source"> Calculate the shortest paths from this node.</param>
    /// <remarks>If there isn't a path between two edges, the distance is set to `infinity`.</remarks>
    /// <returns>Tuples of target node and distance.</returns>
    static member ofDiGraph (source: 'NodeKey) (getEdgeWeight : 'EdgeData -> float) (graph: DiGraph<'NodeKey, 'NodeData, 'EdgeData>) : ('NodeKey * float) [] =
        let que= ResizeArray()
        let sourceIx = graph.IdMap[source]
        let dist = Dijkstra.getAdjacencyArrayFor graph getEdgeWeight sourceIx

        for n in 0 .. graph.NodeKeys.Count - 1 do
            que.Add(n)

        while que.Count > 0 do
            let minDistNode = 
                que
                |> ResizeArray.minBy( fun n -> dist[n])

            let minDistNodeIx =  que.IndexOf minDistNode
            que.RemoveAt minDistNodeIx

            let minDistAdjacency = Dijkstra.getAdjacencyArrayFor graph getEdgeWeight minDistNode

            for n in que do
                let newCost = dist[minDistNode] + minDistAdjacency[n]
                if newCost < dist[n] then
                    dist[n] <- newCost
        dist
        |> Array.mapi(fun i x -> graph.NodeKeys[i], x)

    
    /// <summary> 
    /// Returns the distance in numebr of directed edges between two nodes.
    /// </summary>
    /// <param name="origin">The starting node of the path</param> 
    /// <param name="destination">The finishing node of the path</param> 
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the distance</returns>
    static member ofDiGraphBetween (getEdgeWeight : 'EdgeData -> float) (graph : DiGraph<'NodeKey, 'NodeData, 'EdgeData>) (origin :'NodeKey)  (destination :'NodeKey)  =
        Dijkstra.ofDiGraph origin getEdgeWeight graph
        |> Array.tryFind(fun (d,_) -> d = destination)
        |> fun o -> 
            match o with 
            | Some (n,f) -> Some f
            | None -> None
    


    static member compute (starting : 'NodeKey, getEdgeWeight: ('EdgeData -> float), graph :  FGraph<'NodeKey, 'NodeData, 'EdgeData>) =
        Dijkstra.ofDirectedFGraph starting getEdgeWeight graph 

    static member compute (starting : 'NodeKey, getEdgeWeight: ('EdgeData -> float), graph :  DiGraph<'NodeKey, 'NodeData, 'EdgeData>) =
        Dijkstra.ofDiGraph starting getEdgeWeight graph 

    static member computeBetween (origin : 'NodeKey, destination :'NodeKey, graph :  FGraph<'NodeKey, 'NodeData, float>) =
        //TODO: Implement Dijkstra.ofFGraphBetween
        System.NotImplementedException() |> raise

    static member computeBetween (origin : 'NodeKey, destination :'NodeKey,  graph :  DiGraph<'NodeKey, 'NodeData, float>) =
        //TODO: Implement Dijkstra.ofDiGraphBetween
        System.NotImplementedException() |> raise

