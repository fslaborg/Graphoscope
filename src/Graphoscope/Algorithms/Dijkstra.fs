namespace Graphoscope.Algorithms

open FSharpAux
open Graphoscope
open System.Collections.Generic

/// <summary> 
/// Computes Dijkstra's shortest path
/// </summary>
type Dijkstra() =
    

    // Function to perform Dijkstra's shortest path algorithm
    static member ofFGraph (starting : 'NodeKey) (graph :  FGraph<'NodeKey, 'NodeData, float>) =
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
                if kv.Value < 0. then failwithf "Dijkstra does not handle neg. edge weigth"
                let totalDistance = (currentDistance + kv.Value) // Assuming edgeWeight is always 1 in this example
                // Impove getValue
                if totalDistance < distance.[kv.Key] then
                    distance.[kv.Key] <- totalDistance
                    priorityQueue.Add((kv.Key, totalDistance)) |> ignore
        

        distance

    /// Computes the shortest path
    static member internal getAdjacencyArrayFor (graph: DiGraph<'NodeKey, float>) (nodeIx: int) =
            let dist =
                Array.init (graph.NodeKeys.Count) (fun x -> if x = nodeIx then 0. else infinity)
            graph.OutEdges[nodeIx]
            |> ResizeArray.iter(fun (target, w) -> dist[target] <- w)
            dist

        /// <summary> 
        /// Computes shortest paths from <paramref name="source"/> for <paramref name="graph"/> using Dijkstra's algorithm in parallel.
        /// </summary>
        /// <param name="graph"> The graph for which to compute the shortest path.</param>
        /// <param name="source"> Calculate the shortest paths from this node.</param>
        /// <remarks>If there isn't a path between two edges, the distance is set to `infinity`.</remarks>
        /// <returns>Tuples of target node and distance.</returns>
        static member Compute (graph: DiGraph<'NodeKey, float>) (source: 'NodeKey): ('NodeKey * float) [] =
            let que= ResizeArray()
            let sourceIx = graph.IdMap[source]
            let dist = Dijkstra.getAdjacencyArrayFor graph sourceIx

            for n in 0 .. graph.NodeKeys.Count - 1 do
                que.Add(n)

            while que.Count > 0 do
                let minDistNode = 
                    que
                    |> ResizeArray.minBy( fun n -> dist[n])

                let minDistNodeIx =  que.IndexOf minDistNode
                que.RemoveAt minDistNodeIx

                let minDistAdjacency = Dijkstra.getAdjacencyArrayFor graph minDistNode

                for n in que do
                    let newCost = dist[minDistNode] + minDistAdjacency[n]
                    if newCost < dist[n] then
                        dist[n] <- newCost
            dist
            |> Array.mapi(fun i x -> graph.NodeKeys[i], x)
        
        /// <summary> 
        /// Computes all-pairs shortest paths for <paramref name="graph"/> using Dijkstra algorithm in parallel.
        /// </summary>
        /// <param name="graph">The graph for which to compute the shortest paths.</param>
        /// <remarks>If there isn't a path between two edges, the distance is set to `infinity`.</remarks>
        /// <returns>
        /// The ordered array of nodes and 2D Array of distances where each
        /// row and column index corresponds to a node's index in the nodes array.
        /// </returns>
        static member ComputeAllPairs (graph: DiGraph<'NodeKey, float>): 'NodeKey [] * float [][] =
            let allDists = DiGraph.toMatrix graph
            allDists
            |> Array.iteri(fun ri r ->
                r
                |> Array.iteri(fun ci c ->
                    if c = 0. && ri <> ci then
                        allDists[ri][ci] <- infinity
                    elif ri = ci then
                        allDists[ri][ci] <- 0.
                )
            )
            
            let dijkstra (sourceIx: int) =
                let que= ResizeArray()
                let dist = allDists[sourceIx] |> Array.copy

                for n in 0 .. graph.NodeKeys.Count - 1 do
                    que.Add(n)

                while que.Count > 0 do
                    let minDistNode = 
                        que
                        |> Seq.minBy( fun n -> dist[n])

                    let minDistNodeIx =  que.IndexOf minDistNode
                    que.RemoveAt minDistNodeIx

                    for n in que do
                        let newCost = dist[minDistNode] + allDists[minDistNode][n]
                        if newCost < dist[n] then
                            dist[n] <- newCost
                dist

            graph.NodeKeys |> Array.ofSeq,
            [|0 .. graph.NodeKeys.Count - 1|]
            |> Array.Parallel.map dijkstra 

        /// <summary> 
        /// Returns the distance in numebr of directed edges between two nodes.
        /// </summary>
        /// <param name="origin">The starting node of the path</param> 
        /// <param name="destination">The finishing node of the path</param> 
        /// <param name="graph">The graph to be analysed</param> 
        /// <returns>A float of the distance</returns>
        static member getShortestPath (origin :'NodeKey)  (destination :'NodeKey) (graph : DiGraph<'NodeKey, float>)  =
            Dijkstra.Compute graph origin 
            |> Array.tryFind(fun (d,_) -> d = destination)
            |> fun o -> 
                match o with 
                | Some (n,f) -> Some f
                | None -> None
        



