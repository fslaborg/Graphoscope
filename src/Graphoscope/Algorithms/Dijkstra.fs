﻿namespace Graphoscope.Algorithms

open FSharpAux
open Graphoscope
open Graphoscope.Graphs
open System.Collections.Generic

/// <summary> 
/// Computes Dijkstra's shortest path
/// </summary>
type Dijkstra() =
    
    
    // Function to perform Dijkstra's shortest path algorithm
    static member ofFContextMap (starting : 'NodeKey) (getEdgeWeight : 'EdgeData -> float) (graph :  Directed.FContextMap<'NodeKey, 'NodeData, 'EdgeData> ) =
        let distance = Dictionary<'NodeKey, float>()
        //let priorityQueue = SortedSet<'NodeKey * float>(Comparer<'NodeKey * float>.Create(fun (_, d1) (_, d2) -> compare d1 d2))
        let priorityQueue: Queue<('NodeKey * float)> = System.Collections.Generic.Queue()//Priority_Queue.SimplePriorityQueue<('NodeKey*float),float>()
        let infinity = System.Double.MaxValue

        // Initialize distances to infinity for all nodes except the starting node
        // TODO: this can be improved by getOrDefault
        for nodeKey in graph.Keys do
            if nodeKey = starting then
                distance.[nodeKey] <- 0.
            else
                distance.[nodeKey] <- infinity

        priorityQueue.Enqueue((starting, 0.)) |> ignore

        while priorityQueue.Count > 0 do
            let (currentNode, currentDistance) = priorityQueue.Dequeue()
            //priorityQueue.Remove(priorityQueue.Min) |> ignore
        
            let (_, _, predecessors) = graph.[currentNode]

            for kv in predecessors do
                let kvValue = kv.Value |> getEdgeWeight
                if kvValue < 0. then failwithf "Dijkstra does not handle neg. edge weigth"
                let totalDistance = (currentDistance + kvValue) // Assuming edgeWeight is always 1 in this example
                // Impove getValue
                if totalDistance < distance.[kv.Key] then
                    distance.[kv.Key] <- totalDistance
                    priorityQueue.Enqueue(kv.Key,totalDistance) |> ignore
                    Seq.sortBy snd priorityQueue |>ignore


        distance

    // Function to perform Dijkstra's shortest path algorithm
    static member ofAdjGraph (starting : 'NodeKey) (getEdgeWeight : 'EdgeData -> float) (graph :  AdjGraph<'NodeKey, 'NodeData, 'EdgeData> ) =
        let distance = Dictionary<'NodeKey, float>()
        let priorityQueue: Queue<('NodeKey * float)> = System.Collections.Generic.Queue()//Priority_Queue.SimplePriorityQueue<('NodeKey*float),float>()
        let infinity = System.Double.MaxValue

        // Initialize distances to infinity for all nodes except the starting node
        // TODO: this can be improved by getOrDefault
        for nodeKey in graph.Keys do
            if nodeKey = starting then
                distance.[nodeKey] <- 0.
            else
                distance.[nodeKey] <- infinity

        priorityQueue.Enqueue((starting, 0.)) |> ignore

        while priorityQueue.Count > 0 do
            let ((currentNode), currentDistance) = priorityQueue.Dequeue()
            //priorityQueue.Remove(priorityQueue.Min) |> ignore
        
            let neighbours = AdjGraph.getNeighbours currentNode graph

            for node,rawDistance in neighbours do
                let weightedDistance = rawDistance |> getEdgeWeight
                if weightedDistance < 0. then failwithf "Dijkstra does not handle neg. edge weigth"
                let totalDistance = (currentDistance + weightedDistance) // Assuming edgeWeight is always 1 in this example
                // Impove getValue
                if totalDistance < distance.[node] then
                    distance.[node] <- totalDistance
                    priorityQueue.Enqueue(node,totalDistance) |> ignore
                    Seq.sortBy snd priorityQueue |>ignore

        distance

    // Function to perform Dijkstra's shortest path algorithm
    static member ofUndirectedFContextMapIncludingPath (starting : 'NodeKey) (getEdgeWeight : 'EdgeData -> float) (graph :  AdjGraph<'NodeKey, 'NodeData, 'EdgeData>) =
        let distance = Dictionary<'NodeKey,('NodeKey*float)>()
        let priorityQueue: Queue<('NodeKey * float)> = System.Collections.Generic.Queue()//Priority_Queue.SimplePriorityQueue<('NodeKey*float),float>()
        let infinity = System.Double.MaxValue

        // Initialize distances to infinity for all nodes except the starting node
        // TODO: this can be improved by getOrDefault
        for nodeKey in graph.Keys do
            if nodeKey = starting then
                distance.[nodeKey] <- (starting,0.)
            else
                distance.[nodeKey] <- (starting,infinity)

        priorityQueue.Enqueue((starting, 0)) |> ignore

        while priorityQueue.Count > 0 do
            let (currentNode, currentDistance) = priorityQueue.Dequeue()
            //priorityQueue.Remove(priorityQueue.Min) |> ignore
        
            let neighbours = AdjGraph.getNeighbours currentNode graph

            for node,rawDistance in neighbours do
                let weightedDistance = rawDistance |> getEdgeWeight
                if weightedDistance < 0. then failwithf "Dijkstra does not handle neg. edge weigth"
                let totalDistance = (currentDistance + weightedDistance) // Assuming edgeWeight is always 1 in this example
                // Impove getValue
                let prevNode,prevDistance = distance.[node]
                if totalDistance < prevDistance then
                    distance.[node] <- (currentNode,totalDistance)
                    priorityQueue.Enqueue(node,totalDistance) |> ignore
                    Seq.sortBy snd priorityQueue |>ignore

        distance

    /// Computes the shortest path
    static member internal getAdjacencyArrayForUndirected (graph: Undirected.UndirectedGraph<'NodeKey, 'NodeData, 'EdgeData>) (getEdgeWeight : 'EdgeData -> float) (nodeIx: int) =
        let dist =
            Array.init (graph.NodeKeys.Count) (fun x -> if x = nodeIx then 0. else infinity)
        graph.Edges[nodeIx]
        |> ResizeArray.iter(fun (target, w) -> dist[target] <- getEdgeWeight w)
        dist

    static member internal getAdjacencyArrayForDirected (graph: Directed.LilMatrix<'NodeKey, 'NodeData, 'EdgeData>) (getEdgeWeight : 'EdgeData -> float) (nodeIx: int) =
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
    static member ofUndirected (source: 'NodeKey) (getEdgeWeight : 'EdgeData -> float) (graph: Undirected.UndirectedGraph<'NodeKey, 'NodeData, 'EdgeData>) : ('NodeKey * float) [] =
        let que = SortedSet<int * float>(Comparer<int * float>.Create(fun (n1, d1) (n2, d2) -> compare (d1,n1) (d2,n2)))
        let sourceIx = graph.IdMap[source]
        let dist = Array.init (graph.NodeKeys.Count) (fun ix -> if ix = sourceIx then 0. else  infinity)

        que.Add((sourceIx, 0.)) |> ignore

        while que.Count > 0 do
            let (currentNodeIx, currentDistance) = que.Min
            que.Remove(que.Min) |> ignore

            let neighbors = graph.Edges[currentNodeIx]

            for (ix, ed) in neighbors do
                let newCost = currentDistance + (getEdgeWeight ed)
                if newCost < dist[ix] then
                    dist[ix] <- newCost
                    que.Add((ix, newCost)) |> ignore

        dist
        |> Array.mapi(fun i x -> graph.NodeKeys[i], x)

    /// <summary> 
    /// Computes shortest paths from <paramref name="source"/> for <paramref name="graph"/> using Dijkstra's algorithm in parallel.
    /// </summary>
    /// <param name="graph"> The graph for which to compute the shortest path.</param>
    /// <param name="source"> Calculate the shortest paths from this node.</param>
    /// <remarks>If there isn't a path between two edges, the distance is set to `infinity`.</remarks>
    /// <returns>Tuples of target node and distance.</returns>
    static member ofLilMatrix (source: 'NodeKey) (getEdgeWeight : 'EdgeData -> float) (graph: Directed.LilMatrix<'NodeKey, _, 'EdgeData>) : ('NodeKey * float) [] =
        let que= SortedSet<int * float>(Comparer<int * float>.Create(fun (n1, d1) (n2, d2) -> compare (d1,n1) (d2,n2)))
        let sourceIx = graph.IdMap[source]
        let dist = Array.init (graph.NodeKeys.Count) (fun ix -> if ix = sourceIx then 0. else  infinity)

        que.Add((sourceIx, 0.)) |> ignore

        while que.Count > 0 do
            let (currentNodeIx, currentDistance) = que.Min
            que.Remove(que.Min) |> ignore

            let successors = graph.OutEdges[currentNodeIx]

            for (ix, ed) in successors do
                let newCost = currentDistance + (getEdgeWeight ed)
                if newCost < dist[ix] then
                    que.Add((ix, newCost)) |> ignore
                    dist[ix] <- newCost
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
    static member ofUndirectedAllPairs (getEdgeWeight : 'EdgeData -> float) (graph: Undirected.UndirectedGraph<'NodeKey, 'NodeData, 'EdgeData>): 'NodeKey [] * float [][] =        
        let dijkstra (sourceIx: int) =
            let que= SortedSet<int * float>(Comparer<int * float>.Create(fun (n1, d1) (n2, d2) -> compare (d1,n1) (d2,n2)))
            let dist = Array.init (graph.NodeKeys.Count) (fun ix -> if ix = sourceIx then 0. else  infinity)

            que.Add((sourceIx, 0.)) |> ignore

            while que.Count > 0 do
                let (currentNodeIx, currentDistance) = que.Min
                que.Remove(que.Min) |> ignore

                let neighbors = graph.Edges[currentNodeIx]

                for (ix, ed) in neighbors do
                    let newCost = currentDistance + (getEdgeWeight ed)
                    if newCost < dist[ix] then
                        que.Add((ix, newCost)) |> ignore
                        dist[ix] <- newCost
            dist

        graph.NodeKeys |> Array.ofSeq,
        dijkstra |> Array.Parallel.init graph.NodeKeys.Count

    /// <summary> 
    /// Computes all-pairs shortest paths for <paramref name="graph"/> using Dijkstra algorithm in parallel.
    /// </summary>
    /// <param name="graph">The graph for which to compute the shortest paths.</param>
    /// <remarks>If there isn't a path between two edges, the distance is set to `infinity`.</remarks>
    /// <returns>
    /// The ordered array of nodes and 2D Array of distances where each
    /// row and column index corresponds to a node's index in the nodes array.
    /// </returns>
    static member ofLilMatrixAllPairs (getEdgeWeight : 'EdgeData -> float) (graph: Directed.LilMatrix<'NodeKey, _, 'EdgeData>): 'NodeKey [] * float [][] =
        let dijkstra (sourceIx: int) =
            let que= SortedSet<int * float>(Comparer<int * float>.Create(fun (n1, d1) (n2, d2) -> compare (d1,n1) (d2,n2)))
            let dist = Array.init (graph.NodeKeys.Count) (fun ix -> if ix = sourceIx then 0. else  infinity)

            que.Add((sourceIx, 0.)) |> ignore

            while que.Count > 0 do
                let (currentNodeIx, currentDistance) = que.Min
                que.Remove(que.Min) |> ignore

                let successors = graph.OutEdges[currentNodeIx]

                for (ix, ed) in successors do
                    let newCost = currentDistance + (getEdgeWeight ed)
                    if newCost < dist[ix] then
                        que.Add((ix, newCost)) |> ignore
                        dist[ix] <- newCost
            dist

        graph.NodeKeys |> Array.ofSeq,
        dijkstra |> Array.Parallel.init graph.NodeKeys.Count


    /// <summary> 
    /// Returns the distance in numebr of directed edges between two nodes.
    /// </summary>
    /// <param name="origin">The starting node of the path</param> 
    /// <param name="destination">The finishing node of the path</param> 
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the distance</returns>
    static member ofLilMatrixBetween (getEdgeWeight : 'EdgeData -> float) (graph : Directed.LilMatrix<'NodeKey, 'NodeData, 'EdgeData>) (origin :'NodeKey)  (destination :'NodeKey)  =
        Dijkstra.ofLilMatrix origin getEdgeWeight graph
        |> Array.tryFind(fun (d,_) -> d = destination)
        |> fun o -> 
            match o with 
            | Some (n,f) -> Some f
            | None -> None
    

    static member compute (starting : 'NodeKey, graph :  Directed.FContextMap<'NodeKey, 'NodeData, 'EdgeData>) =
        Dijkstra.ofFContextMap starting (fun x -> 1.) graph 
    static member computeWithEdgeData (starting : 'NodeKey, graph :  Directed.FContextMap<'NodeKey, 'NodeData, float>) =
        Dijkstra.ofFContextMap starting id graph 
    static member computeWithEdgeDataBy (starting : 'NodeKey, getEdgeWeight: ('EdgeData -> float), graph :  Directed.FContextMap<'NodeKey, 'NodeData, 'EdgeData>) =
        Dijkstra.ofFContextMap starting getEdgeWeight graph 

    static member compute (starting : 'NodeKey, graph :  AdjGraph<'NodeKey, 'NodeData, 'EdgeData>) =
        Dijkstra.ofAdjGraph starting (fun x -> 1.) graph 
    static member computeWithEdgeData (starting : 'NodeKey, graph :  AdjGraph<'NodeKey, 'NodeData, float>) =
        Dijkstra.ofAdjGraph starting id graph 
    static member computeWithEdgeDataBy (starting : 'NodeKey, getEdgeWeight: ('EdgeData -> float), graph :  AdjGraph<'NodeKey, 'NodeData, 'EdgeData>) =
        Dijkstra.ofAdjGraph starting getEdgeWeight graph 

    static member compute (starting : 'NodeKey, getEdgeWeight: ('EdgeData -> float), graph :  Directed.LilMatrix<'NodeKey, _, 'EdgeData>) =
        Dijkstra.ofLilMatrix starting getEdgeWeight graph 

    static member computeBetween (origin : 'NodeKey, destination :'NodeKey, graph :  Directed.FContextMap<'NodeKey, 'NodeData, float>) =
        //TODO: Implement Dijkstra.ofFContextMapBetween
        System.NotImplementedException() |> raise

    static member computeBetween (origin : 'NodeKey, destination :'NodeKey,  graph :  Directed.LilMatrix<'NodeKey, 'NodeData, float>) =
        //TODO: Implement Dijkstra.ofLilMatrixBetween
        System.NotImplementedException() |> raise

