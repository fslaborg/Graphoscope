namespace Graphoscope.Graph

open Graphoscope
open FSharpAux

module Algorithms =

    type Dijkstra() =
        static member internal getAdjacencyArrayFor (graph: Graph<'Node, float>) (nodeIx: int) =
            let dist =
                Array.init (graph.Nodes.Count) (fun x -> if x = nodeIx then 0. else infinity)
            graph.Edges[nodeIx]
            |> ResizeArray.iter(fun (target, w) -> dist[target] <- w)
            dist

        /// <summary> 
        /// Computes shortest paths from <paramref name="source"/> for <paramref name="graph"/> using Dijkstra's algorithm in parallel.
        /// </summary>
        /// <param name="graph"> The graph for which to compute the shortest path.</param>
        /// <param name="source"> Calculate the shortest paths from this node.</param>
        /// <remarks>If there isn't a path between two edges, the distance is set to `infinity`.</remarks>
        /// <returns>Tuples of target node and distance.</returns>
        static member Compute (graph: Graph<'Node, float>) (source: 'Node): ('Node * float) [] =
            let que= ResizeArray()
            let sourceIx = graph.IdMap[source]
            let dist = Dijkstra.getAdjacencyArrayFor graph sourceIx

            for n in 0 .. graph.Nodes.Count - 1 do
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
            |> Array.mapi(fun i x -> graph.Nodes[i], x)
        
        /// <summary> 
        /// Computes all-pairs shortest paths for <paramref name="graph"/> using Dijkstra algorithm in parallel.
        /// </summary>
        /// <param name="graph">The graph for which to compute the shortest paths.</param>
        /// <remarks>If there isn't a path between two edges, the distance is set to `infinity`.</remarks>
        /// <returns>
        /// The ordered array of nodes and 2D Array of distances where each
        /// row and column index corresponds to a node's index in the nodes array.
        /// </returns>
        static member ComputeAllPairs (graph: Graph<'Node, float>): 'Node [] * float [][] =
            let allDists = Converters.toAdjacencyMatrix graph
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

                for n in 0 .. graph.Nodes.Count - 1 do
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

            graph.Nodes |> Array.ofSeq,
            [|0 .. graph.Nodes.Count - 1|]
            |> Array.Parallel.map dijkstra 



    type FloydWarshall() =
        static member private fromAdjacencyMatrix (adjacencyMatrix: float [][]): float [][] =
            adjacencyMatrix
            |> Array.iteri(fun ri r->
                r
                |> Array.iteri(fun ci c ->
                    if c = 0. && ri <> ci then
                        adjacencyMatrix[ri][ci] <- infinity
                    elif ri = ci then
                        adjacencyMatrix[ri][ci] <- 0.
                )
            )

            for k in 0 .. adjacencyMatrix.Length - 1 do
                for i in 0 .. adjacencyMatrix.Length - 1 do
                    for j in i .. adjacencyMatrix.Length - 1 do
                        let w = adjacencyMatrix[i][j]
                        let newW = adjacencyMatrix[i][k] + adjacencyMatrix[k][j]
                        if w > newW then
                            adjacencyMatrix[i][j] <- newW
                            adjacencyMatrix[j][i] <- newW
            adjacencyMatrix

        /// <summary> 
        /// Computes all-pairs shortest paths for <paramref name="graph"/> using Floyd-Warshall algorithm.
        /// </summary>
        /// <param name="graph">The graph for which to compute the shortest path.</param>
        /// <remarks>If there isn't a path between two edges, the distance is set to `infinity`.</remarks>
        /// <returns>
        /// The ordered array of nodes and 2D Array of distances where each
        /// row and column index corresponds to a node's index in the nodes array.
        /// </returns>
        static member Compute (graph: Graph<'Node, float>): 'Node [] * float [][] =
            let adj = graph |> Converters.toAdjacencyMatrix
            graph.Nodes |> Array.ofSeq, FloydWarshall.fromAdjacencyMatrix adj

