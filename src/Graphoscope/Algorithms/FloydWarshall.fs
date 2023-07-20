namespace Graphoscope.Algorithm


open Graphoscope
open DiGraph

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
                for j in 0 .. adjacencyMatrix.Length - 1 do
                    let w = adjacencyMatrix[i][j]
                    let newW = adjacencyMatrix[i][k] + adjacencyMatrix[k][j]
                    if w > newW then
                        adjacencyMatrix[i][j] <- newW
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
    static member Compute (graph: DiGraph<'Node, float>): 'Node [] * float [][] =
        let adj = graph |> Converters.toAdjacencyMatrix
        graph.Nodes |> Array.ofSeq, FloydWarshall.fromAdjacencyMatrix adj

