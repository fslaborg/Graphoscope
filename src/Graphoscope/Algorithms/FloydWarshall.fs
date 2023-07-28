namespace Graphoscope.Algorithms

open FSharpAux
open Graphoscope
open System.Collections.Generic

/// <summary> 
/// Computes all-pairs shortest paths for a given graph using Floyd-Warshall algorithm.
///
/// The ordered array of nodes and 2D Array of distances where each
/// row and column index corresponds to a node's index in the nodes array.
/// </summary>
type FloydWarshall() =

    /// <summary> 
    /// Computes all-pairs shortest paths for <paramref name="graph"/> using Floyd-Warshall algorithm.
    /// </summary>
    /// <param name="graph">The graph for which to compute the shortest path.</param>
    /// <remarks>If there isn't a path between two edges, the distance is set to `infinity`.</remarks>
    /// <returns>
    /// The ordered array of nodes and 2D Array of distances where each
    /// row and column index corresponds to a node's index in the nodes array.
    /// </returns>        
    static member fromJaggedArray (graph: float [][]): float [][] =
        let count = graph.Length
        let arr = 
            JaggedArray.init count count (fun n m ->
                    let c = graph[n][m]
                    if c = 0. && n <> m then
                        infinity
                    elif n = m then
                        0.
                    else
                        c 
                    )
        
        for k in 0 .. arr.Length - 1 do
            for i in 0 .. arr.Length - 1 do
                for j in 0 .. arr.Length - 1 do
                    let w = arr[i][j]
                    let newW = arr[i][k] + arr[k][j]
                    if w > newW then
                        arr[i][j] <- newW
        arr

    /// <summary> 
    /// Computes all-pairs shortest paths for <paramref name="graph"/> using Floyd-Warshall algorithm.
    /// </summary>
    /// <param name="graph">The graph for which to compute the shortest path.</param>
    /// <remarks>If there isn't a path between two edges, the distance is set to `infinity`.</remarks>
    /// <returns>
    /// The ordered array of nodes and 2D Array of distances where each
    /// row and column index corresponds to a node's index in the nodes array.
    /// </returns>        
    static member fromArray2D (graph: float [,]) : float [,] =
        let count = graph.Length
        let arr = 
            Array2D.init count count (fun n m ->
                    let c = graph[n, m]
                    if c = 0. && n <> m then
                        infinity
                    elif n = m then
                        0.
                    else
                        c 
                    )
        
        for k in 0 .. arr.Length - 1 do
            for i in 0 .. arr.Length - 1 do
                for j in 0 .. arr.Length - 1 do
                    let w = arr[i, j]
                    let newW = arr[i, k] + arr[k, j]
                    if w > newW then
                        arr[i, j] <- newW
        arr

    static member compute() = 42


