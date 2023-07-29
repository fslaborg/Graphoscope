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
                if totalDistance < distance.[kv.Key] then
                    distance.[kv.Key] <- totalDistance
                    priorityQueue.Add((kv.Key, totalDistance)) |> ignore
        

        distance

    /// Computes the shortest path
    static member Compute() = 42.


