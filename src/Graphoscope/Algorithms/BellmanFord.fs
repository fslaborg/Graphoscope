namespace Graphoscope.Algorithms

open Graphoscope
open System.Collections.Generic


/// <summary> 
/// Computes BellmanFord shortest path
/// </summary>
type BellmanFord() =


    static member ofFGraph (starting: 'NodeKey) (graph :  FGraph<'NodeKey, 'NodeData, float>) =
        let nodesCount = graph.Count
        let distances = new Dictionary<'NodeKey, float>()

        // Step 1: Initialize distances from src to all
        // other vertices as INFINITE
        // Initialize distances to infinity for all nodes except the starting node
        for nodeKey in graph.Keys do
            if nodeKey = starting then
                distances.[nodeKey] <- 0
            else
                distances.[nodeKey] <- System.Double.MaxValue
 
        // Step 2: Relax all edges |V| - 1 times. A simple
        // shortest path from src to any other vertex can
        // have at-most |V| - 1 edges
        for nkv in graph do
            let (_,_,p) = nkv.Value 
            for ekv in p do 
                if (distances[nkv.Key] <> System.Double.MaxValue
                    && distances[nkv.Key] + ekv.Value < distances[ekv.Key]) then
                    distances[ekv.Key] <- distances[nkv.Key] + ekv.Value
        
        distances


