namespace Graphoscope.Algorithms

open Graphoscope
open Graphoscope.Graphs
open System.Collections.Generic

module private BF =
    
    let rec internal loopEdges (snk:'NodeKey) (adj: byref<Dictionary.Enumerator<'NodeKey, float>>) 
        (distances:Dictionary<'NodeKey, float>) =
        match adj.MoveNext() with
        | true ->
            let weight = adj.Current.Value
            if (distances[snk] <> System.Double.MaxValue
                && distances[snk] + weight < distances[adj.Current.Key]) then
                true
            else
                loopEdges snk (&adj) distances
        | false -> false



/// <summary> 
/// Computes BellmanFord shortest path
/// </summary>
type BellmanFord() =



    static member ofFContextMap (starting: 'NodeKey) (graph :  Directed.FContextMap<'NodeKey, 'NodeData, float>) =
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
 

    static member hasNegativeCycles (graph : Directed.FContextMap<'NodeKey, 'NodeData, float>) (distances:Dictionary<'NodeKey, float>) =
    
        // // Step 3: check for negative-weight cycles. The
        // // above step guarantees shortest distances if graph
        // // doesn't contain negative weight cycle. If we get
        // // a shorter path, then there is a cycle.
        let mutable enGraph = graph.GetEnumerator()
        let rec loopNodes () = //(enGraph: byref<Dictionary.Enumerator<'NodeKey, FContext<'NodeKey, 'NodeData, 'float>>>) 
            //(distances:Dictionary<'NodeKey, float>) =
            match enGraph.MoveNext() with
            | false -> false
            | true -> 
                let nodeKey = enGraph.Current.Key
                let (_,_,p) = enGraph.Current.Value
                let mutable enP = p.GetEnumerator()
                match (BF.loopEdges nodeKey (&enP) distances) with
                | true -> true
                | false -> loopNodes ()

        loopNodes()
    
