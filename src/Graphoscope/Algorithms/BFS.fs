namespace Graphoscope.Algorithms

open Graphoscope
open System.Collections.Generic


/// <summary> 
/// Breadth-First Traversal (or Search)
/// </summary>
type BFS() =
    
    /// <summary> 
    /// Traverses nodes reachable from given node in a Breadth-First Traversal (or Search)
    /// </summary>
    /// <param name="starting">Nodekey for starting the BFS traversal.</param> 
    /// <param name="graph">The graph to traverse.</param> 
    /// <returns>Sequence of node key and node data</returns>
    static member ofFGraph (starting : 'NodeKey) (graph : FGraph<'NodeKey, 'NodeData, 'EdgeData>) =
        let visited = HashSet<'NodeKey>()
        let queue = Queue<'NodeKey>()

        queue.Enqueue(starting)
        visited.Add(starting) |> ignore
        seq {
            while queue.Count > 0 do
                let nodeKey = queue.Dequeue()            
                let (_,nd,s) = graph.[nodeKey]
                yield (nodeKey, nd)
                for kv in s do
                    if not(visited.Contains(kv.Key)) then
                        queue.Enqueue(kv.Key)
                        visited.Add(kv.Key) |> ignore
        }  


    ///// <summary> 
    ///// Traverses nodes reachable from given node in a Breadth-First Traversal (or Search)
    ///// </summary>
    ///// <param name="starting">Nodekey for starting the BFS traversal.</param> 
    ///// <param name="graph">The graph to traverse.</param> 
    ///// <returns>Sequence of node key and adjacent components</returns>
    //static member ofAdjGraph (starting : 'NodeKey) (graph : AdjGraph<'NodeKey, 'NodeData, 'EdgeData>) =
    //    let visited = HashSet<'NodeKey>()
    //    let queue = Queue<'NodeKey>()

    //    queue.Enqueue(starting)
    //    visited.Add(starting) |> ignore
    //    seq {
    //        while queue.Count > 0 do
    //            let nodeKey = queue.Dequeue()
    //            let node, adjComponent = graph.[nodeKey]
    //            yield (nodeKey, adjComponent)
    //            for kv in adjComponent do
    //                if not(visited.Contains(kv.Key)) then
    //                    queue.Enqueue(kv.Key)
    //                    visited.Add(kv.Key) |> ignore
    //    }    

    /// <summary> 
    /// Traverses nodes reachable from given node in a Breadth-First Traversal (or Search)
    /// </summary>
    /// <param name="starting">Nodekey for starting the BFS traversal.</param> 
    /// <param name="graph">The graph to traverse.</param> 
    /// <returns>Sequence of node key and node data</returns>
    static member Compute (starting : 'NodeKey, graph : FGraph<'NodeKey, 'NodeData, 'EdgeData>) = 
        // in this overloads type conversion is OK
        BFS.ofFGraph starting graph



    /// <summary> 
    /// Traverses nodes reachable from given node in a Breadth-First Traversal (or Search)
    /// </summary>
    /// <param name="starting">Nodekey for starting the BFS traversal.</param> 
    /// <param name="graph">The graph to traverse.</param> 
    /// <returns>Sequence of node key and node data</returns>    
    static member Compute (starting : 'NodeKey, graph : DiGraph<'NodeKey, 'NodeData, 'EdgeData>) = 
        // in this overloads type conversion is OK
        System.NotImplementedException() |> raise
