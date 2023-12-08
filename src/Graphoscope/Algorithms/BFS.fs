namespace Graphoscope.Algorithms

open Graphoscope.Graphs
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
    static member ofFContextMap (starting : 'NodeKey) (graph : Directed.FContextMap<'NodeKey, 'NodeData, 'EdgeData>) =
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

    /// <summary> 
    /// Traverses nodes reachable from given node in a Breadth-First Traversal (or Search)
    /// </summary>
    /// <param name="starting">Nodekey for starting the BFS traversal.</param> 
    /// <param name="graph">The graph to traverse.</param> 
    /// <returns>Sequence of node key and node data</returns>
    static member OfUndirectedFContextMap (starting : 'NodeKey) (undirectedGraph : Graphoscope.Graphs.Undirected.FContextMapU<'NodeKey, 'NodeData, 'EdgeData>) =
        let visited = HashSet<'NodeKey>()
        let queue = Queue<'NodeKey>()

        queue.Enqueue(starting)
        visited.Add(starting) |> ignore
        seq {
            while queue.Count > 0 do
                let nodeKey = queue.Dequeue()            
                let (_,nd,s) = undirectedGraph.[nodeKey]
                yield (nodeKey, nd)
                for kv in s do
                    if not(visited.Contains(kv.Key)) then
                        queue.Enqueue(kv.Key)
                        visited.Add(kv.Key) |> ignore               
        }  

    /// <summary> 
    /// Traverses nodes reachable from given node in a Breadth-First Traversal (or Search)
    /// </summary>
    /// <param name="starting">Nodekey for starting the BFS traversal.</param> 
    /// <param name="graph">The graph to traverse.</param> 
    /// <returns>Sequence of node key and node data</returns>
    static member Compute(starting : 'NodeKey, graph : Directed.FContextMap<'NodeKey, 'NodeData, 'EdgeData>) = 
        // in this overloads type conversion is OK
        BFS.ofFContextMap starting graph

    /// <summary> 
    /// Traverses nodes reachable from given node in a Breadth-First Traversal (or Search)
    /// </summary>
    /// <param name="starting">Nodekey for starting the BFS traversal.</param> 
    /// <param name="graph">The graph to traverse.</param> 
    /// <returns>Sequence of node key and node data</returns>
    static member ComputeUndirected(starting : 'NodeKey, graph : Undirected.FContextMapU<'NodeKey, 'NodeData, 'EdgeData'>) = 
        // in this overloads type conversion is OK
        BFS.OfUndirectedFContextMap starting graph


    /// <summary> 
    /// Traverses nodes reachable from given node in a Breadth-First Traversal (or Search)
    /// </summary>
    /// <param name="starting">Nodekey for starting the BFS traversal.</param> 
    /// <param name="graph">The graph to traverse.</param> 
    /// <returns>Sequence of node key and node data</returns>    
    static member Compute (starting : 'NodeKey, graph : Directed.LilMatrix<'NodeKey, 'NodeData, 'EdgeData>) = 
        // in this overloads type conversion is OK
        System.NotImplementedException() |> raise
