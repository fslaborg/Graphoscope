namespace Graphoscope.Algorithms

open Graphoscope
open System.Collections.Generic


/// <summary> 
/// Depth-First Traversal (or Search)
/// </summary>
type DFS() =
    
    /// <summary> 
    /// Traverses nodes reachable from given node in a Depth-First Traversal (or Search)
    /// </summary>
    /// <param name="starting">Nodekey for starting the BFS traversal.</param> 
    /// <param name="graph">The graph to traverse.</param> 
    /// <returns>Sequence of node key and node data</returns>
    static member ofFGraph (starting : 'NodeKey) (graph : FGraph<'NodeKey, 'NodeData, 'EdgeData>) =
        let visited = HashSet<'NodeKey>()
        let stack = Stack<'NodeKey>()

        stack.Push(starting)
        visited.Add(starting) |> ignore

        seq {
            while stack.Count > 0 do
                let nodeKey = stack.Pop()            
                let (_, nd, s) = graph.[nodeKey]
                yield (nodeKey, nd)

                for kv in s do
                    if not(visited.Contains(kv.Key)) then
                        stack.Push(kv.Key)
                        visited.Add(kv.Key) |> ignore
        }




