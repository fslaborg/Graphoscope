namespace Graphoscope.Algorithms

open Graphoscope
open System.Collections.Generic




    /// <summary> 
    /// Depth-First Traversal (or Search)
    /// </summary>
    type DFS() =

        static member private searchDiGraph 
            (starting : 'NodeKey) 
            (edgeFinder: 'NodeKey -> DiGraph<'NodeKey,'NodeData,'EdgeData> -> ('NodeKey * 'EdgeData) array)
            (graph : DiGraph<'NodeKey, 'NodeData, 'EdgeData>) =

            let visited = HashSet<'NodeKey>()
            let stack = Stack<'NodeKey>()

            stack.Push(starting)
            visited.Add(starting) |> ignore

            seq {
                while stack.Count > 0 do
                    let nodeKey = stack.Pop()      
                    let sucessors = edgeFinder nodeKey graph   
                    let nodeData = DiGraph.Node.getNodeData nodeKey graph  
                    
                    yield (nodeKey, nodeData)

                    for (key,_) in sucessors do
                        if not(visited.Contains(key)) then
                            stack.Push(key)
                            visited.Add(key) |> ignore
            }

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

        /// Traverses nodes reachable from given node in a Depth-First Traversal (or Search)
        /// </summary>
        /// <param name="starting">Nodekey for starting the BFS traversal.</param> 
        /// <param name="graph">The graph to traverse.</param> 
        /// <returns>Sequence of node key and node data</returns>
        static member ofDiGraph (starting : 'NodeKey) (graph : DiGraph<'NodeKey, 'NodeData, 'EdgeData>) =
            DFS.searchDiGraph starting (DiGraph.getOutEdges) graph 
        
        /// Same as ofDiGraph except it traverses nodes along in and out edges. 
        /// This is useful for finding components and other operations where the direction of the edge shouldnt matter.
        /// </summary>
        /// <param name="starting">Nodekey for starting the BFS traversal.</param> 
        /// <param name="graph">The graph to traverse.</param> 
        /// <returns>Sequence of node key and node data</returns>
        static member ofDiGraphUndirected (starting : 'NodeKey) (graph : DiGraph<'NodeKey, 'NodeData, 'EdgeData>) =

            let undirectedEdgeFinder (starting : 'NodeKey) (graph : DiGraph<'NodeKey, 'NodeData, 'EdgeData>) = 
                DiGraph.getOutEdges starting graph
                |> Array.append(DiGraph.getInEdges starting graph)
         
            DFS.searchDiGraph starting undirectedEdgeFinder graph 
        
    


