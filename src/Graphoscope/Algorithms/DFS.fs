namespace Graphoscope.Algorithms

open Graphoscope
open Graphoscope.Graphs
open System.Collections.Generic




    /// <summary> 
    /// Depth-First Traversal (or Search)
    /// </summary>
    type DFS() =

        static member private searchLilMatrix 
            (starting : 'NodeKey) 
            (edgeFinder: 'NodeKey -> Directed.LilMatrix<'NodeKey,'NodeData,'EdgeData> -> ('NodeKey * 'EdgeData) array)
            (graph : Directed.LilMatrix<'NodeKey, 'NodeData, 'EdgeData>) =

            let visited = HashSet<'NodeKey>()
            let stack = Stack<'NodeKey>()

            stack.Push(starting)
            visited.Add(starting) |> ignore

            seq {
                while stack.Count > 0 do
                    let nodeKey = stack.Pop()      
                    let sucessors = edgeFinder nodeKey graph   
                    let nodeData = Directed.LilMatrix.Node.getNodeData nodeKey graph  
                    
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
        static member ofFContextMap (starting : 'NodeKey) (graph : Directed.FContextMap<'NodeKey, 'NodeData, 'EdgeData>) =
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
        static member ofLilMatrix (starting : 'NodeKey) (graph : Directed.LilMatrix<'NodeKey, 'NodeData, 'EdgeData>) =
            DFS.searchLilMatrix starting (Directed.LilMatrix.getOutEdges) graph 
        
        /// Same as ofLilMatrix except it traverses nodes along in and out edges. 
        /// This is useful for finding components and other operations where the direction of the edge shouldnt matter.
        /// </summary>
        /// <param name="starting">Nodekey for starting the BFS traversal.</param> 
        /// <param name="graph">The graph to traverse.</param> 
        /// <returns>Sequence of node key and node data</returns>
        static member ofLilMatrixUndirected (starting : 'NodeKey) (graph : Directed.LilMatrix<'NodeKey, 'NodeData, 'EdgeData>) =

            let undirectedEdgeFinder (starting : 'NodeKey) (graph : Directed.LilMatrix<'NodeKey, 'NodeData, 'EdgeData>) = 
                Directed.LilMatrix.getOutEdges starting graph
                |> Array.append(Directed.LilMatrix.getInEdges starting graph)
         
            DFS.searchLilMatrix starting undirectedEdgeFinder graph 
        
    


