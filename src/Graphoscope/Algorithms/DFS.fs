namespace Graphoscope.Algorithms

open Graphoscope
open System.Collections.Generic




    /// <summary> 
    /// Depth-First Traversal (or Search).
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
        /// Traverses nodes reachable from given node in a Depth-First Traversal (or Search).
        /// </summary>
        /// <param name="starting">Nodekey for starting the DFS traversal.</param> 
        /// <param name="graph">The graph to traverse.</param> 
        /// <returns>Sequence of node key and node data.</returns>
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

        /// <summary> 
        /// Traverses nodes reachable from given node in a Depth-First Traversal (or Search) while using a predicate function.
        /// </summary>
        /// <param name="starting">Nodekey for starting the DFS traversal.</param> 
        /// <param name="predicate">A function applied to the NodeKeys, NodeData, and EdgeData of a traversed node. Nodes are only collected if the function returns true.</param>
        /// <param name="graph">The graph to traverse.</param> 
        /// <returns>Sequence of node key and node data traversed where predicate returned true.</returns>
        /// <remarks>This function does not collect all traversable nodes via DFS and filters them but stops the traversal for a given node as soon as predicate returns false and continues with the next node not yet visited. This means that you may lose subgraphs with nodes that would return when applied to predicate.</remarks>
        static member ofFGraphBy (starting : 'NodeKey) (predicate : 'NodeKey -> 'NodeData -> 'EdgeData -> bool) (graph : FGraph<'NodeKey, 'NodeData, 'EdgeData>) =
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
                        let _, ndSuccessor, _ = graph[kv.Key]
                        if not (visited.Contains(kv.Key)) && predicate nodeKey ndSuccessor s[nodeKey] then
                            stack.Push(kv.Key)
                            visited.Add(kv.Key) |> ignore
            }

        /// <summary> 
        /// Traverses nodes reachable from given node in a Depth-First Traversal (or Search) while using a predicate function.
        /// </summary>
        /// <param name="starting">Nodekey for starting the DFS traversal.</param> 
        /// <param name="predicate">A function applied to the NodeKeys, NodeData, and EdgeData of a traversed node. Nodes are only traversed if the function returns true.</param>
        /// <param name="graph">The graph to traverse.</param> 
        /// <returns>Sequence of node key and node data where predicate returned true.</returns>
        static member ofFGraphWithDepth (starting : 'NodeKey) depth (graph : FGraph<'NodeKey, 'NodeData, 'EdgeData>) =
            let visited = HashSet<'NodeKey>()
            let stack = Stack<'NodeKey * int>()

            stack.Push(starting,0)
            visited.Add(starting) |> ignore

            seq {
                while stack.Count > 0 do
                    let nodeKey, currDepth = stack.Pop()
                    let (_, nd, s) = graph.[nodeKey]
                    yield (nodeKey, nd)

                    if currDepth < depth then
                        for kv in s do
                            if not (visited.Contains(kv.Key)) then
                                stack.Push(kv.Key, currDepth + 1)
                                visited.Add(kv.Key) |> ignore
            }

        static member ofFGraphWithDepthBy (starting : 'NodeKey) depth (predicate : 'NodeKey -> 'NodeData -> 'EdgeData -> bool) (graph : FGraph<'NodeKey, 'NodeData, 'EdgeData>) =
            let visited = HashSet<'NodeKey>()
            let stack = Stack<'NodeKey * int>()

            stack.Push(starting,0)
            visited.Add(starting) |> ignore

            seq {
                while stack.Count > 0 do
                    let nodeKey, currDepth = stack.Pop()
                    let (_, nd, s) = graph.[nodeKey]
                    yield (nodeKey, nd)

                    if currDepth < depth then
                        for kv in s do
                            let _, ndSuccessor, _ = graph[kv.Key]
                            if not( visited.Contains(kv.Key)) && predicate kv.Key nd s[kv.Key] then
                                stack.Push(kv.Key, currDepth + 1)
                                visited.Add(kv.Key) |> ignore
            }

        /// Traverses nodes reachable from given node in a Depth-First Traversal (or Search).
        /// </summary>
        /// <param name="starting">Nodekey for starting the DFS traversal.</param> 
        /// <param name="graph">The graph to traverse.</param> 
        /// <returns>Sequence of node key and node data.</returns>
        static member ofDiGraph (starting : 'NodeKey) (graph : DiGraph<'NodeKey, 'NodeData, 'EdgeData>) =
            DFS.searchDiGraph starting (DiGraph.getOutEdges) graph 
        
        /// Same as ofDiGraph except it traverses nodes along in and out edges. 
        /// This is useful for finding components and other operations where the direction of the edge shouldn't matter.
        /// </summary>
        /// <param name="starting">Nodekey for starting the DFS traversal.</param> 
        /// <param name="graph">The graph to traverse.</param> 
        /// <returns>Sequence of node key and node data.</returns>
        static member ofDiGraphUndirected (starting : 'NodeKey) (graph : DiGraph<'NodeKey, 'NodeData, 'EdgeData>) =

            let undirectedEdgeFinder (starting : 'NodeKey) (graph : DiGraph<'NodeKey, 'NodeData, 'EdgeData>) = 
                DiGraph.getOutEdges starting graph
                |> Array.append(DiGraph.getInEdges starting graph)

            DFS.searchDiGraph starting undirectedEdgeFinder graph 


