namespace Graphoscope

open FSharpAux
open System.Collections.Generic

type UndirectedGraph<'NodeKey, 'NodeData, 'EdgeData when 'NodeKey: equality and 'NodeKey: comparison>() = 
    let idMap = Dictionary<'NodeKey,int>()
    let nodeKeys = ResizeArray<'NodeKey>() 
    let nodeData = ResizeArray<'NodeData>() 
    let edges = ResizeArray<ResizeArray<(int * 'EdgeData)>>()

    member internal _.IdMap: Dictionary<'NodeKey, int> = idMap
    member internal _.NodeKeys: ResizeArray<'NodeKey> = nodeKeys
    member internal _.NodeData: ResizeArray<'NodeData> = nodeData
    member internal _.Edges: ResizeArray<ResizeArray<(int * 'EdgeData)>> = edges


type UndirectedGraph() =
    /// <summary> 
    /// Converts the Graph to an Adjacency Matrix
    /// The operation assumes edge data types of float in the graph.
    /// </summary>
    /// <param name="graph">The graph to be converted</param> 
    /// <returns>An adjacency matrix</returns>
    let toMatrix (graph: UndirectedGraph<_, _, float>) =
        let matrix = Array.init graph.NodeKeys.Count (fun _ -> Array.init graph.NodeKeys.Count (fun _ -> 0.))
        graph.Edges
        |> ResizeArray.iteri(fun ri r ->
            r
            |> ResizeArray.iter(fun c ->
                matrix[ri][fst c] <- snd c
            )
        )
        matrix

    /// <summary> 
    /// Returns all nodes in te graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An array of nodes</returns>
    static member getNodes (graph: UndirectedGraph<'NodeKey, _, _>) =
        graph.NodeKeys
        |> Array.ofSeq

    /// <summary> 
    /// Adds a new node to the graph
    /// </summary>
    /// <param name="node">The node to be created. The type must match the node type of the graph.</param> 
    /// /// <param name="graph">The graph the node will be added to.</param> 
    /// /// <returns>Unit</returns>
    static member addNode (nodeKey: 'NodeKey) (nodeData: 'NodeData) (graph: UndirectedGraph<'NodeKey, 'NodeData, 'EdgeData>) =
        // TODO: Check if node exists
        graph.IdMap.Add(nodeKey, graph.NodeKeys.Count * 1)
        graph.NodeKeys.Add nodeKey
        graph.NodeData.Add nodeData
        graph.Edges.Add (ResizeArray())
        graph

    static member addNodes (nodes: ('NodeKey * 'NodeData) []) (graph: UndirectedGraph<'NodeKey, 'NodeData, 'EdgeData>) =
        nodes 
        |> Array.iter (fun (nk, nd) -> UndirectedGraph.addNode nk nd graph|>ignore)
        graph

    /// <summary> 
    /// Removes a node from the graph
    /// </summary>
    /// <param name="node">The node to be removed.</param> 
    /// <param name="graph">The graph the edge will be removed from.</param> 
    /// <returns>Unit</returns>
    static member removeNode (node: 'NodeKey) (graph: UndirectedGraph<'NodeKey, _, 'EdgeData>) = 
        let nodeIx = graph.IdMap[node]

        graph.Edges
        |> ResizeArray.iteri(fun ri r ->
            r
            |> ResizeArray.mapi(fun ci (target, _) -> if target = nodeIx then Some ci else None)
            |> ResizeArray.choose id
            |> ResizeArray.rev
            |> ResizeArray.iter(fun x -> graph.Edges[ri].RemoveAt x)
        )
        
        // Update IdMap
        graph.IdMap.Remove node |> ignore
        for KeyValue(k,v) in graph.IdMap do
            if v > nodeIx then
                graph.IdMap[k] <- v - 1

        graph.NodeKeys.RemoveAt nodeIx
        graph.NodeData.RemoveAt nodeIx
        graph.Edges.RemoveAt nodeIx

        graph.Edges
        |> ResizeArray.iteri(fun ri r ->
            r
            |> ResizeArray.iteri(fun ci (target, w) ->
                if target > nodeIx then
                    graph.Edges[ri][ci] <- target - 1, w
            )
        )
        graph

    /// <summary> 
    /// Adds a new edge to the graph
    /// </summary>
    /// <param name="edge">The edge to be created. A three part tuple containing the origin node, the destination node, and any edge label such as the weight.</param> 
    /// <param name="graph">The graph the edge will be added to.</param> 
    /// <returns>Unit</returns>
    static member addEdge (edge: ('NodeKey * 'NodeKey * 'EdgeData)) (graph: UndirectedGraph<'NodeKey, _, 'EdgeData>)=
        let orig, dest, attr = edge
        // TODO: add errors for unit returns
        match (graph.IdMap.ContainsKey orig),(graph.IdMap.ContainsKey dest) with
        | true,true ->         
            let origIx = graph.IdMap[orig]
            let destIx = graph.IdMap[dest]
            match graph.Edges[origIx] |> ResizeArray.tryFind(fun (t,_) -> t = destIx) with
            | Some _ -> ()
            | None ->
                graph.Edges[origIx].Add(destIx, attr)
                if orig <> dest then
                    graph.Edges[destIx].Add(origIx, attr)
        |_,_ ->
            ()
        graph

    /// <summary> 
    /// Returns the edges for given node
    /// </summary>
    /// <param name="origin">The node from which the edges start</param> 
    /// <param name="graph">The graph the node is present in</param> 
    /// <returns>An array of target nodes and the corresponding 'EdgeData.</returns>
    static member getEdges (origin: 'NodeKey) (graph: UndirectedGraph<'NodeKey, _, 'EdgeData>) : ('NodeKey * 'EdgeData) []=
        graph.Edges[graph.IdMap[origin]]
        |> Seq.map(fun (t, w) -> graph.NodeKeys[t], w)
        |> Array.ofSeq

    /// <summary> 
    /// Returns the all edges in the graph
    /// </summary>
    /// <param name="graph">The graph the edges are present in</param> 
    /// <returns>An array of origin, destination nodes and the corresponding 'EdgeData tuples.</returns>
    static member getAllEdges (graph: UndirectedGraph<'NodeKey, _, 'EdgeData>) : ('NodeKey * 'NodeKey * 'EdgeData) [] =
        UndirectedGraph.getNodes graph
        |> Array.mapi(fun i n ->
            UndirectedGraph.getEdges n graph
            |> Array.choose(fun (t, w) ->
                if graph.IdMap[t] >= i then
                    Some (n, t, w)
                else None
            )
        )
        |> Array.concat


    /// <summary> 
    /// Adds many edges to a graph at once
    /// </summary>
    /// <param name="edges">The array of edges. Each edge is a three part tuple containing the origin node, the destination node, and any edge label such as the weight.</param> 
    /// <param name="graph">The graph to add the edge to</param> 
    /// <returns>Unit</returns>
    static member addEdges (edges: ('NodeKey * 'NodeKey * 'EdgeData) []) (graph: UndirectedGraph<'NodeKey, _, 'EdgeData>) =
        edges |> Array.iter (fun e -> (UndirectedGraph.addEdge e graph)|>ignore)
        graph

    static member internal getAllPossibleEdges (graph: UndirectedGraph<'NodeKey, _, 'EdgeData>) =
        seq {
            for i in 0 .. graph.NodeKeys.Count - 1 do
                yield!
                    seq{
                        for j in i .. graph.NodeKeys.Count - 1 ->
                            graph.NodeKeys[i],graph.NodeKeys[j]
                    }
        }

    /// Returns all possible edges in a digraph, excluding self-loops.
    static member internal getNonLoopingPossibleEdges (graph: UndirectedGraph<'NodeKey, _, 'EdgeData>) =
        UndirectedGraph.getAllPossibleEdges graph
        |> Seq.filter(fun (n1, n2) -> n1 <> n2)

    /// <summary> 
    /// Tries to find an edge between the specified nodes. Raises KeyNotFoundException if no such edge exists in the graph.
    /// </summary>
    /// <param name="origin">The starting node of the edge</param> 
    /// <param name="destination">The target node of the edge</param> 
    /// <param name="graph">The graph to find the edge in</param> 
    /// <returns>A edge as a three part tuple of origin node, the destination node, and any edge label such as the weight.</returns>
    static member find (origin: 'NodeKey) (destination: 'NodeKey) (graph: UndirectedGraph<'NodeKey, _, 'EdgeData>) : 'NodeKey * 'NodeKey * 'EdgeData =
        let k2 = graph.IdMap[destination]
        graph.Edges[graph.IdMap[origin]]
        |> ResizeArray.find (fun (k,_) -> k=k2)
        |> fun (_, ed) -> origin, destination, ed

    /// <summary> 
    /// Normalises the weights of edges for each node in a graph.
    /// The function assumes that the edge data type of the graph will be float. 
    /// </summary>
    /// <param name="graph">The graph to perform the operation on</param> 
    /// <returns>Unit</returns>
    static member normalizeEdges (graph: UndirectedGraph<'NodeKey, _, float>) = // should this return the graph?
        graph.Edges
        |> ResizeArray.iteri( fun ri edges ->
            let total =
                (0., edges)
                ||> ResizeArray.fold(fun acc c -> acc + snd c)
            edges
            |> ResizeArray.iteri(fun ci (dest,weight) -> 
                graph.Edges[ri][ci] <- (dest, weight / total)
            )
        )
        graph

    /// <summary> 
    /// Removes an edge to the graph.
    /// </summary>
    /// <param name="edge">The edge to be removed. A two part tuple containing the origin node, the destination node.</param> 
    /// <param name="graph">The graph the edge will be removed from.</param> 
    /// <returns>Unit</returns>
    static member removeEdge (edge: ('NodeKey * 'NodeKey)) (graph: UndirectedGraph<'NodeKey, _, 'EdgeData>)  = 
        let orig, dest = edge
        let ixIn = graph.Edges[graph.IdMap[orig]] |> ResizeArray.tryFindIndex(fun (n, _) -> n = graph.IdMap[dest])
        let ixOut = graph.Edges[graph.IdMap[dest]] |> ResizeArray.tryFindIndex(fun (n, _) -> n = graph.IdMap[orig])
        match ixIn, ixOut with
        | Some outE, Some inE -> 
            graph.Edges[graph.IdMap[orig]].RemoveAt outE
            graph.Edges[graph.IdMap[dest]].RemoveAt inE
        | Some _ , None
        | None, Some _ -> failwith "Something in undirected graph edges went horribly wrong."
        | None, None -> failwith $"Edge to be removed doesn't exist: {edge}"
        graph

    static member createFromNodes (nodes: ('NodeKey * 'NodeData) []) : UndirectedGraph<'NodeKey, _, 'EdgeData> =
        let g = UndirectedGraph<'NodeKey,_,'EdgeData>()
        UndirectedGraph.addNodes nodes g
        
    /// <summary> 
    /// Builds a graph from a list of edges. 
    /// This is a shorthand graph generation method
    /// where NodeData is assumed to be the same as NodeKey.
    /// </summary>
    /// <param name="edges">An array of edges. Each edge is a triple of origin node, the destination node, and any edge data such as the weight</param> 
    /// <returns>A graph containing the nodes</returns>
    static member createFromEdges (edges: ('NodeKey * 'NodeKey * 'EdgeData)[]) : UndirectedGraph<'NodeKey, 'NodeKey, 'EdgeData> =
        let g = UndirectedGraph<'NodeKey, 'NodeKey, 'EdgeData>()
        let nodes =
            edges
            |> Array.collect(fun (n1,n2,_)-> [|n1,n1;n2,n2|])
            |> Array.distinct
      
        UndirectedGraph.addNodes nodes g
        |> UndirectedGraph.addEdges edges
        
    static member create ((nodes: ('NodeKey * 'NodeData) []), (edges: ('NodeKey * 'NodeKey * 'EdgeData)[])) =
        nodes
        |> UndirectedGraph.createFromNodes
        |> UndirectedGraph.addEdges edges

    static member addElement (nk1 : 'NodeKey) (nd1 : 'NodeData) (nk2 : 'NodeKey) (nd2 : 'NodeData) (ed : 'EdgeData) (g : UndirectedGraph<'NodeKey, 'NodeData, 'EdgeData>) : UndirectedGraph<'NodeKey, 'NodeData, 'EdgeData> =
        if not (g.IdMap.ContainsKey nk1) then
            UndirectedGraph.addNode nk1 nd1 g|>ignore
        if not (g.IdMap.ContainsKey nk2) then
            UndirectedGraph.addNode nk2 nd1 g|>ignore
        
        UndirectedGraph.addEdge (nk1,nk2,ed) g

    /// <summary> 
    /// Gets the total number of nodes of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the total nodes</returns>
    static member countNodes (graph: UndirectedGraph<'NodeKey, _, _>) : int = 
        graph.NodeKeys |> ResizeArray.length
        
    /// <summary> 
    /// Gets the total number of edges of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the total edges</returns>
    static member countEdges (graph: UndirectedGraph<'NodeKey, _, 'EdgeData>)  :int = 
        UndirectedGraph.getAllEdges graph 
        |> Array.length 
    
    static member ofSeq (edgelist : seq<'NodeKey * 'NodeData * 'NodeKey * 'NodeData * 'EdgeData>) :UndirectedGraph<'NodeKey, _, 'EdgeData> =
        let graph = UndirectedGraph<'NodeKey, _, 'EdgeData>()
        edgelist
        |> Seq.iter (fun (sk,s,tk,t,ed) -> UndirectedGraph.addElement sk s tk t ed graph |> ignore)
        graph

    static member toSeq (graph:UndirectedGraph<'NodeKey, 'NodeData, 'EdgeData>) :seq<'NodeKey * 'NodeData * 'NodeKey * 'NodeData * 'EdgeData> =
        graph.NodeKeys
        |> Seq.mapi(fun i n ->
            graph.Edges[i]
            |> Seq.map(fun (t, w) -> n, graph.NodeData[i], graph.NodeKeys[t], graph.NodeData[t], w)
        )
        |> Seq.concat

    /// <summary> 
    /// Converts the Graph to an Adjacency Matrix
    /// This is preliminary step in many graph algorithms such as Floyd-Warshall. 
    /// The operation assumes edge data types of float in the graph.
    /// </summary>
    /// <param name="graph">The graph to be converted</param> 
    /// <returns>An adjacency matrix</returns>
    static member toAdjacencyMatrix (getEdgeWeight : 'EdgeData -> float) (graph: UndirectedGraph<_, _, 'EdgeData>) =
        let matrix = Array.init graph.NodeKeys.Count (fun _ -> Array.init graph.NodeKeys.Count (fun _ -> 0.))
        graph.Edges
        |> ResizeArray.iteri(fun ri r ->
            r
            |> ResizeArray.iter(fun (ci, v) ->
                matrix[ri][ci] <- getEdgeWeight v
                matrix[ci][ri] <- getEdgeWeight v
            )
        )
        matrix


module UndirectedGraph =

    /// <summary> 
    /// Creates an empty undirected Graph
    /// </summary>
    /// <returns>Empty DiGraph</returns>
    let empty<'NodeKey, 'NodeData, 'EdgeData when 'NodeKey : comparison>
        : UndirectedGraph<'NodeKey, 'NodeData, 'EdgeData> =
        UndirectedGraph<'NodeKey, 'NodeData, 'EdgeData>()

    type Node() =
        /// <summary> 
        /// Adds a new node to the graph
        /// </summary>
        /// <param name="node">The node to be created. The type must match the node type of the graph.</param> 
        /// /// <param name="graph">The graph the node will be added to.</param> 
        /// /// <returns>Unit</returns>
        static member addNode (graph: UndirectedGraph<'NodeKey, 'NodeData, 'EdgeData>) (node: 'NodeKey) =
            UndirectedGraph.addNode node graph

        /// <summary> 
        /// Removes a node from the graph
        /// </summary>
        /// <param name="node">The node to be removed.</param> 
        /// <param name="graph">The graph the edge will be removed from.</param> 
        /// <returns>Unit</returns>
        static member removeNode (graph: UndirectedGraph<'NodeKey, 'NodeData, 'EdgeData>) (node: 'NodeKey) = 
            UndirectedGraph.removeNode node graph

        /// <summary> 
        /// Returns Node Data for a given node from the graph
        /// </summary>
        /// <param name="node">The key of the node node to be returned</param> 
        /// <param name="graph">The graph the node will be returned from.</param> 
        /// <returns>Unit</returns>
        static member getNodeData (node: 'NodeKey)  (graph: UndirectedGraph<'NodeKey, 'NodeData, 'EdgeData>) = 
            graph.NodeData[graph.IdMap[node]]
           
    
    type Edge() =

        /// <summary> 
        /// Adds a new edge to the graph
        /// </summary>
        /// <param name="edge">The edge to be created. A three part tuple containing the origin node, the destination node, and any edge label such as the weight.</param> 
        /// <param name="graph">The graph the edge will be added to.</param> 
        /// <returns>Unit</returns>
        static member addEdge (graph: UndirectedGraph<'NodeKey, 'NodeData, 'EdgeData>)  (edge: ('NodeKey * 'NodeKey * 'EdgeData)) =
            UndirectedGraph.addEdge edge graph

        /// <summary> 
        /// Removes an edge to the graph.
        /// </summary>
        /// <param name="edge">The edge to be removed. A two part tuple containing the origin node, the destination node.</param> 
        /// <param name="graph">The graph the edge will be removed from.</param> 
        /// <returns>Unit</returns>
        static member removeEdge (edge: ('NodeKey * 'NodeKey)) (graph: UndirectedGraph<'NodeKey, 'NodeData, 'EdgeData>)  = 
            UndirectedGraph.removeEdge edge graph