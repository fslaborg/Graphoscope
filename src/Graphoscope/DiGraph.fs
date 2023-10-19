namespace Graphoscope

open FSharpAux
open System.Collections.Generic

type DiGraph<'NodeKey, 'NodeData, 'EdgeData when 'NodeKey: equality and 'NodeKey: comparison>() = 
    let idMap = Dictionary<'NodeKey,int>()
    let nodeKeys = ResizeArray<'NodeKey>() 
    let nodeData = ResizeArray<'NodeData>() 
    let outEdges = ResizeArray<ResizeArray<(int * 'EdgeData)>>()
    let inEdges = ResizeArray<ResizeArray<(int * 'EdgeData)>>()

    member internal _.IdMap: Dictionary<'NodeKey, int> = idMap
    member internal _.NodeKeys: ResizeArray<'NodeKey> = nodeKeys
    member internal _.NodeData: ResizeArray<'NodeData> = nodeData
    member internal _.OutEdges: ResizeArray<ResizeArray<(int * 'EdgeData)>> = outEdges
    member internal _.InEdges: ResizeArray<ResizeArray<(int * 'EdgeData)>> = inEdges

type DiGraph() =
    static member internal nodeExists (nodeKey: 'NodeKey) (g : DiGraph<'NodeKey, 'NodeData, 'EdgeData>) = 
        g.IdMap.ContainsKey nodeKey

    /// <summary> 
    /// Returns all nodes in te graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An array of nodes</returns>
    static member getNodes (graph: DiGraph<'NodeKey, 'NodeData, 'EdgeData>) =
        graph.NodeKeys
        |> Array.ofSeq

    /// <summary> 
    /// Adds a new node to the graph
    /// </summary>
    /// <param name="node">The node to be created. The type must match the node type of the graph.</param> 
    /// /// <param name="graph">The graph the node will be added to.</param> 
    /// /// <returns>Unit</returns>
    static member addNode (nodeKey: 'NodeKey) (nodeData: 'NodeData) (graph: DiGraph<'NodeKey, 'NodeData, 'EdgeData>) =
        if DiGraph.nodeExists nodeKey graph then
            failwith $"Node already exists, {nodeKey}."
        else
            graph.IdMap.Add(nodeKey, graph.NodeKeys.Count * 1)
            graph.NodeKeys.Add nodeKey
            graph.NodeData.Add nodeData
            graph.OutEdges.Add (ResizeArray())
            graph.InEdges.Add (ResizeArray())
            graph

    static member addNodes (nodes: ('NodeKey * 'NodeData) []) (graph: DiGraph<'NodeKey, 'NodeData, 'EdgeData>) =
        nodes 
        |> Array.iter (fun (nk, nd) -> DiGraph.addNode nk nd graph|>ignore)
        graph

    /// <summary> 
    /// Removes a node from the graph
    /// </summary>
    /// <param name="node">The node to be removed.</param> 
    /// <param name="graph">The graph the edge will be removed from.</param> 
    /// <returns>Unit</returns>
    static member removeNode (node: 'NodeKey) (graph: DiGraph<'NodeKey, 'NodeData, 'EdgeData>) = 
        let nodeIx = graph.IdMap[node]

        let removeEdges (edges: ResizeArray<ResizeArray<int * 'EdgeData>>) =
            edges
            |> ResizeArray.iteri(fun ri r ->
                r
                |> ResizeArray.mapi(fun ci (target, _) -> if target = nodeIx then Some ci else None)
                |> ResizeArray.choose id
                |> ResizeArray.rev
                |> ResizeArray.iter(fun x -> edges[ri].RemoveAt x)
            )

        removeEdges graph.OutEdges
        removeEdges graph.InEdges

        // Update IdMap
        graph.IdMap.Remove node |> ignore
        for KeyValue(k,v) in graph.IdMap do
            if v > nodeIx then
                graph.IdMap[k] <- v - 1

        graph.NodeKeys.RemoveAt nodeIx
        graph.NodeData.RemoveAt nodeIx
        graph.OutEdges.RemoveAt nodeIx
        graph.InEdges.RemoveAt nodeIx
        
        graph.InEdges
        |> ResizeArray.iteri(fun ri r ->
            r
            |> ResizeArray.iteri(fun ci (origin, w) ->
                if origin > nodeIx then
                    graph.InEdges[ri][ci] <- origin - 1, w
            )
        )

        graph.OutEdges
        |> ResizeArray.iteri(fun ri r ->
            r
            |> ResizeArray.iteri(fun ci (target, w) ->
                if target > nodeIx then
                    graph.OutEdges[ri][ci] <- target - 1, w
            )
        )
        graph
        
    /// <summary> 
    /// Adds a new edge to the graph
    /// </summary>
    /// <param name="edge">The edge to be created. A three part tuple containing the origin node, the destination node, and any edge label such as the weight.</param> 
    /// <param name="graph">The graph the edge will be added to.</param> 
    /// <returns>Unit</returns>
    static member addEdge (edge: ('NodeKey * 'NodeKey * 'EdgeData)) (graph: DiGraph<'NodeKey, 'NodeData, 'EdgeData>) =
        // TODO: Check if orig and dest nodes exist
        let orig, dest, attr = edge
        let origIx = graph.IdMap[orig]
        let destIx = graph.IdMap[dest]
        match graph.OutEdges[origIx] |> ResizeArray.tryFind(fun (t,_) -> t = destIx) with
        | Some _ -> 
            ()
            //failwith $"Edge already exists: ({orig}, {dest})"
        | None ->
            graph.OutEdges[origIx].Add(destIx, attr)
            graph.InEdges[destIx].Add(origIx, attr)
        graph


    /// <summary> 
    /// Returns the outbound edges for given node
    /// </summary>
    /// <param name="origin">The node from which the edges start</param> 
    /// <param name="graph">The graph the node is present in</param> 
    /// <returns>An array of target nodes and the corresponding 'EdgeData.</returns>
    static member getOutEdges (origin: 'NodeKey) (graph: DiGraph<'NodeKey, 'NodeData, 'EdgeData>) : ('NodeKey * 'EdgeData) []=
        graph.OutEdges[graph.IdMap[origin]]
        |> ResizeArray.map(fun (t, w) -> graph.NodeKeys[t], w)
        |> Array.ofSeq

    /// <summary> 
    /// Returns the all outbound edges in the graph
    /// </summary>
    /// <param name="graph">The graph the edges are present in</param> 
    /// <returns>An array of origin, destination nodes and the corresponding 'EdgeData tuples.</returns>
    static member getAllEdges (graph: DiGraph<'NodeKey, 'NodeData, 'EdgeData>): ('NodeKey * 'NodeKey * 'EdgeData) [] =
        DiGraph.getNodes graph
        |> Array.collect(fun n ->
            n
            |> (fun n -> DiGraph.getOutEdges n graph)
            |> Array.map(fun (t, w) -> n, t, w)
        )

    /// <summary> 
    /// Returns the outbound edges for given node
    /// </summary>
    /// <param name="origin">The node from which the edges start</param> 
    /// <param name="graph">The graph the node is present in</param> 
    /// <returns>An array of target nodes and the corresponding 'EdgeData.</returns>
    static member getInEdges (destination: 'NodeKey) (graph: DiGraph<'NodeKey, 'NodeData, 'EdgeData>) : ('NodeKey * 'EdgeData) []=
        graph.InEdges[graph.IdMap[destination]]
        |> ResizeArray.map(fun (t, w) -> graph.NodeKeys[t], w)
        |> Array.ofSeq

    /// <summary> 
    /// Adds many edges to a graph at once
    /// </summary>
    /// <param name="edges">The array of edges. Each edge is a three part tuple containing the origin node, the destination node, and any edge label such as the weight.</param> 
    /// <param name="graph">The graph to add the edge to</param> 
    /// <returns>Unit</returns>
    static member addEdges (edges: ('NodeKey * 'NodeKey * 'EdgeData) []) (graph: DiGraph<'NodeKey, 'NodeData, 'EdgeData>) =
        edges 
        |> Array.iter (fun e -> DiGraph.addEdge e graph|>ignore)
        graph
    // let getInEdges (dest: 'NodeKey) (g: DiGraph<'NodeKey>) =
    //     g.InEdges[g.IdMap[dest]]

    static member internal getAllPossibleEdges (graph: DiGraph<'NodeKey, _, 'EdgeData>) =
        graph.NodeKeys
        |> Seq.allPairs graph.NodeKeys

    /// Returns all possible edges in a digraph, excluding self-loops.
    static member internal getNonLoopingPossibleEdges (graph: DiGraph<'NodeKey, _, 'EdgeData>) =
        DiGraph.getAllPossibleEdges graph
        |> Seq.filter(fun (n1, n2) -> n1 <> n2)

    /// <summary> 
    /// Tries to find an edge between the specified nodes. Raises KeyNotFoundException if no such edge exists in the graph.
    /// </summary>
    /// <param name="origin">The starting node of the edge</param> 
    /// <param name="destination">The target node of the edge</param> 
    /// <param name="graph">The graph to find the edge in</param> 
    /// <returns>A edge as a three part tuple of origin node, the destination node, and any edge label such as the weight.</returns>
    static member find (origin:'NodeKey) (destination:'NodeKey) (graph : DiGraph<'NodeKey, 'NodeData, 'EdgeData>) : 'NodeKey * 'NodeKey * 'EdgeData =
            let k2 = graph.IdMap[origin]
            graph.OutEdges[graph.IdMap[origin]]
            |> ResizeArray.find (fun (k,l) -> k=k2)
            |> fun (_,l) -> origin, destination, l
    
    /// <summary> 
    /// Normalises the weights of outbound edges from each node in a graph.
    /// The function assumes that the edge data type of the graph will be float. 
    /// </summary>
    /// <param name="graph">The graph to perform the operation on</param> 
    /// <returns>Unit</returns>
    static member normalizeOutEdges (graph: DiGraph<'NodeKey, 'NodeData, float>) =
        graph.OutEdges
        |> ResizeArray.iter( fun outEdges ->
            let total =
                (0., outEdges)
                ||> ResizeArray.fold(fun acc c -> acc + snd c)
            outEdges
            |> ResizeArray.iteri(fun i (dest,weight) -> 
                outEdges[i] <- (dest, weight / total)
            )
        )
        graph

    /// <summary> 
    /// Removes an edge to the graph.
    /// </summary>
    /// <param name="edge">The edge to be removed. A two part tuple containing the origin node, the destination node.</param> 
    /// <param name="graph">The graph the edge will be removed from.</param> 
    /// <returns>Unit</returns>
    static member removeEdge (edge: ('NodeKey * 'NodeKey)) (graph: DiGraph<'NodeKey, 'NodeData, 'EdgeData>)  = 
        let orig, dest = edge
        let outIx =
            graph.OutEdges[graph.IdMap[orig]]
            |> ResizeArray.tryFindIndex(fun (n, _) -> n = graph.IdMap[dest])
        match outIx with
        | Some n -> graph.OutEdges[graph.IdMap[orig]].RemoveAt n
        | None -> printfn $"Out Edge to be removed doesn't exist: {edge}"

        let inIx =
            graph.InEdges[graph.IdMap[dest]]
            |> ResizeArray.tryFindIndex(fun (n, _) -> n = graph.IdMap[orig])

        match inIx with
        | Some n -> graph.InEdges[graph.IdMap[dest]].RemoveAt n
        | None -> printfn $"In Edge to be removed doesn't exist: {edge}"

        graph

    /// <summary> 
    /// Builds a graph from a list of edges. 
    /// This is a shorthand graph generation method
    /// where NodeData is assumed to be of the same type as NodeKey.
    /// </summary>
    /// <param name="edges">An array of edges. Each edge is  a three part tuple of origin node, the destination node, and any edge label such as the weight</param> 
    /// <returns>A graph containing the nodes</returns>
    static member createFromEdges (edges: ('NodeKey * 'NodeKey * 'EdgeData)[]) : DiGraph<'NodeKey, 'NodeKey, 'EdgeData> =
        let g = DiGraph<'NodeKey, 'NodeKey, 'EdgeData>()
        edges
        |> Array.collect(fun (n1,n2,_)-> [|n1, n1; n2, n2|])
        |> Array.distinct
        |> fun x -> DiGraph.addNodes x g
        |> DiGraph.addEdges edges

    static member addElement (nk1 : 'NodeKey) (nd1 : 'NodeData) (nk2 : 'NodeKey) (nd2 : 'NodeData) (ed : 'EdgeData) (g : DiGraph<'NodeKey, 'NodeData, 'EdgeData>) : DiGraph<'NodeKey, 'NodeData, 'EdgeData> =
        if not (g |> DiGraph.nodeExists nk1) then
            DiGraph.addNode nk1 nd1 g |>ignore

        if not (g |> DiGraph.nodeExists nk2) then
            DiGraph.addNode nk2 nd2 g |>ignore

        DiGraph.addEdge (nk1,nk2,ed) g//gUpdated

    /// <summary> 
    /// Builds a graph from a list of nodes. 
    /// The edges will then need to be added
    /// </summary>
    /// <param name="nodes">An array of nodes. The type of the nodes will strongly type the created graph to use that type for all nodes.</param> 
    /// <returns>A graph containing the nodes</returns>
    static member createFromNodes<'NodeKey, 'NodeData, 'EdgeData when 'NodeKey: equality and 'NodeKey: comparison> (nodes: ('NodeKey * 'NodeData) []) : DiGraph<'NodeKey, 'NodeData, 'EdgeData> =
        DiGraph<'NodeKey, 'NodeData, 'EdgeData>()
        |>DiGraph.addNodes nodes

    /// <summary> 
    /// Gets the total number of nodes of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the total nodes</returns>
    static member countNodes (graph: DiGraph<'NodeKey, 'NodeData, 'EdgeData>) : int = 
        graph.NodeKeys |> ResizeArray.length
        
    /// <summary> 
    /// Gets the total number of edges of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the total edges</returns>
    static member countEdges (graph: DiGraph<'NodeKey, 'NodeData, 'EdgeData>)  :int = 
        DiGraph.getAllEdges graph 
        |> Array.length 

    static member ofSeq (edgelist : seq<'NodeKey * 'NodeData * 'NodeKey * 'NodeData * 'EdgeData>) :DiGraph<'NodeKey, 'NodeData, 'EdgeData> =
        let graph = DiGraph<'NodeKey, 'NodeData, 'EdgeData>()
        edgelist
        |> Seq.iter (fun (sk,s,tk,t,ed) -> DiGraph.addElement sk s tk t ed graph |> ignore)
        graph

    static member toSeq (graph:DiGraph<'NodeKey, 'NodeData, 'EdgeData>) :seq<'NodeKey * 'NodeKey * 'NodeKey * 'NodeKey * 'EdgeData> =
        graph.NodeKeys
        |> Seq.map(fun n ->
            n
            |> (fun n -> graph.OutEdges[graph.IdMap[n]]|> Seq.map(fun (t, w) -> graph.NodeKeys[t], w))
            |> Seq.map(fun (t, w) -> n, n, t, t,  w)
        )
        |> Seq.concat

    /// <summary> 
    /// Converts the Graph to an Adjacency Matrix
    /// This is preliminary step in many graph algorithms such as Floyd-Warshall. 
    /// The operation assumes edge data types of float in the graph.
    /// </summary>
    /// <param name="graph">The graph to be converted</param> 
    /// <returns>An adjacency matrix</returns>
    static member toAdjacencyMatrix (getEdgeWeight : 'EdgeData -> float) (graph: DiGraph<'NodeKey, _, 'EdgeData>) =
        let matrix = Array.init graph.NodeKeys.Count (fun _ -> Array.init graph.NodeKeys.Count (fun _ -> 0.))
        graph.OutEdges
        |> ResizeArray.iteri(fun ri r ->
            r
            |> ResizeArray.iter(fun (ci, v) ->
                matrix[ri][ci] <- getEdgeWeight v
            )
        )
        matrix


module DiGraph =    
    /// <summary> 
    /// Creates an empty DiGraph
    /// </summary>
    /// <returns>Empty DiGraph</returns>
    let empty<'NodeKey, 'NodeData, 'EdgeData when 'NodeKey : comparison>
        : DiGraph<'NodeKey, 'NodeData, 'EdgeData> =
        DiGraph<'NodeKey, 'NodeData, 'EdgeData>()


    type Node() =
        /// <summary> 
        /// Adds a new node to the graph
        /// </summary>
        /// <param name="node">The node to be created. The type must match the node type of the graph.</param> 
        /// /// <param name="graph">The graph the node will be added to.</param> 
        /// /// <returns>Unit</returns>
        static member addNode (graph: DiGraph<'NodeKey, 'NodeData, 'EdgeData>) (node: 'NodeKey) =
            DiGraph.addNode node graph

        /// <summary> 
        /// Removes a node from the graph
        /// </summary>
        /// <param name="node">The node to be removed.</param> 
        /// <param name="graph">The graph the edge will be removed from.</param> 
        /// <returns>Unit</returns>
        static member removeNode (graph: DiGraph<'NodeKey, 'NodeData, 'EdgeData>) (node: 'NodeKey) = 
            DiGraph.removeNode node graph

        /// <summary> 
        /// Returns Node Data for a given node from the graph
        /// </summary>
        /// <param name="node">The key of the node node to be returned</param> 
        /// <param name="graph">The graph the node will be returned from.</param> 
        /// <returns>Unit</returns>
        static member getNodeData(node: 'NodeKey)  (graph: DiGraph<'NodeKey, 'NodeData, 'EdgeData>) = 
            graph.NodeData[graph.IdMap[node]]
           
    type Edge() =

        /// <summary> 
        /// Adds a new edge to the graph
        /// </summary>
        /// <param name="edge">The edge to be created. A three part tuple containing the origin node, the destination node, and any edge label such as the weight.</param> 
        /// <param name="graph">The graph the edge will be added to.</param> 
        /// <returns>Unit</returns>
        static member addEdge (graph: DiGraph<'NodeKey, 'NodeData, 'EdgeData>)  (edge: ('NodeKey * 'NodeKey * 'EdgeData)) =
            DiGraph.addEdge edge graph

        /// <summary> 
        /// Removes an edge to the graph.
        /// </summary>
        /// <param name="edge">The edge to be removed. A two part tuple containing the origin node, the destination node.</param> 
        /// <param name="graph">The graph the edge will be removed from.</param> 
        /// <returns>Unit</returns>
        static member removeEdge (edge: ('NodeKey * 'NodeKey)) (graph: DiGraph<'NodeKey, 'NodeData, 'EdgeData>)  = 
            DiGraph.removeEdge edge graph
