namespace Graphoscope

//open FSharpx.Collections
open FSharpAux
open System.Collections.Generic
open FSharp.Data
open Graphoscope
open System

type DiGraph<'NodeKey, 'EdgeData when 'NodeKey: equality and 'NodeKey: comparison>() = 
    let idMap = Dictionary<'NodeKey,int>()
    let nodeKeys = ResizeArray<'NodeKey>() 
    let outEdges = ResizeArray<ResizeArray<(int * 'EdgeData)>>()
    let inEdges = ResizeArray<ResizeArray<(int * 'EdgeData)>>()
    // InEdges: ResizeArray<ResizeArray<(int * float)>>

    member internal _.IdMap: Dictionary<'NodeKey, int> = idMap
    member internal _.NodeKeys: ResizeArray<'NodeKey> = nodeKeys
    member internal _.OutEdges: ResizeArray<ResizeArray<(int * 'EdgeData)>> = outEdges
    member internal _.InEdges: ResizeArray<ResizeArray<(int * 'EdgeData)>> = inEdges



module DiGraph =    
    /// <summary> 
    /// Creates an empty DiGraph
    /// </summary>
    /// <returns>Empty DiGraph</returns>
    let empty<'NodeKey, 'EdgeData when 'NodeKey : comparison>
        : DiGraph<'NodeKey, 'EdgeData> =
        DiGraph<'NodeKey, 'EdgeData>()

    
type DiGraph() =

    /// <summary> 
    /// Returns all nodes in te graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An array of nodes</returns>
    static member getNodes (graph: DiGraph<'NodeKey,'EdgeData>) =
        graph.NodeKeys
        |> Array.ofSeq

    /// <summary> 
    /// Adds a new node to the graph
    /// </summary>
    /// <param name="node">The node to be created. The type must match the node type of the graph.</param> 
    /// /// <param name="graph">The graph the node will be added to.</param> 
    /// /// <returns>Unit</returns>
    static member addNode (node: 'NodeKey) (graph: DiGraph<'NodeKey,'EdgeData>) =
        // TODO: Check if node exists
        graph.IdMap.Add(node, graph.NodeKeys.Count * 1)
        graph.NodeKeys.Add node
        graph.OutEdges.Add (ResizeArray())
        graph.InEdges.Add (ResizeArray())

    static member addMany (nodes: 'NodeKey []) (graph: DiGraph<'NodeKey,'EdgeData>) =
        nodes |> Array.iter (fun n -> DiGraph.addNode n graph)

    /// <summary> 
    /// Removes a node from the graph
    /// </summary>
    /// <param name="node">The node to be removed.</param> 
    /// <param name="graph">The graph the edge will be removed from.</param> 
    /// <returns>Unit</returns>
    static member removeNode (node: 'NodeKey) (graph: DiGraph<'NodeKey,'EdgeData>) = 
        let nodeIx = graph.IdMap[node]

        graph.OutEdges
        |> ResizeArray.iteri(fun ri r ->
            r
            |> ResizeArray.mapi(fun ci (target, _) -> if target = nodeIx then Some ci else None)
            |> ResizeArray.choose id
            |> ResizeArray.rev
            |> ResizeArray.iter(fun x -> graph.OutEdges[ri].RemoveAt x)
        )

        // Update IdMap
        graph.IdMap.Remove node |> ignore
        for KeyValue(k,v) in graph.IdMap do
            if v > nodeIx then
                graph.IdMap[k] <- v - 1

        graph.NodeKeys.RemoveAt nodeIx
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

        
    /// <summary> 
    /// Adds a new edge to the graph
    /// </summary>
    /// <param name="edge">The edge to be created. A three part tuple containing the origin node, the destination node, and any edge label such as the weight.</param> 
    /// <param name="graph">The graph the edge will be added to.</param> 
    /// <returns>Unit</returns>
    static member addEdge (edge: ('NodeKey * 'NodeKey * 'EdgeData)) (graph: DiGraph<'NodeKey,'EdgeData>) =
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

    /// <summary> 
    /// Returns the outbound edges for given node
    /// </summary>
    /// <param name="origin">The node from which the edges start</param> 
    /// <param name="graph">The graph the node is present in</param> 
    /// <returns>An array of target nodes and the corresponding 'EdgeData.</returns>
    static member getOutEdges (origin: 'NodeKey) (graph: DiGraph<'NodeKey,'EdgeData>) : ('NodeKey * 'EdgeData) []=
        graph.OutEdges[graph.IdMap[origin]]
        |> Seq.map(fun (t, w) -> graph.NodeKeys[t], w)
        |> Array.ofSeq

    /// <summary> 
    /// Returns the all outbound edges in the graph
    /// </summary>
    /// <param name="graph">The graph the edges are present in</param> 
    /// <returns>An array of origin, destination nodes and the corresponding 'EdgeData tuples.</returns>
    static member getAllEdges (graph: DiGraph<'NodeKey,'EdgeData>): ('NodeKey * 'NodeKey * 'EdgeData) [] =
        DiGraph.getNodes graph
        |> Array.map(fun n ->
            n
            |> (fun n -> DiGraph.getOutEdges n graph)
            |> Array.map(fun (t, w) -> n, t, w)
        )
        |> Array.concat

    /// <summary> 
    /// Returns the outbound edges for given node
    /// </summary>
    /// <param name="origin">The node from which the edges start</param> 
    /// <param name="graph">The graph the node is present in</param> 
    /// <returns>An array of target nodes and the corresponding 'EdgeData.</returns>
    static member getInEdges (graph: DiGraph<'NodeKey,'EdgeData>) (destination: 'NodeKey): ('NodeKey * 'EdgeData) []=
        graph.InEdges[graph.IdMap[destination]]
        |> Seq.map(fun (t, w) -> graph.NodeKeys[t], w)
        |> Array.ofSeq

    /// <summary> 
    /// Adds many edges to a graph at once
    /// </summary>
    /// <param name="edges">The array of edges. Each edge is a three part tuple containing the origin node, the destination node, and any edge label such as the weight.</param> 
    /// <param name="graph">The graph to add the edge to</param> 
    /// <returns>Unit</returns>
    static member addEdges (graph: DiGraph<'NodeKey,'EdgeData>) (edges: ('NodeKey * 'NodeKey * 'EdgeData) []) =
        edges |> Array.iter (fun e -> DiGraph.addEdge e graph)

    // let getInEdges (dest: 'NodeKey) (g: DiGraph<'NodeKey>) =
    //     g.InEdges[g.IdMap[dest]]

    /// <summary> 
    /// Tries to find an edge between the specified nodes. Raises KeyNotFoundException if no such edge exists in the graph.
    /// </summary>
    /// <param name="origin">The starting node of the edge</param> 
    /// <param name="destination">The target node of the edge</param> 
    /// <param name="graph">The graph to find the edge in</param> 
    /// <returns>A edge as a three part tuple of origin node, the destination node, and any edge label such as the weight.</returns>
    static member find (origin:'NodeKey) (destination:'NodeKey) (graph : DiGraph<'NodeKey, 'EdgeData>) : 'NodeKey * 'NodeKey * 'EdgeData =
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
    static member normalizeOutEdges (graph: DiGraph<'NodeKey,float>) = // should this return the graph?
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

    /// <summary> 
    /// Removes an edge to the graph.
    /// </summary>
    /// <param name="edge">The edge to be removed. A two part tuple containing the origin node, the destination node.</param> 
    /// <param name="graph">The graph the edge will be removed from.</param> 
    /// <returns>Unit</returns>
    static member removeEdge (edge: ('NodeKey * 'NodeKey)) (graph: DiGraph<'NodeKey,'EdgeData>)  = 
        let orig, dest = edge
        let ix = graph.OutEdges[graph.IdMap[orig]] |> ResizeArray.tryFindIndex(fun (n, _) -> n = graph.IdMap[dest])
        match ix with
        | Some n -> graph.OutEdges[graph.IdMap[orig]].RemoveAt n
        | None -> printfn $"Edge to be removed doesn't exist: {edge}"
        
        // g.InEdges[g.IdMap[dest]]...

   /// <summary> 
    /// Builds a graph from a list of edges. 
    /// </summary>
    /// <param name="edges">An array of edges. Each edge is  a three part tuple of origin node, the destination node, and any edge label such as the weight</param> 
    /// <returns>A graph containing the nodes</returns>
    static member createFromEdges (edges: ('NodeKey * 'NodeKey * 'EdgeData)[]) : DiGraph<'NodeKey, 'EdgeData> =
        let g = DiGraph.empty
        edges
        |> Array.map(fun (n1, n2, _) -> n1, n2)
        |> Array.unzip
        |> fun (a1, a2) -> Array.append a1 a2
        |> Array.distinct
        |> Array.sort
        |> Array.iter (fun n -> DiGraph.addNode n g)

        edges |> Array.iter (fun e -> DiGraph.addEdge e g)
        g
 

    static member addElement (nk1 : 'NodeKey) (nd1 : 'NodeData) (nk2 : 'NodeKey) (nd2 : 'NodeData) (ed : 'EdgeData) (g : DiGraph<'NodeKey, 'EdgeData>) : DiGraph<'NodeKey, 'EdgeData> =
        if not (g.IdMap.ContainsKey nk1) then
            DiGraph.addNode nk1 g
        if not (g.IdMap.ContainsKey nk2) then
            DiGraph.addNode nk2 g
        
        DiGraph.addEdge (nk1,nk2,ed) g
        g


    /// <summary> 
    /// Builds a graph from a list of nodes. 
    /// The edges will then need to be added
    /// </summary>
    /// <param name="nodes">An array of nodes. The type of the nodes will strongly type the created graph to use that type for all nodes.</param> 
    /// <returns>A graph containing the nodes</returns>
    static member createFromNodes<'NodeKey,'EdgeData when 'NodeKey: equality and 'NodeKey: comparison> (nodes: 'NodeKey []) : DiGraph<'NodeKey, 'EdgeData> =
        let g = DiGraph.empty
        nodes |> Array.iter (fun n -> DiGraph.addNode n g)
        g

   
    /// <summary> 
    /// Converts the DiGraph to an Adjacency Matrix 
    /// The operation assumes edge data types of float in the graph.
    /// </summary>
    /// <param name="graph">The graph to be converted</param> 
    /// <returns>An adjacency matrix</returns>
    static member toMatrix (graph: DiGraph<'NodeKey, float>) =
        let matrix = Array.init graph.NodeKeys.Count (fun _ -> Array.init graph.NodeKeys.Count (fun _ -> 0.))
        graph.OutEdges
        |> ResizeArray.iteri(fun ri r ->
            r
            |> ResizeArray.iter(fun c ->
                matrix[ri][fst c] <- snd c
            )
        )
        matrix

    /// <summary> 
    /// Gets the total number of nodes of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the total nodes</returns>
    static member countNodes (graph: DiGraph<'NodeKey, 'EdgeData>) : int = 
        graph.NodeKeys |> ResizeArray.length
        
    /// <summary> 
    /// Gets the total number of edges of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the total edges</returns>
    static member countEdges (graph: DiGraph<'NodeKey, 'EdgeData>)  :int = 
        DiGraph.getAllEdges graph 
        |> Array.length 
