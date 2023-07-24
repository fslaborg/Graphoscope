
namespace Graphoscope.Graph

open FSharpAux
open System.Collections.Generic

type Graph<'NodeKey, 'EdgeData when 'NodeKey: equality and 'NodeKey: comparison>() = 
    let idMap = Dictionary<'NodeKey,int>()
    let nodeKeys = ResizeArray<'NodeKey>() 
    let edges = ResizeArray<ResizeArray<(int * 'EdgeData)>>()

    member internal _.IdMap: Dictionary<'NodeKey, int> = idMap
    member internal _.NodeKeys: ResizeArray<'NodeKey> = nodeKeys
    member internal _.Edges: ResizeArray<ResizeArray<(int * 'EdgeData)>> = edges

[<AutoOpen>]
module Operations =

    /// <summary> 
    /// Returns all nodes in te graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An array of nodes</returns>
    let getNodes (graph: Graph<'NodeKey,'EdgeData>) =
        graph.NodeKeys
        |> Array.ofSeq

    /// <summary> 
    /// Adds a new node to the graph
    /// </summary>
    /// <param name="node">The node to be created. The type must match the node type of the graph.</param> 
    /// /// <param name="graph">The graph the node will be added to.</param> 
    /// /// <returns>Unit</returns>
    let addNode (graph: Graph<'NodeKey,'EdgeData>) (node: 'NodeKey) =
        // TODO: Check if node exists
        graph.IdMap.Add(node, graph.NodeKeys.Count * 1)
        graph.NodeKeys.Add node
        graph.Edges.Add (ResizeArray())

    let addManyNodes (graph: Graph<'NodeKey,'EdgeData>) (nodes: 'NodeKey []) =
        nodes |> Array.iter (addNode graph)

    /// <summary> 
    /// Removes a node from the graph
    /// </summary>
    /// <param name="node">The node to be removed.</param> 
    /// <param name="graph">The graph the edge will be removed from.</param> 
    /// <returns>Unit</returns>
    let removeNode (graph: Graph<'NodeKey,'EdgeData>) (node: 'NodeKey) = 
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
        graph.Edges.RemoveAt nodeIx

        graph.Edges
        |> ResizeArray.iteri(fun ri r ->
            r
            |> ResizeArray.iteri(fun ci (target, w) ->
                if target > nodeIx then
                    graph.Edges[ri][ci] <- target - 1, w
            )
        )
        
    /// <summary> 
    /// Adds a new edge to the graph
    /// </summary>
    /// <param name="edge">The edge to be created. A three part tuple containing the origin node, the destination node, and any edge label such as the weight.</param> 
    /// <param name="graph">The graph the edge will be added to.</param> 
    /// <returns>Unit</returns>
    let addEdge (graph: Graph<'NodeKey,'EdgeData>) (edge: ('NodeKey * 'NodeKey * 'EdgeData)) =
        // TODO: Check if orig and dest nodes exist
        let orig, dest, attr = edge
        let origIx = graph.IdMap[orig]
        let destIx = graph.IdMap[dest]
        match graph.Edges[origIx] |> ResizeArray.tryFind(fun (t,_) -> t = destIx) with
        | Some _ -> failwith $"Edge already exists: ({orig}, {dest})"
        | None ->
            graph.Edges[origIx].Add(destIx, attr)
            if orig <> dest then
                graph.Edges[destIx].Add(origIx, attr)

    /// <summary> 
    /// Returns the edges for given node
    /// </summary>
    /// <param name="origin">The node from which the edges start</param> 
    /// <param name="graph">The graph the node is present in</param> 
    /// <returns>An array of target nodes and the corresponding 'EdgeData.</returns>
    let getEdges (graph: Graph<'NodeKey,'EdgeData>) (origin: 'NodeKey): ('NodeKey * 'EdgeData) []=
        graph.Edges[graph.IdMap[origin]]
        |> Seq.map(fun (t, w) -> graph.NodeKeys[t], w)
        |> Array.ofSeq

    /// <summary> 
    /// Returns the all edges in the graph
    /// </summary>
    /// <param name="graph">The graph the edges are present in</param> 
    /// <returns>An array of origin, destination nodes and the corresponding 'EdgeData tuples.</returns>
    let getAllEdges (graph: Graph<'NodeKey,'EdgeData>): ('NodeKey * 'NodeKey * 'EdgeData) [] =
        getNodes graph
        |> Array.mapi(fun i n ->
            n
            |> (getEdges graph)
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
    let addManyEdges (graph: Graph<'NodeKey,'EdgeData>) (edges: ('NodeKey * 'NodeKey * 'EdgeData) []) =
        edges |> Array.iter (addEdge graph)

    /// <summary> 
    /// Tries to find an edge between the specified nodes. Raises KeyNotFoundException if no such edge exists in the graph.
    /// </summary>
    /// <param name="origin">The starting node of the edge</param> 
    /// <param name="destination">The target node of the edge</param> 
    /// <param name="graph">The graph to find the edge in</param> 
    /// <returns>A edge as a three part tuple of origin node, the destination node, and any edge label such as the weight.</returns>
    let findEdge (origin:'NodeKey) (destination:'NodeKey) (graph : Graph<'NodeKey, 'EdgeData>) : 'NodeKey * 'NodeKey * 'EdgeData =
        let k2 = graph.IdMap[origin]
        graph.Edges[graph.IdMap[origin]]
        |> ResizeArray.find (fun (k,l) -> k=k2)
        |> fun (_,l) -> origin, destination, l
    
    /// <summary> 
    /// Normalises the weights of edges for each node in a graph.
    /// The function assumes that the edge data type of the graph will be float. 
    /// </summary>
    /// <param name="graph">The graph to perform the operation on</param> 
    /// <returns>Unit</returns>
    let normalizeEdges (graph: Graph<'NodeKey,float>) = // should this return the graph?
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

    /// <summary> 
    /// Removes an edge to the graph.
    /// </summary>
    /// <param name="edge">The edge to be removed. A two part tuple containing the origin node, the destination node.</param> 
    /// <param name="graph">The graph the edge will be removed from.</param> 
    /// <returns>Unit</returns>
    let removeEdge (graph: Graph<'NodeKey,'EdgeData>) (edge: ('NodeKey * 'NodeKey)) = 
        let orig, dest = edge
        let ixIn = graph.Edges[graph.IdMap[orig]] |> ResizeArray.tryFindIndex(fun (n, _) -> n = graph.IdMap[dest])
        let ixOut = graph.Edges[graph.IdMap[dest]] |> ResizeArray.tryFindIndex(fun (n, _) -> n = graph.IdMap[orig])
        match ixIn, ixOut with
        | Some outE, Some inE -> 
            graph.Edges[graph.IdMap[orig]].RemoveAt outE
            graph.Edges[graph.IdMap[dest]].RemoveAt inE
        | Some _ , None
        | None, Some _ -> failwith "Something in undirected graph edges went horribly wrong."
        | None, None -> printfn $"Edge to be removed doesn't exist: {edge}"

    /// Returns all possible edges in a Graph, including self-loops.
    let internal getAllPossibleEdges (graph: Graph<'NodeKey,'EdgeData>): ('NodeKey * 'NodeKey) seq =
        seq {
            for i in 0 .. graph.NodeKeys.Count - 1 do 
                for j in i .. 0 .. graph.NodeKeys.Count - 1 ->
                    graph.NodeKeys[i], graph.NodeKeys[j]
        }

    /// Returns all possible edges in a Graph, excluding self-loops.
    let internal getNonLoopingPossibleEdges (graph: Graph<'NodeKey,'EdgeData>): ('NodeKey * 'NodeKey) seq  =
        getAllPossibleEdges graph
        |> Seq.filter(fun (n1, n2) -> n1 <> n2)

module Builders =  
    /// <summary> 
    /// Create a new empty directed graph with nodes and edges of the specified type.
    /// The type specified for the nodes must support equality operations. 
    /// Edge data can be used to specify weights of edges or other edge labels. 
    /// </summary>
    /// <returns>A graph of the specified type</returns>
    let create<'NodeKey,'EdgeData when 'NodeKey: equality and 'NodeKey: comparison> () : Graph<'NodeKey, 'EdgeData>=
        Graph()

    /// <summary> 
    /// Builds a graph from a list of nodes. 
    /// The edges will then need to be added
    /// </summary>
    /// <param name="nodes">An array of nodes. The type of the nodes will strongly type the created graph to use that type for all nodes.</param> 
    /// <returns>A graph containing the nodes</returns>
    let createFromNodes<'NodeKey,'EdgeData when 'NodeKey: equality and 'NodeKey: comparison> (nodes: 'NodeKey []) : Graph<'NodeKey, 'EdgeData> =
        let g = create()
        nodes |> Array.iter (addNode g)
        g

    /// <summary> 
    /// Builds a graph from a list of edges. 
    /// </summary>
    /// <param name="edges">An array of edges. Each edge is  a three part tuple of origin node, the destination node, and any edge label such as the weight</param> 
    /// <returns>A graph containing the nodes</returns>
    let createFromEdges (edges: ('NodeKey * 'NodeKey * 'EdgeData)[]) : Graph<'NodeKey, 'EdgeData> =
        let g = 
            edges
            |> Array.map(fun (n1, n2, _) -> n1, n2)
            |> Array.unzip
            |> fun (a1, a2) -> Array.append a1 a2
            |> Array.distinct
            |> Array.sort
            |> createFromNodes

        edges |> Array.iter (addEdge g)
        g

            
module Converters =
    /// <summary> 
    /// Converts the Graph to an Adjacency Matrix
    /// This is preliminary step in many graph algorithms such as Floyd-Warshall. 
    /// The operation assumes edge data types of float in the graph.
    /// </summary>
    /// <param name="graph">The graph to be converted</param> 
    /// <returns>An adjacency matrix</returns>
    let toAdjacencyMatrix (graph: Graph<'NodeKey, float>) =
        let matrix = Array.init graph.NodeKeys.Count (fun _ -> Array.init graph.NodeKeys.Count (fun _ -> 0.))
        graph.Edges
        |> ResizeArray.iteri(fun ri r ->
            r
            |> ResizeArray.iter(fun (ci, v) ->
                matrix[ri][ci] <- v
                matrix[ci][ri] <- v
            )
        )
        matrix

module Generators =
    /// <summary> 
    /// Generates a complete Graph of size `n`.
    /// EdgeData is set to `1.0`. This in effect looks like an unweighted graph.
    /// </summary>
    /// <param name="n">The number of nodes in the created graph</param> 
    /// <returns>A directed graph with integer type nodes and float typed edges with value 1.0</returns>
    let complete (n: int): Graph<int, float> =
        let g  = Builders.createFromNodes [|0 .. n - 1|]
        
        getNonLoopingPossibleEdges g
        |> Seq.map(fun (n1, n2) -> (n1, n2, 1.))
        |> Array.ofSeq
        |> (addManyEdges g)
        g

    /// <summary> 
    /// Generates a random Graph of size `n`.
    /// It will create a graph of specified size and edge density. 
    /// </summary>
    /// <param name="n">The number of nodes in the created graph</param> 
    /// <param name="rng">A random number generator</param> 
    /// <param name="p">TThe probability of an edge existing between a pair of nodes. Higher values will create more densely connected graphs.</param> 
    /// <returns>A random directed graph with integer type nodes and float typed edges with value 1.0</returns>
    let randomGnp (rng: System.Random) (n: int) (p: float) = 
        let g = Builders.createFromNodes [|0 .. n - 1|]
        getNonLoopingPossibleEdges g
        |> Seq.iter( fun (o, d) ->
            if rng.NextDouble() <= p then
                addEdge g (o, d, 1.0)
        )
        g
