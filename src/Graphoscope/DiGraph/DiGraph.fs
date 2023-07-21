namespace Graphoscope.DiGraph

//open FSharpx.Collections
open FSharpAux
open System.Collections.Generic

(*** hide ***)
type DiGraph<'Node, 'EdgeData when 'Node: equality and 'Node: comparison>() = 
    let idMap = Dictionary<'Node,int>()
    let nodes = ResizeArray<'Node>() 
    let outEdges = ResizeArray<ResizeArray<(int * 'EdgeData)>>()
    // InEdges: ResizeArray<ResizeArray<(int * float)>>

    member internal _.IdMap: Dictionary<'Node, int> = idMap
    member internal _.Nodes: ResizeArray<'Node> = nodes
    member internal _.OutEdges: ResizeArray<ResizeArray<(int * 'EdgeData)>> = outEdges

[<AutoOpen>]
module Operations =
    /// <summary> 
    /// Create a new empty directed graph with nodes and edges of the specified type.
    /// The type specified for the nodes must support equality operations. 
    /// Edge data can be used to specify weights of edges or other edge labels. 
    /// </summary>
    /// <returns>A graph of the specified type</returns>
    let create<'Node,'EdgeData when 'Node: equality and 'Node: comparison> () : DiGraph<'Node, 'EdgeData>=
        DiGraph()

    /// <summary> 
    /// Adds a new node to the graph
    /// </summary>
    /// <param name="node">The node to be created. The type must match the node type of the graph.</param> 
    /// /// <param name="graph">The graph the node will be added to.</param> 
    /// /// <returns>Unit</returns>
    let addNode (graph: DiGraph<'Node,'EdgeData>) (node: 'Node) =
        // TODO: Check if node exists
        graph.IdMap.Add(node, graph.Nodes.Count * 1)
        graph.Nodes.Add node
        graph.OutEdges.Add (ResizeArray())
        // g.InEdges.Add (ResizeArray())

    let addManyNodes (graph: DiGraph<'Node,'EdgeData>) (nodes: 'Node []) =
        nodes |> Array.iter (addNode graph)

    /// <summary> 
    /// Removes a node from the graph
    /// </summary>
    /// <param name="node">The node to be removed.</param> 
    /// <param name="graph">The graph the edge will be removed from.</param> 
    /// <returns>Unit</returns>
    let removeNodes (graph: DiGraph<'Node,'EdgeData>) (node: 'Node) = 
        let nodeIx = graph.IdMap[node]

        graph.OutEdges
        |> ResizeArray.iteri(fun ri r ->
            r
            |> ResizeArray.mapi(fun ci (target, _) -> if target = nodeIx then Some ci else None)
            |> ResizeArray.choose id
            |> ResizeArray.rev
            |> ResizeArray.iter(fun x -> graph.OutEdges[ri].RemoveAt x)
        )

        graph.IdMap.Remove node |> ignore
        graph.Nodes.RemoveAt nodeIx
        graph.OutEdges.RemoveAt nodeIx

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
    let addEdge (graph: DiGraph<'Node,'EdgeData>) (edge: ('Node * 'Node * 'EdgeData)) =
        // TODO: Check if orig and dest nodes exist
        // TODO: Check if edge already exists
        let orig, dest, attr = edge
        graph.OutEdges[graph.IdMap[orig]].Add(graph.IdMap[dest], attr)
        // g.InEdges[g.IdMap[dest]].Add(g.IdMap[orig], attr)

    /// <summary> 
    /// Returns the outbound edges for given node
    /// </summary>
    /// <param name="origin">The node from which the edges start</param> 
    /// <param name="graph">The graph the node is present in</param> 
    /// <returns>An array of target nodes and the corresponding 'EdgeData.</returns>
    let getOutEdges (graph: DiGraph<'Node,'EdgeData>) (origin: 'Node): ('Node * 'EdgeData) []=
        graph.OutEdges[graph.IdMap[origin]]
        |> Seq.map(fun (t, w) -> graph.Nodes[t], w)
        |> Array.ofSeq

    
    /// <summary> 
    /// Adds many edges to a graph at once
    /// </summary>
    /// <param name="edges">The array of edges. Each edge is a three part tuple containing the origin node, the destination node, and any edge label such as the weight.</param> 
    /// <param name="graph">The graph to add the edge to</param> 
    /// <returns>Unit</returns>
    let addManyEdges (graph: DiGraph<'Node,'EdgeData>) (edges: ('Node * 'Node * 'EdgeData) []) =
        edges |> Array.iter (addEdge graph)

    // let getInEdges (dest: 'Node) (g: DiGraph<'Node>) =
    //     g.InEdges[g.IdMap[dest]]

    /// <summary> 
    /// Tries to find an edge between the specified nodes. Raises KeyNotFoundException if no such edge exists in the graph.
    /// </summary>
    /// <param name="origin">The starting node of the edge</param> 
    /// <param name="destination">The target node of the edge</param> 
    /// <param name="graph">The graph to find the edge in</param> 
    /// <returns>A edge as a three part tuple of origin node, the destination node, and any edge label such as the weight.</returns>
    let findEdge (origin:'Node) (destination:'Node) (graph : DiGraph<'Node, 'EdgeData>) : 'Node * 'Node * 'EdgeData =
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
    let normalizeOutEdges (graph: DiGraph<'Node,float>) = // should this return the graph?
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
    let removeEdge (graph: DiGraph<'Node,'EdgeData>) (edge: ('Node * 'Node)) = 
        let orig, dest = edge
        let ix = graph.OutEdges[graph.IdMap[orig]] |> ResizeArray.tryFindIndex(fun (n, _) -> n = graph.IdMap[dest])
        match ix with
        | Some n -> graph.OutEdges[graph.IdMap[orig]].RemoveAt n
        | None -> printfn $"Edge to be removed doesn't exist: {edge}"
        
        // g.InEdges[g.IdMap[dest]]...

    /// Returns all possible edges in a digraph, including self-loops.
    let internal getAllPossibleEdges (graph: DiGraph<'Node,'EdgeData>) =
        graph.Nodes
        |> Seq.allPairs graph.Nodes

    /// Returns all possible edges in a digraph, excluding self-loops.
    let internal getNonLoopingPossibleEdges (graph: DiGraph<'Node,'EdgeData>) =
        getAllPossibleEdges graph
        |> Seq.filter(fun (n1, n2) -> n1 <> n2)

module Constructors =  
    /// <summary> 
    /// Builds a graph from a list of nodes. 
    /// The edges will then need to be added
    /// </summary>
    /// <param name="nodes">An array of nodes. The type of the nodes will strongly type the created graph to use that type for all nodes.</param> 
    /// <returns>A graph containing the nodes</returns>
    let createFromNodes<'Node,'EdgeData when 'Node: equality and 'Node: comparison> (nodes: 'Node []) : DiGraph<'Node, 'EdgeData> =
        let g = create()
        nodes |> Array.iter (addNode g)
        g

    /// <summary> 
    /// Builds a graph from a list of edges. 
    /// </summary>
    /// <param name="edges">An array of edges. Each edge is  a three part tuple of origin node, the destination node, and any edge label such as the weight</param> 
    /// <returns>A graph containing the nodes</returns>
    let createFromEdges (edges: ('Node * 'Node * 'EdgeData)[]) : DiGraph<'Node, 'EdgeData> =
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
    /// Converts the DiGraph to an Adjacency Matrix
    /// This is preliminary step in many graph algorithms such as Floyd-Warshall. 
    /// The operation assumes edge data types of float in the graph.
    /// </summary>
    /// <param name="graph">The graph to be converted</param> 
    /// <returns>An adjacency matrix</returns>
    let toAdjacencyMatrix (graph: DiGraph<'Node, float>) =
        let matrix = Array.init graph.Nodes.Count (fun _ -> Array.init graph.Nodes.Count (fun _ -> 0.))
        graph.OutEdges
        |> ResizeArray.iteri(fun ri r ->
            r
            |> ResizeArray.iter(fun c ->
                matrix[ri][fst c] <- snd c
            )
        )
        matrix

[<RequireQualifiedAccess>]
module Measures = 
    /// <summary> 
    /// Get the mean degree of the graph. 
    /// This is an undirected measure so inbound links add to a nodes degree.
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the mean degree</returns>
    let getMeanDegree (graph : DiGraph<'Node, 'EdgeData>)  = 
        graph.OutEdges
        |> ResizeArray.map(fun n -> (n |> ResizeArray.length) * 2 |> float)
        |> ResizeArray.toArray
        |> Array.average
    
    /// <summary> 
    /// Gets the total number of edges of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the total edges</returns>
    let getVolume(graph : DiGraph<'Node, 'EdgeData>)  = 
        graph.OutEdges 
        |> ResizeArray.map(fun n -> n |> ResizeArray.length |> float)
        |> ResizeArray.toArray
        |> Array.sum
        |> fun v -> (v|> float) 

    /// <summary> 
    /// Gets the total number of nodes of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the total nodes</returns>
    let getSize (graph : DiGraph<'Node, 'EdgeData>) = 
        graph.Nodes  |> ResizeArray.length
    
    /// <summary> 
    /// Returns the degree distribution of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float array of degree values</returns>
    let getDegreeDistribution (graph : DiGraph<'Node, 'EdgeData>) = 
        graph.OutEdges 
        |> ResizeArray.map(fun n -> n |> ResizeArray.length |> float)
        |> ResizeArray.toArray

module Generators =
    /// <summary> 
    /// Generates a complete digraph of size `n`.
    /// EdgeData is set to `1.0`. This in effect looks like an unweighted graph.
    /// </summary>
    /// <param name="n">The number of nodes in the created graph</param> 
    /// <returns>A directed graph with integer type nodes and float typed edges with value 1.0</returns>
    let complete (n: int): DiGraph<int, float> =
        let g  = Constructors.createFromNodes [|0 .. n - 1|]
        
        getNonLoopingPossibleEdges g
        |> Seq.map(fun (n1, n2) -> (n1, n2, 1.))
        |> Array.ofSeq
        |> (addManyEdges g)
        g

    /// <summary> 
    /// Generates a random digraph of size `n`.
    /// It will create a graph of specified size and edge density. 
    /// </summary>
    /// <param name="n">The number of nodes in the created graph</param> 
    /// <param name="rng">A random number generator</param> 
    /// <param name="p">TThe probability of an edge existing between a pair of nodes. Higher values will create more densely connected graphs.</param> 
    /// <returns>A random directed graph with integer type nodes and float typed edges with value 1.0</returns>
    let randomGnp (rng: System.Random) (n: int) (p: float) = 
        let g = Constructors.createFromNodes [|0 .. n - 1|]
        getNonLoopingPossibleEdges g
        |> Seq.iter( fun (o, d) ->
            if rng.NextDouble() <= p then
                addEdge g (o, d, 1.0)
        )
        g
