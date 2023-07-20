namespace Graphoscope

//open FSharpx.Collections
open FSharpAux
open System.Collections.Generic

type DiGraph<'Node, 'EdgeData when 'Node: equality> = {
    IdMap: Dictionary<'Node, int>
    Nodes: ResizeArray<'Node>
    OutEdges: ResizeArray<ResizeArray<(int * 'EdgeData)>>
    // InEdges: ResizeArray<ResizeArray<(int * float)>>
}

module DiGraph =
    /// <summary> 
    /// Create a new empty directed graph with nodes and edges of the specificed types. 
    /// The type specified for the nodes must support equality operations. 
    /// Edge data can be used to specify weights of edges or other edge labels. 
    /// </summary>
    /// <returns>A graph of the specfied type</returns>
    let create<'Node,'EdgeData when 'Node: equality> () =
        {
            IdMap = Dictionary<'Node, int>()
            Nodes = ResizeArray<'Node>()
            OutEdges = ResizeArray<ResizeArray<(int * 'EdgeData)>>()
            // InEdges = ResizeArray<ResizeArray<(int * float)>>()
        }

    [<RequireQualifiedAccess>]
    module Nodes =
        /// <summary> 
        /// Adds a new node to the graph
        /// </summary>
        /// <param name="node">The node to be created. The type must match the node type of the graph.</param> 
        /// /// <param name="graph">The graph the node will be added to.</param> 
        /// /// <returns>Unit</returns>
        let add (node: 'Node) (graph: DiGraph<'Node,'NodeData>) =
            // TODO: Check if node exists
            graph.IdMap.Add(node, graph.Nodes.Count)
            graph.Nodes.Add node
            graph.OutEdges.Add (ResizeArray())
            // g.InEdges.Add (ResizeArray())
    
    [<RequireQualifiedAccess>]
    module Edges =
        /// <summary> 
        /// Adds a new edge to the graph
        /// </summary>
        /// <param name="edge">The edge to be created. A three part tuple containing the origin node, the detination node, and any edge label such as the weight.</param> 
        /// <param name="graph">The graph the edge will be added to.</param> 
        /// <returns>Unit</returns>
        let add (edge: ('Node * 'Node * 'EdgeData)) (graph: DiGraph<'Node,'EdgeData>) = 
            // TODO: Check if orig and dest nodes exist
            // TODO: Check if edge already exists
            let orig, dest, attr = edge
            graph.OutEdges[g.IdMap[orig]].Add(g.IdMap[dest], attr)
            // g.InEdges[g.IdMap[dest]].Add(g.IdMap[orig], attr)

        /// <summary> 
        /// Returns the outbound edges for given node
        /// </summary>
        /// <param name="origin">The node from which the edges start</param> 
        /// /// <param name="graph">The graph the node is present in</param> 
        /// /// <returns>A mutable (Resize) array </returns>
        let getOutEdges (origin: 'Node) (graph: DiGraph<'Node,'EdgeData>) =
            graph.OutEdges[graph.IdMap[origin]] // should we convert return types to imutable objects?

    // let getInEdges (dest: 'Node) (g: DiGraph<'Node>) =
    //     g.InEdges[g.IdMap[dest]]

        ///Lookup a labeled edge in the graph. Raising KeyNotFoundException if no binding exists in the graph.
        let find (v1:'Node) (v2:'Node) (g : DiGraph<'Node, 'EdgeData>) : 'Node * 'Node * 'EdgeData =
                let k2 = g.IdMap[v2]
                g.OutEdges[g.IdMap[v1]]
                |> ResizeArray.find (fun (k,l) -> k=k2)
                |> fun (_,l) -> v1, v2, l
        
        /// Normailies weights of outboard links from each node.
        let normalizeOutEdges (g: DiGraph<'Node,float>) =
            g.OutEdges
            |> ResizeArray.iter( fun outEdges ->
                let total =
                    (0., outEdges)
                    ||> ResizeArray.fold(fun acc c -> acc + snd c)
                outEdges
                |> ResizeArray.iteri(fun i (dest,weight) -> 
                    outEdges[i] <- (dest, weight / total)
                )
            )
    module Converters =
        /// Converts graph data structure into an Adjacency Matrix
        let toAdjacencyMatrix (g: DiGraph<'Node, float>) =
            let matrix = Array.init g.Nodes.Count (fun _ -> Array.init g.Nodes.Count (fun _ -> 0.))
            g.OutEdges
            |> ResizeArray.iteri(fun ri r ->
                r
                |> ResizeArray.iter(fun c ->
                    matrix[ri][fst c] <- snd c
                )
            )
            matrix

    [<RequireQualifiedAccess>]
    module Measures = 
        ///get the mean degree of the graph. This is an undirected measure so inbound links add to a nodes degree.
        let getMeanDegree (g : DiGraph<'Node, 'EdgeData>)  = 
            g.OutEdges
            |> ResizeArray.map(fun n -> (n |> ResizeArray.length) * 2 |> float)
            |> ResizeArray.toArray
            |> Array.average
        
        /// gets the total number of edges of the graph
        let getVolume(g : DiGraph<'Node, 'EdgeData>)  = 
            g.OutEdges 
            |> ResizeArray.map(fun n -> n |> ResizeArray.length |> float)
            |> ResizeArray.toArray
            |> Array.sum
            |> fun v -> (v|> float) 

        /// gets the total number of nodes of the graph
        let getSize (g : DiGraph<'Node, 'EdgeData>) = 
            g.Nodes  |> ResizeArray.length

        /// returns the degree distribution of the graph
        let getDegreeDistribution (g : DiGraph<'Node, 'EdgeData>) = 
            g.OutEdges 
            |> ResizeArray.map(fun n -> n |> ResizeArray.length |> float)
            |> ResizeArray.toArray