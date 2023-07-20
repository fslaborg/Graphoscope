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
    /// Creates an empty graph
    let create<'Node,'EdgeData when 'Node: equality> () =
        {
            IdMap = Dictionary<'Node, int>()
            Nodes = ResizeArray<'Node>()
            OutEdges = ResizeArray<ResizeArray<(int * 'EdgeData)>>()
            // InEdges = ResizeArray<ResizeArray<(int * float)>>()
        }

    [<RequireQualifiedAccess>]
    module Nodes =
        /// adds a new node to the graph
        let add (g: DiGraph<'Node,'EdgeData>) (node: 'Node) =
            // TODO: Check if node exists
            g.IdMap.Add(node, g.Nodes.Count)
            g.Nodes.Add node
            g.OutEdges.Add (ResizeArray())
            // g.InEdges.Add (ResizeArray())

        let addMany (g: DiGraph<'Node,'EdgeData>) (nodes: 'Node []) =
            nodes |> Array.iter (add g)
    
    [<RequireQualifiedAccess>]
    module Edges =
        /// adds a new edge to the graph
        let add (g: DiGraph<'Node,'EdgeData>) (edge: ('Node * 'Node * 'EdgeData)) = 
            // TODO: Check if orig and dest nodes exist
            // TODO: Check if edge already exists
            let orig, dest, attr = edge
            g.OutEdges[g.IdMap[orig]].Add(g.IdMap[dest], attr)
            // g.InEdges[g.IdMap[dest]].Add(g.IdMap[orig], attr)

        let addMany (g: DiGraph<'Node,'EdgeData>) (edges: ('Node * 'Node * 'EdgeData) []) =
            edges |> Array.iter (add g)

        /// returns all outbound edges 
        let getOutEdges (g: DiGraph<'Node,'EdgeData>) (orig: 'Node) =
            g.OutEdges[g.IdMap[orig]]

    // let getInEdges (dest: 'Node) (g: DiGraph<'Node>) =
    //     g.InEdges[g.IdMap[dest]]

        ///Lookup a labeled edge in the graph. Raising KeyNotFoundException if no binding exists in the graph.
        let find (v1:'Node) (v2:'Node) (g : DiGraph<'Node, 'EdgeData>) : 'Node * 'Node * 'EdgeData =
                let k2 = g.IdMap[v2]
                g.OutEdges[g.IdMap[v1]]
                |> ResizeArray.find (fun (k,l) -> k=k2)
                |> fun (_,l) -> v1, v2, l
        
        /// Normailizes weights of outboard links from each node.
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

        /// Returns all possible edges in a digraph, including self-loops.
        let internal getAllPossibleEdges (g: DiGraph<'Node,'EdgeData>) =
            g.Nodes
            |> Seq.allPairs g.Nodes

        /// Returns all possible edges in a digraph, excluding self-loops.
        let internal getNonLoopingPossibleEdges (g: DiGraph<'Node,'EdgeData>) =
            getAllPossibleEdges g
            |> Seq.filter(fun (n1, n2) -> n1 <> n2)

    module Constructors =
        let createFromNodes (nodes: 'Node []) =
            let g = create<'Node,'EdgeData>()
            nodes |> Array.iter (Nodes.add g)
            g

        let createFromEdges (edges: ('Node * 'Node * float)[]) =
            let g = 
                edges
                |> Array.map(fun (n1, n2, _) -> n1, n2)
                |> Array.unzip
                |> fun (a1, a2) -> Array.append a1 a2
                |> Array.distinct
                |> Array.sort
                |> createFromNodes

            edges |> Array.iter (Edges.add g)
            g

                
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

    module Generators =
        /// Generates a complete digraph of size `n`.
        /// EdgeData is set to `1.0`.
        let complete (n: int) =
            let nodes = [|0 .. n - 1|]
            let g  = Constructors.createFromNodes nodes
            
            Edges.getNonLoopingPossibleEdges g
            |> Seq.map(fun (n1, n2) -> (n1,n2,1.))
            |> Array.ofSeq
            |> (Edges.addMany g)
            g

        let randomGnp (rng: System.Random) (n: int) (p: float) =
            let g = Constructors.createFromNodes [|0 .. n - 1|]
            Edges.getNonLoopingPossibleEdges g
            |> Seq.iter( fun (o, d) ->
                if rng.NextDouble() <= p then
                    Edges.add g (o, d, 1.0)
            )
            g




    