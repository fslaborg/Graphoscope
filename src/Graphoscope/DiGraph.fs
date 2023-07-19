﻿namespace Graphoscope

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
        let add (node: 'Node) (g: DiGraph<'Node,'NodeData>) =
            // TODO: Check if node exists
            g.IdMap.Add(node, g.Nodes.Count)
            g.Nodes.Add node
            g.OutEdges.Add (ResizeArray())
            // g.InEdges.Add (ResizeArray())
    
    [<RequireQualifiedAccess>]
    module Edges =
        /// adds a new edge to the graph
        let add (edge: ('Node * 'Node * 'EdgeData)) (g: DiGraph<'Node,'EdgeData>) = 
            // TODO: Check if orig and dest nodes exist
            // TODO: Check if edge already exists
            let orig, dest, attr = edge
            g.OutEdges[g.IdMap[orig]].Add(g.IdMap[dest], attr)
            // g.InEdges[g.IdMap[dest]].Add(g.IdMap[orig], attr)

        /// returns all outbound edges 
        let getOutEdges (orig: 'Node) (g: DiGraph<'Node,'EdgeData>) =
            g.OutEdges[g.IdMap[orig]]

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