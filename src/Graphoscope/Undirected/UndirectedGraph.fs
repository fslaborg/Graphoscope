
namespace Graphoscope.Undirected

open FSharpx.Collections
open System.Collections.Generic
open Core

module UndirectedGraph =

    let nodesFromEdges(edges: array<'Node * 'Node * float>) = 
        edges
        |> Array.map(fun (orig, _, _) -> orig) 
        |> Array.append (edges |> Array.map(fun (_, dest, _)  -> dest))
        |> Array.distinct
        |> Array.sort
        
    type UndirectedGraph<'Node when 'Node: equality and 'Node : comparison> (nodes : array<'Node>, weightedEdges : array<'Node * 'Node * float>) = 
        //Private mutable fields
        let mutable Nodes: ResizeArray<'Node> = nodes |> ResizeArray 
        
        let mutable IdMap : Dictionary<'Node, int> =
            let d = Dictionary<'Node, int>()
            Nodes 
            |> ResizeArray.mapi(fun i n -> d.Add(n, i))
            |> ignore
            d
            
        let mutable Edges: ResizeArray<ResizeArray<(int * float)>> = 
            Nodes 
            |> ResizeArray.mapi(fun i n -> 
                weightedEdges 
                |> Seq.filter(fun (f,t, w) -> f = n || t = n )
                |> Seq.map(fun (f,t, w) -> 
                    if (f = n) then IdMap[t],1.0 else IdMap[f], 1.0)
                |> ResizeArray)

        //CONSTRUCTORS
        /// Construct graph just from weighted edge list
        new (edges: array<'Node * 'Node * float>) = 
            let nodes = nodesFromEdges edges
            UndirectedGraph(nodes, edges)  
      
        /// Construct graph just from unweighted edge list
        new (edges: array<'Node * 'Node>) = 
            let defaultWeightedEdges = 
                edges
                |> Array.map(fun (f, t) -> f, t, 1.0)

            let nodes = nodesFromEdges defaultWeightedEdges

            UndirectedGraph(nodes, defaultWeightedEdges)  

        ///additional constructor to create empty grpah      
        new () = UndirectedGraph( [||], [||] )
    
        //input validation
        member private this.checkNodeDoesntExist  (node: 'Node) =
            match IdMap.TryFind node with 
            | Some x -> Failure (sprintf "node: %A already exists in the graph" node)
            | None -> Success node

        member private this.checkNodeExists  (node: 'Node) =
            match IdMap.TryFind node with 
            | None  -> Failure (sprintf "node: %A doesnt exist in the graph" node)
            | Some x -> Success node

        member private this.checkEdgeDoesntExist  (edge: 'Node * 'Node ) =
            let i1 = IdMap[(fst edge)]
            let i2 = IdMap[(snd edge)]

            match Edges[i1] |> ResizeArray.exists(fun (t, w) ->t = i2 ),
                    Edges[i2] |> ResizeArray.exists(fun (t, w) ->t = i1 ) with 
            | true,  _ -> Failure (sprintf "edge: %A already exists in the graph" edge)
            | _,  true -> Failure (sprintf "edge: %A already exists in the graph" edge)
            | _ -> Success edge

        //nternal Methods
        member private this.InternalAddEdge (edge: ('Node * 'Node * float)) = 
            let orig, dest, weight  = edge
            
            match   (this.checkNodeExists orig),
                    (this.checkNodeExists dest),
                    (this.checkEdgeDoesntExist (orig, dest)) with
            | _, _, Failure x-> failwith x
            | _, Failure x, _ -> failwith x
            | Failure x, _, _-> failwith x 
            | Success _, Success _, Success _-> 
                let origId = IdMap[orig]
                let destId = IdMap[dest]
                Edges[origId].Add(destId,weight) 

        //GRAPH EDITING METHODS
        member this.AddNode (node: 'Node) =
            match this.checkNodeDoesntExist node with 
            | Success x ->                 
                IdMap.Add(node, Nodes.Count)
                Nodes.Add node
                Edges.Add (ResizeArray())
            | Failure x -> failwith x

        member this.AddEdge(edge: ('Node * 'Node * float)) = 
            this.InternalAddEdge edge

        member this.AddEdge(edge: ('Node * 'Node)) = 
            let orig, dest = edge 
            this.InternalAddEdge (orig, dest, 1.0)

        member this.GetEdges(n : 'Node) = 
            Edges[IdMap[n]] 
            |> ResizeArray.map(fun e -> 
                let dest, weight = e
                n, Nodes[dest], weight)
            |> ResizeArray.toArray
        
        member this.GetEdges() = 
            Edges 
            |> ResizeArray.mapi(fun i n -> 
                n 
                |> ResizeArray.map(fun e -> 
                    let dest, weight = e
                    Nodes[i], Nodes[dest], weight)
                )
            |> ResizeArray.concat
            |> ResizeArray.toArray

        member this.GetNodes() = 
            IdMap.Keys
            |> Seq.toArray

        member this.Degree(n : 'Node) = 
            Edges[IdMap[n]]
            |> ResizeArray.length 
        
        member this.MeanDegree() = 
            Edges
            |> ResizeArray.map(fun n -> n |> ResizeArray.length |> float)
            |> ResizeArray.toArray
            |> Array.average
        
        member this.Volume() = 
            (this.GetEdges() |> Array.length) / 2
        
        member this.Size() = 
            (Nodes |> ResizeArray.length) 

        member this.DegreeDistribution() = 
            Edges
            |> ResizeArray.map(fun n -> n |> ResizeArray.length |> float)
            |> ResizeArray.toArray
          
