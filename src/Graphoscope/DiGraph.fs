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
    let create<'Node,'EdgeData when 'Node: equality> () =
        {
            IdMap = Dictionary<'Node, int>()
            Nodes = ResizeArray<'Node>()
            OutEdges = ResizeArray<ResizeArray<(int * 'EdgeData)>>()
            // InEdges = ResizeArray<ResizeArray<(int * float)>>()
        }

    let copy (g: DiGraph<'Node,'EdgeData>): DiGraph<'Node,'EdgeData> = 
        failwith "Not implemented" // Will be useful for immutable operations

    let addNode (node: 'Node) (g: DiGraph<'Node,'NodeData>) =
        // TODO: Check if node exists
        g.IdMap.Add(node, g.Nodes.Count)
        g.Nodes.Add node
        g.OutEdges.Add (ResizeArray())
        // g.InEdges.Add (ResizeArray())

    let addEdge (edge: ('Node * 'Node * 'EdgeData)) (g: DiGraph<'Node,'EdgeData>) = 
        // TODO: Check if orig and dest nodes exist
        // TODO: Check if edge already exists
        let orig, dest, attr = edge
        g.OutEdges[g.IdMap[orig]].Add(g.IdMap[dest], attr)
        // g.InEdges[g.IdMap[dest]].Add(g.IdMap[orig], attr)

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