namespace Graphoscope

open System.Collections.Generic

type DiGraph<'Node when 'Node: equality> = {
    IdMap: Dictionary<'Node, int>
    Nodes: Vec<'Node>
    OutEdges: Vec<Vec<(int * float)>>
    // InEdges: ResizeArray<ResizeArray<(int * float)>>
}

module DiGraph =
    let create<'Node when 'Node: equality> () =
        {
            IdMap = Dictionary<'Node, int>()
            Nodes = Vec<'Node>()
            OutEdges = Vec<Vec<(int * float)>>()
            // InEdges = ResizeArray<ResizeArray<(int * float)>>()
        }

    let copy (g: DiGraph<'Node>): DiGraph<'Node> = 
        failwith "Not implemented" // Will be useful for immutable operations

    let addNode (node: 'Node) (g: DiGraph<'Node>) =
        // TODO: Check if node exists
        g.IdMap.Add(node, g.Nodes.Length)
        g.Nodes.Add node
        g.OutEdges.Add (new Vec<(int * float)>())
        // g.InEdges.Add (ResizeArray())

    let addEdge (edge: ('Node * 'Node * float)) (g: DiGraph<'Node>) = 
        // TODO: Check if orig and dest nodes exist
        // TODO: Check if edge already exists
        let orig, dest, attr = edge
        g.OutEdges[g.IdMap[orig]].Add((g.IdMap[dest], attr))
        // g.InEdges[g.IdMap[dest]].Add(g.IdMap[orig], attr)

    let getOutEdges (orig: 'Node) (g: DiGraph<'Node>) =
        g.OutEdges[g.IdMap[orig]]

    // let getInEdges (dest: 'Node) (g: DiGraph<'Node>) =
    //     g.InEdges[g.IdMap[dest]]

    let normalizeOutEdges (g: DiGraph<'Node>) =
        for i in 0 .. g.OutEdges.Length - 1 do
            let outEdges = g.OutEdges[i]
            let total = outEdges |> Vec.sumBy snd
            outEdges
            |> Vec.iteri(fun i (dest,weight) -> 
                outEdges[i] <- (dest, weight / total)
            )