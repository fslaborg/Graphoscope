namespace Graphoscope.Algorithm


open Graphoscope

type FloydWarshall() =
    static member fromAdjacencyMatrix (adj: float [][]) =
        adj
        |> Array.iteri(fun ri r->
            r
            |> Array.iteri(fun ci c ->
                if c = 0. && ri <> ci then
                    adj[ri][ci] <- infinity
                elif ri = ci then
                    adj[ri][ci] <- 0.
            )
        )

        for k in 0 .. adj.Length - 1 do
            for i in 0 .. adj.Length - 1 do
                for j in 0 .. adj.Length - 1 do
                    let w = adj[i][j]
                    let newW = adj[i][k] + adj[k][j]
                    if w > newW then
                        adj[i][j] <- newW
        adj

    static member Compute (g: DiGraph<'Node, float>) =
        let adj = g |> DiGraph.toAdjacencyMatrix
        FloydWarshall.fromAdjacencyMatrix adj

    static member Compute (g : FGraph<'NodeKey,'NodeData,float>) =
        let adj = g |> FGraph.Converters.toAdjacencyMatrix
        FloydWarshall.fromAdjacencyMatrix adj