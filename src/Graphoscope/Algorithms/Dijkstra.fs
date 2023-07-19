namespace Graphoscope.Algorithm

open Graphoscope
open FSharpAux
open DiGraph


type Dijkstra() =
    static member getAdjacencyArrayFor (g: DiGraph<'Node, float>) (nodeIx: int) =
        let dist =
            Array.init (Measures.getSize g) (fun x -> if x = nodeIx then 0. else infinity)
        g.OutEdges[nodeIx]
        |> ResizeArray.iter(fun (target, w) -> dist[target] <- w)
        dist

    static member Compute (g: DiGraph<'Node, float>) (source: 'Node): float [] =
        let que= ResizeArray()
        let sourceIx = g.IdMap[source]
        let dist = Dijkstra.getAdjacencyArrayFor g sourceIx

        for n in 0 .. g.Nodes.Count - 1 do
            que.Add(n)

        while que.Count > 0 do
            let minDistNode = 
                que
                |> Seq.minBy( fun n -> dist[n])

            let minDistNodeIx =  que.IndexOf minDistNode
            que.RemoveAt minDistNodeIx

            let minDistAdjacency = Dijkstra.getAdjacencyArrayFor g minDistNode

            for n in que do
                let newCost = dist[minDistNode] + minDistAdjacency[n]
                if newCost < dist[n] then
                    dist[n] <- newCost
        dist
    
    /// Computes all shortest paths in a graph
    static member ComputeAllPairs (g: DiGraph<'Node, float>): float [][] =
        let allDists = Converters.toAdjacencyMatrix g
        allDists
        |> Array.iteri(fun ri r ->
            r
            |> Array.iteri(fun ci c ->
                if c = 0. && ri <> ci then
                    allDists[ri][ci] <- infinity
                elif ri = ci then
                    allDists[ri][ci] <- 0.
            )
        )
        
        let dijkstra (sourceIx: int) =
            let que= ResizeArray()
            let dist = allDists[sourceIx] |> Array.copy

            for n in 0 .. g.Nodes.Count - 1 do
                que.Add(n)

            while que.Count > 0 do
                let minDistNode = 
                    que
                    |> Seq.minBy( fun n -> dist[n])

                let minDistNodeIx =  que.IndexOf minDistNode
                que.RemoveAt minDistNodeIx

                for n in que do
                    let newCost = dist[minDistNode] + allDists[minDistNode][n]
                    if newCost < dist[n] then
                        dist[n] <- newCost
            dist

        [|0 .. g.Nodes.Count - 1|]
        |> Array.Parallel.map dijkstra 
