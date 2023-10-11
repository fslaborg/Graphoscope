namespace Graphoscope.RandomModels


open Graphoscope

type WattsStrogatz() =


            
    /// <summary> 
    /// Returns a Watts-Strogatz graph https://en.wikipedia.org/wiki/Watts%E2%80%93Strogatz_model
    /// </summary>
    /// <param name="n">Specifies the number of nodes</param>
    /// <param name="k">Specifies the degree of the nodes</param>
    /// <param name="p">Specifies the rewiring probability</param>
    /// <returns>An UndirectedGraph</returns>
    /// 
    static member initWattsStrogatz (n: int) (k: int) (p:float) =

        let  rng = System.Random()
        let rec rndExclude (nodeCount: int) (excl: int array) =
        
            let r = rng.Next(nodeCount)

            if excl |> Array.contains r then
                rndExclude nodeCount excl
            else
                r
            
        let g = RegularRingLattice.initUndirectedGraph n k 

        // rewire
        g
        |> UndirectedGraph.getAllEdges
        |> Array.map(fun (f,t, _) -> 
            if rng.NextDouble() < p then 
                UndirectedGraph.removeEdge (f,t) g |> ignore
                let existingTargets = UndirectedGraph.getEdges f g |> Array.map(fun (tar , _)-> tar)
                let excl = existingTargets |> Array.append [|f;t|]
                UndirectedGraph.addEdge (f , (rndExclude n excl), 1.0) g |> ignore
        ) |> ignore

        g   