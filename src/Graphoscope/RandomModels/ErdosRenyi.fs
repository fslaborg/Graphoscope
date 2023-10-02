namespace Graphoscope.RandomModels

open System
open Graphoscope.Graph

type ErdosRenyi() =


    /// <summary> 
    /// Returns a Erdos-Renyi graph https://en.wikipedia.org/wiki/Erd%C5%91s%E2%80%93R%C3%A9nyi_model
    /// </summary>
    /// <param name="nodes">Specifies the number of nodes</param>
    /// <param name="edges">Specifies the number of edges</param>

    /// <returns>An UndirectedGraph</returns>
    /// 
    static member init (nodes:int) (edges:int) = 
        let  rng = System.Random()
        let g = 
            UndirectedGraph.empty
            |> UndirectedGraph.addNodes (Array.init nodes id)
            
        let createRandomEdge (nodeCount: int)=
                let f = rng.Next(nodeCount)
                let s = rng.Next(nodeCount)
                Math.Min(f,s), Math.Max(f,s)

        Seq.initInfinite(fun _ -> createRandomEdge nodes)
        |> Seq.filter(fun (o,t) -> o<>t)
        |> Seq.distinct
        |> Seq.take edges
        |> Seq.map(fun (f,t)-> f,t,1.0)
        |> Seq.toArray
        |> fun e -> UndirectedGraph.addEdges e g