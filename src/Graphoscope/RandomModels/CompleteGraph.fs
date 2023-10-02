namespace Graphoscope.RandomModels

open Graphoscope
open Graphoscope.Graph


type CompleteGraph() =

    /// <summary> 
    /// Returns a complete undirected graph. Every node is connected to every node.
    /// </summary>
    /// <param name="numberOfNodes"> specifies how many additional nodes the final graph will have.</param>
    /// <returns>An UndirectedGraph</returns>
    static member init (nodesToCreate: int) = 

        let nodeIds = Array.init nodesToCreate id
        let g = 
            UndirectedGraph.empty   
            |> UndirectedGraph.addNodes (nodeIds |> Array.map(fun i -> i))

        nodeIds |> Seq.allPairs nodeIds
        |> Seq.filter(fun (o,t) -> o<>t)
        |> Seq.map(fun (f,t)-> f,t,1.0)
        |> Seq.toArray
        |> fun e -> UndirectedGraph.addEdges e g
