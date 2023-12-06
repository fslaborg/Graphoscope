namespace Graphoscope.RandomModels

open Graphoscope
open Graphoscope.Graphs

// Adaptation of the gilbert random plane networks
 // Gilbert, E.N., 1961. Random plane networks. Journal of the society for industrial and applied mathematics, 9(4), pp.533-543.
 /// Returns an ArrayAdjacencyGraph that is generated randomly with the given parameters.
 ///
 /// numberOfNodes indicates the number of vertices the final graph will have. 
 /// probability represents the probability of an edge between 2 vertices.   
type Gilbert() =
    
    
    /// <summary> 
    /// Inits a directed AdjGraph according to the gilbert random plane networks
    /// </summary>
    static member initDirectedAdjGraph (numberOfNodes: int) (probability: float) =
         if probability > 1. || probability < 0. then failwithf "The stated probability %F is outside the expected range of 0. to 1." probability

         let rnd = new System.Random()
         let g   = AdjGraph.empty

         for i=0 to (numberOfNodes-1) do          
             for ii=0 to (numberOfNodes-1) do
                 if rnd.NextDouble() < probability then
                     g |> AdjGraph.addElement i i ii ii probability  |> ignore
         g

    /// <summary> 
    /// Inits a directed FContextMap according to the gilbert random plane networks
    /// </summary>
    static member initDirectedFContextMap (numberOfNodes: int) (probability: float) =
        if probability > 1. || probability < 0. then failwithf "The stated probability %F is outside the expected range of 0. to 1." probability

        let rnd = new System.Random()
        let g   :  Directed.FContextMap<int, int, float> = Directed.FContextMap.empty

        for i=0 to (numberOfNodes-1) do          
            for ii=0 to (numberOfNodes-1) do
                if rnd.NextDouble() < probability then
                    g |> Directed.FContextMap.addElement i i ii ii probability |> ignore
        g

    /// <summary> 
    /// Inits an undirected FContextMap according to the gilbert random plane networks
    /// </summary>
    static member initUndirectedFContextMap (numberOfNodes: int) (probability: float) =
        if probability > 1. || probability < 0. then failwithf "The stated probability %F is outside the expected range of 0. to 1." probability

        let rnd = new System.Random()
        let g   :  Directed.FContextMap<int, int, float> = Directed.FContextMap.empty

        for i=0 to (numberOfNodes-1) do          
            for ii=i to (numberOfNodes-1) do
                if rnd.NextDouble() < probability then
                    g |> Directed.FContextMap.addElement i i ii ii probability |> ignore
        g       

    /// <summary> 
    /// Inits an UndirectedGraph according to the gilbert random plane networks
    /// </summary>
    static member initUndirectedGraph (rng: System.Random) (numberOfNodes: int) (probability: float) =
        if probability > 1. || probability < 0. then
            failwithf "The stated probability %F is outside the expected range of 0. to 1." probability

        let g = Undirected.UndirectedGraph.createFromNodes (Array.init numberOfNodes (fun i -> i, i))
        Undirected.UndirectedGraph.getNonLoopingPossibleEdges g
        |> Seq.iter( fun (o, d) ->
            if rng.NextDouble() <= probability then
                Undirected.UndirectedGraph.addEdge (o, d, 1.0) g |> ignore
        )
        g

    /// <summary> 
    /// Inits a LilMatrix according to the gilbert random plane networks
    /// </summary>
    static member initLilMatrix (rng: System.Random) (numberOfNodes: int) (probability: float) = 
        if probability > 1. || probability < 0. then
            failwithf "The stated probability %F is outside the expected range of 0. to 1." probability

        let g = Directed.LilMatrix.createFromNodes ([|0 .. numberOfNodes - 1|] |> Array.zip [|0 .. numberOfNodes - 1|])
        Directed.LilMatrix.getNonLoopingPossibleEdges g
        |> Seq.iter( fun (o, d) ->
            if rng.NextDouble() <= probability then
                Directed.LilMatrix.addEdge (o, d, 1.0) g |> ignore
        )
        g
