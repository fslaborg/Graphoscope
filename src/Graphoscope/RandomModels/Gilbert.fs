namespace Graphoscope.RandomModels

open Graphoscope

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
                     g |> AdjGraph.addElement i i ii ii (i+ii)  |> ignore
         g

    /// <summary> 
    /// Inits a directed FGraph according to the gilbert random plane networks
    /// </summary>
    static member initDirectedFGraph (numberOfNodes: int) (probability: float) =
        if probability > 1. || probability < 0. then failwithf "The stated probability %F is outside the expected range of 0. to 1." probability

        let rnd = new System.Random()
        let g   :  FGraph<int, int, float> = FGraph.empty

        for i=0 to (numberOfNodes-1) do          
            for ii=0 to (numberOfNodes-1) do
                if rnd.NextDouble() < probability then
                    g |> FGraph.addElement i i ii ii probability |> ignore
        g

    /// <summary> 
    /// Inits an undirected FGraph according to the gilbert random plane networks
    /// </summary>
    static member initUndirectedFGraph (numberOfNodes: int) (probability: float) =
        if probability > 1. || probability < 0. then failwithf "The stated probability %F is outside the expected range of 0. to 1." probability

        let rnd = new System.Random()
        let g   :  FGraph<int, int, float> = FGraph.empty

        for i=0 to (numberOfNodes-1) do          
            for ii=i to (numberOfNodes-1) do
                if rnd.NextDouble() < probability then
                    g |> FGraph.addElement i i ii ii probability |> ignore
        g       

    /// <summary> 
    /// Inits an UndirectedGraph according to the gilbert random plane networks
    /// </summary>
    static member initUndirectedGraph (rng: System.Random) (numberOfNodes: int) (probability: float) =
        if probability > 1. || probability < 0. then
            failwithf "The stated probability %F is outside the expected range of 0. to 1." probability

        let g = UndirectedGraph.createFromNodes [|0 .. numberOfNodes - 1|]
        UndirectedGraph.getNonLoopingPossibleEdges g
        |> Seq.iter( fun (o, d) ->
            if rng.NextDouble() <= probability then
                UndirectedGraph.addEdge (o, d, 1.0) g |> ignore
        )
        g

    /// <summary> 
    /// Inits a DiGraph according to the gilbert random plane networks
    /// </summary>
    static member initDiGraph (rng: System.Random) (numberOfNodes: int) (probability: float) = 
        if probability > 1. || probability < 0. then
            failwithf "The stated probability %F is outside the expected range of 0. to 1." probability

        let g = DiGraph.createFromNodes ([|0 .. numberOfNodes - 1|] |> Array.zip [|0 .. numberOfNodes - 1|])
        DiGraph.getNonLoopingPossibleEdges g
        |> Seq.iter( fun (o, d) ->
            if rng.NextDouble() <= probability then
                DiGraph.addEdge (o, d, 1.0) g |> ignore
        )
        g
