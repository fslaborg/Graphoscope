namespace Graphoscope.RandomModels

open Graphoscope

// The Bollobás-Riordan method is a procedure for generating random graphs based on a given degree sequence.
type BollobasRiordan() =
    
    /// <summary> 
    /// Returns a randomly generated, directed, scale free FGraph, based on the given paramters.
    /// </summary>
    /// <param name="n">Number of nodes in graph, integer.</param>
    /// <param name="alpha">Probability for adding a new node connected to an existing node chosen randomly according to the in-degree distribution, float</param>
    /// <param name="beta">Probability for adding an edge between two existing nodes. One existing node is chosen randomly according the in-degree distribution and the other chosen randomly according to the out-degree distribution,float</param>
    /// <param name="gamma">Probability for adding a new node connected to an existing node chosen randomly according to the out-degree distribution, float</param>
    /// <param name="delta_in">Bias for choosing nodes from in-degree distribution, float</param>
    /// <param name="delta_out">Bias for choosing nodes from out-degree distribution, float</param>
    /// <param name="create_using">Basis for the graph generation</param>
    /// <remarks>If the given graph has less than 3 vertices, a hard-coded example is used instead. The sum of <paramref name="alpha"/>, <paramref name="beta"/>, and <paramref name="gamma"/> must be 1.</remarks>
    /// <returns>An FGraph</returns>
    static member initDirectedFGraph (n: int) (alpha: float) (beta: float) (gamma: float) (delta_in: float) (delta_out: float) (create_using:FGraph<int, int, float>) =
        
        if alpha+beta+gamma <> 1. then 
            failwithf "The sum of alpha, beta, and gamma must be 1., but here is %A" (alpha+beta+gamma)
    
        if alpha <= 0. then 
               failwith "alpha must be > 0."

        if beta <= 0. then
            failwith "beta must be > 0."

        if gamma <= 0. then
            failwith "gamma must be > 0."      
        
        let G: FGraph<int, int, float> = 
            if create_using.Count < 3 then 
                FGraph.empty
                |> FGraph.addNodes [|(0,0);(1,1);(2,2)|]
                |> FGraph.addEdges [|(0, 1, 1.); (1, 2, 1.); (2, 0, 1.)|]
            else
                create_using
    
        let rnd = new System.Random()
         
        let _choose_node(distribution, delta, psum) =
            let mutable cumsum = 0.0
            let r = rnd.NextDouble()
            let mutable n = 0
            let mutable threshold = false
            if distribution = "in" then
                while (not threshold) do 
                    let d = G.Item n|> FContext.inwardDegree|> float
                    cumsum <- (cumsum)+((d+delta) / psum)
                    if r < cumsum then
                        threshold <- true
                    else
                        n <- n+1      
                n
            elif distribution = "out" then
                while (not threshold) do
                    let d =  G.Item n|> FContext.outwardDegree |> float
                    cumsum <- (cumsum)+((d+delta) / psum)
                    if r < cumsum then
                        threshold <- true
                    else
                        n <- n+1     
                n
            else
                failwith "ERROR"
        
        while G.Count < n do
            let psum_in     = float (FGraph.countEdges G) + delta_in  *  float (G.Count)
            let psum_out    = float (FGraph.countEdges G) + delta_out * float (G.Count)
            let r           = rnd.NextDouble()
    
            if r < alpha then
                let v = (G.Count)
                let w = _choose_node("in",delta_in,psum_in)
                FGraph.addNode v v G
                |> FGraph.addEdge v w 1. 
                |> ignore

            elif r < (alpha + beta) then
                let v = _choose_node("out",delta_out,psum_out)
                let w = _choose_node("in", delta_in, psum_in)
                match FGraph.containsEdge v w G with
                |false -> 
                    FGraph.addEdge v w 1. G
                    |> ignore
                |true -> ()

    
            else
                let v = _choose_node("out",delta_out,psum_out)
                let w = G.Count

                FGraph.addNode w w G
                |> FGraph.addEdge v w 1. 
                |> ignore

        G
    

    /// <summary> 
    /// Returns a randomly generated, directed, scale free DiGraph, based on the given paramters.
    /// </summary>
    /// <param name="n">Number of nodes in graph, integer.</param>
    /// <param name="alpha">Probability for adding a new node connected to an existing node chosen randomly according to the in-degree distribution, float</param>
    /// <param name="beta">Probability for adding an edge between two existing nodes. One existing node is chosen randomly according the in-degree distribution and the other chosen randomly according to the out-degree distribution,float</param>
    /// <param name="gamma">Probability for adding a new node connected to an existing node chosen randomly according to the out-degree distribution, float</param>
    /// <param name="delta_in">Bias for choosing nodes from in-degree distribution, float</param>
    /// <param name="delta_out">Bias for choosing nodes from out-degree distribution, float</param>
    /// <param name="create_using">Basis for the graph generation</param>
    /// <remarks>If the given graph has less than 3 vertices, a hard-coded example is used instead. The sum of <paramref name="alpha"/>, <paramref name="beta"/>, and <paramref name="gamma"/> must be 1.</remarks>
    /// <returns>A DiGraph</returns>
    static member initDirectedDiGraph (n: int) (alpha: float) (beta: float) (gamma: float) (delta_in: float) (delta_out: float) (create_using:DiGraph<'NodeKey, 'EdgeData>) =
        failwith"not implemented yet"




    /// <summary> 
    /// Returns a randomly generated, directed, scale free FGraph, based on the given paramters.
    /// </summary>
    /// <param name="n">Number of nodes in graph, integer.</param>
    /// <param name="alpha">Probability for adding a new node connected to an existing node chosen randomly according to the in-degree distribution, float</param>
    /// <param name="beta">Probability for adding an edge between two existing nodes. One existing node is chosen randomly according the in-degree distribution and the other chosen randomly according to the out-degree distribution,float</param>
    /// <param name="gamma">Probability for adding a new node connected to an existing node chosen randomly according to the out-degree distribution, float</param>
    /// <param name="delta_in">Bias for choosing nodes from in-degree distribution, float</param>
    /// <param name="delta_out">Bias for choosing nodes from out-degree distribution, float</param>
    /// <param name="create_using">Basis for the graph generation</param>
    /// <remarks>If the given graph has less than 3 vertices, a hard-coded example is used instead. The sum of <paramref name="alpha"/>, <paramref name="beta"/>, and <paramref name="gamma"/> must be 1.</remarks>
    /// <returns>An FGraph</returns>
    static member initDirected ((n: int),(alpha: float),(beta: float),(gamma: float),(delta_in: float),(delta_out: float),(create_using:FGraph<int, int, float>)) =
        BollobasRiordan.initDirectedFGraph n alpha beta gamma delta_in delta_out create_using

    /// <summary> 
    /// Returns a randomly generated, directed, scale free DiGraph, based on the given paramters.
    /// </summary>
    /// <param name="n">Number of nodes in graph, integer.</param>
    /// <param name="alpha">Probability for adding a new node connected to an existing node chosen randomly according to the in-degree distribution, float</param>
    /// <param name="beta">Probability for adding an edge between two existing nodes. One existing node is chosen randomly according the in-degree distribution and the other chosen randomly according to the out-degree distribution,float</param>
    /// <param name="gamma">Probability for adding a new node connected to an existing node chosen randomly according to the out-degree distribution, float</param>
    /// <param name="delta_in">Bias for choosing nodes from in-degree distribution, float</param>
    /// <param name="delta_out">Bias for choosing nodes from out-degree distribution, float</param>
    /// <param name="create_using">Basis for the graph generation</param>
    /// <remarks>If the given graph has less than 3 vertices, a hard-coded example is used instead. The sum of <paramref name="alpha"/>, <paramref name="beta"/>, and <paramref name="gamma"/> must be 1.</remarks>
    /// <returns>A DiGraph</returns>
    static member initDirected ((n: int),(alpha: float),(beta: float),(gamma: float),(delta_in: float),(delta_out: float),(create_using:DiGraph<'NodeKey, 'EdgeData>)) =
        BollobasRiordan.initDirectedDiGraph n alpha beta gamma delta_in delta_out create_using
    