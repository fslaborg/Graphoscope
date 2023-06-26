namespace ArrayAdjacencyGraph.Model

open Graphoscope.ArrayAdjacencyGraph
open FSharpx.Collections
open System
open System.Collections.Generic


module Model = 
    // Adaption of the Barabási–Albert model for generating random scale-free networks using a preferential attachment mechanism.
    // Barabasi AL, Albert R. Emergence of scaling in random networks. Science. 1999 Oct 15;286(5439):509-12. doi: 10.1126/science.286.5439.509. PMID: 10521342.

    /// Returns an ArrayAdjacencyGraph that was randomly grown according to the Barabási–Albert model with the given parameters. 
    ///
    /// startingGraph is the original graph, that is used as the initial connected network. The rest of the calculations and growth of the network are performed on this graph.
    ///
    /// numberOfVertices specifies how many additional vertices the final graph will have.
    ///
    /// numberOfEdgesPerIteration specifies how many edges should be added to the graph per iteration.
    ///
    /// fVertexKey is a function that is used to transform an integer (the index of the vertex) into the 'Vertex type.  
    ///
    /// fLabel is a function that transforms the 'Vertex type into a label of the 'Label type.   
    ///
    /// fWeight is a funtion that takes two 'Vertices and returns a weight between them in form of an 'Edge type.    
    let barabasiAlbert (startingGraph: ArrayAdjacencyGraph<'Vertex,'Label,'Edge>) (numberOfVertices: int) (numberOfEdgesPerIteration: int) (fVertexKey: int -> 'Vertex) (fLabel: 'Vertex -> 'Label) (fWeight: 'Vertex*'Vertex -> 'Edge) =
        let rnd = new System.Random()
            
        let g = startingGraph

        let oldV = g.VertexCount

        for n=oldV to oldV+numberOfVertices do
            let vertex  = fVertexKey n
            let label   = fLabel vertex
            g.AddVertex(vertex,label)|> ignore
            
            let mutable m = 0

            let p k = ( (float (g.Degree k)) / (float (g.EdgeCount)) )
            let possibleConnections = 
                [|
                    for i in g.GetVertices() do
                        (vertex,i),(p i)
                |]
                |> Array.sortBy snd
            
            let rec getEdges counter m edges =
        
                if m = 0 then
                    g.AddManyEdges edges
                else
                    
                    if counter = possibleConnections.Length then
                        getEdges 0 m edges
                    else
                        let (edge,probability) = possibleConnections.[counter]
                        let r = rnd.NextDouble()
                        if r > probability then
                            getEdges (counter+1) (m) (edges)
                        else
                            let addEdge = fst edge, snd edge,fWeight edge
                            getEdges (counter+1) (m-1) (Array.concat [[|addEdge|];edges])
            getEdges 0 numberOfEdgesPerIteration [||] |> ignore

        g

    // Creating a random generated network based on NetworkX, a Python package for the creation, manipulation, and study of the structure, dynamics, and functions of complex networks. Based on the scale free network method,NetworkX ReferenceRelease 2.5, page 630.
    // https://networkx.org/
    /// Returns a randomly generated, directed, scale free ArrayAdjacencyGraph, based on the given paramters.
    ///
    ///   n : integer
    ///       Number of nodes in graph
    ///
    ///   alpha : float
    ///       Probability for adding a new node connected to an existing node
    ///       chosen randomly according to the in-degree distribution.
    ///
    ///   beta : float
    ///       Probability for adding an edge between two existing nodes.
    ///       One existing node is chosen randomly according the in-degree
    ///       distribution and the other chosen randomly according to the out-degree
    ///       distribution.
    ///
    ///   gamma : float
    ///       Probability for adding a new node connected to an existing node
    ///       chosen randomly according to the out-degree distribution.
    ///
    ///   delta_in : float
    ///       Bias for choosing nodes from in-degree distribution.
    ///
    ///   delta_out : float
    ///       Bias for choosing nodes from out-degree distribution.
    ///
    ///   create_using : an ArrayAdjacencyGraph, that can be used as basis for the graph generation. If the given graph has less than 3 vertices, a hard-coded example is used instead. 
    ///The sum of `alpha`, `beta`, and `gamma` must be 1. 
    let bollobasRiordan (n: int) (alpha: float) (beta: float) (gamma: float) (delta_in: float) (delta_out: float) (create_using:ArrayAdjacencyGraph<int,int,float>) =
        
        if alpha+beta+gamma <> 1. then 
            failwithf "The sum of alpha, beta, and gamma must be 1., but here is %A" (alpha+beta+gamma)
    
        if alpha <= 0. then 
               failwith "alpha must be > 0."
        if beta <= 0. then
            failwith "beta must be > 0."
        if gamma <= 0. then
            failwith "gamma must be > 0."
        
        let G = 
            if create_using.VertexCount < 3 then 
                create_using.AddManyVertices[|(0,0);(1,1);(2,2)|]|>ignore
                create_using.AddManyEdges[|(0, 1, 1.); (1, 2, 1.); (2, 0, 1.)|]
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
                    let d = float (G.InDegree n)
                    cumsum <- (cumsum)+((d+delta) / psum)
                    if r < cumsum then
                        threshold <- true
                    else
                        n <- n+1      
                n
            elif distribution = "out" then
                while (not threshold) do
                    let d = float (G.OutDegree n)
                    cumsum <- (cumsum)+((d+delta) / psum)
                    if r < cumsum then
                        threshold <- true
                    else
                        n <- n+1     
                n
            else
                failwith "ERROR"
    
        while G.VertexCount < n do
            let psum_in     = float (G.EdgeCount) + delta_in  *  float (G.VertexCount)
            let psum_out    = float (G.EdgeCount) + delta_out * float (G.VertexCount)
            let r = rnd.NextDouble()
    
            if r < alpha then
                let v = (G.VertexCount)
                let w = _choose_node("in",delta_in,psum_in)
                G.AddVertex (v,v)|>ignore
                G.AddEdge (v,w,1.)
                |>ignore
    
            elif r < (alpha + beta) then
                let v = _choose_node("out",delta_out,psum_out)
                let w = _choose_node("in", delta_in, psum_in)
    
                G.AddEdge(v,w,1.)
                |>ignore
    
            else
                let v = _choose_node("out",delta_out,psum_out)
                let w = G.VertexCount
    
                G.AddVertex (w,w)|>ignore
                G.AddEdge (v,w,1.)
                |>ignore
    
        G


    // Adaptation of the gilbert random plane networks
    // Gilbert, E.N., 1961. Random plane networks. Journal of the society for industrial and applied mathematics, 9(4), pp.533-543.
    /// Returns an ArrayAdjacencyGraph that is generated randomly with the given parameters.
    ///
    /// numberOfVertices indicates the number of vertices the final graph will have. 
    ///
    /// probability represents the probability of an edge between 2 vertices.   
    ///
    /// fVertexKey is a function that is used to transform an integer (the index of the vertex) into the 'Vertex type.  
    ///
    /// fLabel is a function that transforms the 'Vertex type into a label of the 'Label type.   
    ///
    /// fWeight is a funtion that takes two 'Vertices and returns a weight between them in form of an 'Edge type.    
    let gilbert (numberOfVertices: int) (probability: float) (isDirected: bool) (fVertexKey: int -> 'Vertex) (fLabel: 'Vertex -> 'Label) (fWeight: 'Vertex*'Vertex -> 'Edge) =
        if probability > 1. || probability < 0. then failwithf "The stated probability %F is outside the expected range of 0. to 1." probability

        let rnd         = new System.Random()
        let vertexEdges = System.Collections.Generic.Dictionary<'Vertex,LEdge<'Vertex,'Edge>ResizeArray>()
        let labelDict   = System.Collections.Generic.Dictionary<'Vertex,'Label>()

        let vertices = 
             [|
                for i=0 to (numberOfVertices-1) do
                    fVertexKey i
             |]

        for i=0 to (numberOfVertices-1) do          
            let vertex = vertices.[i]
            let label   = fLabel vertex
            labelDict.Add(vertex,label)
            vertexEdges.Add(vertex,ResizeArray())

        if isDirected then
            for s in vertices do
        
                for t in vertices do

                    if rnd.NextDouble() < probability then
                
                        if s=t then                                            
                            let w       = fWeight (s,t)
                            let valueS  = vertexEdges.Item s 
                            (vertexEdges.Item s).Add(s,s,w)
                            (vertexEdges.Item s).Add(s,s,w)
                        else
                            let w       = fWeight (s,t)
                            let valueS  = vertexEdges.Item s 
                            let valueT  = vertexEdges.Item t
                            (vertexEdges.Item s).Add(s,t,w)
                            (vertexEdges.Item t).Add(s,t,w)                  
        else
            for i=0 to vertices.Length-1 do 
                let s = vertices.[i]

                for j=i to vertices.Length-1 do
                    if rnd.NextDouble() < probability then
                        let t = vertices.[j]
    
                        if s=t then 
                            let w       = fWeight (s,t)
                            let valueS  = vertexEdges.Item s 
                            (vertexEdges.Item s).Add(s,s,w)
                        else
                            let w       = fWeight (s,t)
                            let valueS  = vertexEdges.Item s 
                            let valueT  = vertexEdges.Item t
                            (vertexEdges.Item s).Add(s,t,w)
                            (vertexEdges.Item t).Add(s,t,w)        
        
        ArrayAdjacencyGraph(vertexEdges,labelDict)
