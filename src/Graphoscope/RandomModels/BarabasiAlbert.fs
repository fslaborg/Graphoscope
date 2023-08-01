namespace Graphoscope.RandomModels

open Graphoscope

// The Barabási-Albert (BA) model is a popular network growth model used to generate random scale-free networks.  It provides a valuable tool for generating synthetic networks that exhibit similar properties to many natural and man-made networks, mainly a scale-free character.
type BarabasiAlbert() =
    
    /// <summary> 
    /// Returns an FGraph that was randomly grown according to the Barabási–Albert model with the given parameters. 
    /// </summary>
    /// <param name="numberOfVertices"> specifies how many additional vertices the final graph will have.</param>
    /// <param name="numberOfEdgesPerIteration"> specifies how many edges should be added to the graph per iteration.</param>
    /// <param name="fVertexKey"> is a function that is used to transform an integer (the index of the vertex) into the 'Vertex type.</param>
    /// <param name="fLabel"> is a function that transforms the 'Vertex type into a label of the 'Label type.</param>
    /// <param name="fWeight"> is a funtion that takes two 'Vertices and returns a weight between them in form of an 'Edge type.</param>
    /// <param name="startingGraph"> is the original graph, that is used as the initial connected network. The rest of the calculations and growth of the network are performed on this graph.</param>
    /// <returns>An FGraph</returns>

    static member initFGraph (numberOfVertices: int) (numberOfEdgesPerIteration: int) (fVertexKey: int -> int) (fLabel: int -> int) (fWeight: int*int -> float) (startingGraph: FGraph<int, int, float>)  =
        let rnd = new System.Random()
           
        let g = startingGraph
    
        let oldV = g.Count
    
        for n=oldV to oldV+numberOfVertices do
            let vertex  = fVertexKey n
            let label   = fLabel vertex
            FGraph.addNode vertex label g|> ignore
            
            let mutable m = 0
    
            let p k = ( (float (g.Item k|>FContext.degree )) / (float (FGraph.countEdges g)) )
            let possibleConnections = 
                [
                    for i in g.Keys do
                        (vertex,i),(p i)
                ]
                |> List.sortBy snd
            
            let rec getEdges counter m edges =
     
                if m = 0 then
                    FGraph.addEdges edges g
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
                            getEdges (counter+1) (m-1) (addEdge::edges)
            getEdges 0 numberOfEdgesPerIteration [] |> ignore
    
        g
    
        