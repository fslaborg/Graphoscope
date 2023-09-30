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
        
        let p (g:FGraph<int, int, float>) k = ((float (g.Item k|>FContext.degree )) / (float (FGraph.countEdges g)))
        
        let getPossibleConnections (g:FGraph<int, int, float>) vertex = 
            [
                for i in g.Keys do
                    if FGraph.containsEdge vertex i g then 
                        None
                    else
                        Some ((vertex,i),(p g i))
            ]
            |> List.choose id
            |> List.sortBy snd
            
        
        let g = startingGraph
    
        let oldV = g.Count
    
        for n=oldV to oldV+numberOfVertices do
            let vertex  = fVertexKey n
            let label   = fLabel vertex
            FGraph.addNode vertex label g|> ignore
                

            let rec getEdges counter m possibleTargets edges =
     
                if m = 0 then
                    FGraph.addEdges edges g
                else
                    
                    if counter = List.length possibleTargets then
                        getEdges 0 m (getPossibleConnections g vertex) edges

                    else

                        let (edge,probability) = possibleTargets.[counter]
                        let r = rnd.NextDouble()
                        let addEdge = fst edge, snd edge,fWeight edge

                        if r > probability then
                            getEdges (counter+1) (m) (getPossibleConnections g vertex) (edges)

                        elif Set.contains (addEdge) edges then
                            getEdges (counter+1) (m) (getPossibleConnections g vertex) (edges)

                        else
                            getEdges (counter+1) (m-1) (getPossibleConnections g vertex) (Set.add addEdge edges)
                    
            getEdges 0 numberOfEdgesPerIteration (getPossibleConnections g vertex) Set.empty |> ignore
    
        g
    
    /// <summary> 
    /// Returns a DiGraph that was randomly grown according to the Barabási–Albert model with the given parameters. 
    /// </summary>
    /// <param name="numberOfVertices"> specifies how many additional vertices the final graph will have.</param>
    /// <param name="numberOfEdgesPerIteration"> specifies how many edges should be added to the graph per iteration.</param>
    /// <param name="fVertexKey"> is a function that is used to transform an integer (the index of the vertex) into the 'Vertex type.</param>
    /// <param name="fLabel"> is a function that transforms the 'Vertex type into a label of the 'Label type.</param>
    /// <param name="fWeight"> is a funtion that takes two 'Vertices and returns a weight between them in form of an 'Edge type.</param>
    /// <param name="startingGraph"> is the original graph, that is used as the initial connected network. The rest of the calculations and growth of the network are performed on this graph.</param>
    /// <returns>A DiGraph</returns>
    static member initDiGraph (numberOfVertices: int) (numberOfEdgesPerIteration: int) (fVertexKey: int -> int) (fLabel: int -> int) (fWeight: int*int -> float) (startingGraph: DiGraph<'NodeKey, 'NodeData, 'EdgeData>)  =    
        failwith"not implemented yet"



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
    static member init ((numberOfVertices: int),(numberOfEdgesPerIteration: int),(fVertexKey: int -> int),(fLabel: int -> int),(fWeight: int*int -> float),(startingGraph: FGraph<int, int, float>)) =
        BarabasiAlbert.initFGraph numberOfVertices numberOfEdgesPerIteration fVertexKey fLabel fWeight startingGraph

    /// <summary> 
    /// Returns a DiGraph that was randomly grown according to the Barabási–Albert model with the given parameters. 
    /// </summary>
    /// <param name="numberOfVertices"> specifies how many additional vertices the final graph will have.</param>
    /// <param name="numberOfEdgesPerIteration"> specifies how many edges should be added to the graph per iteration.</param>
    /// <param name="fVertexKey"> is a function that is used to transform an integer (the index of the vertex) into the 'Vertex type.</param>
    /// <param name="fLabel"> is a function that transforms the 'Vertex type into a label of the 'Label type.</param>
    /// <param name="fWeight"> is a funtion that takes two 'Vertices and returns a weight between them in form of an 'Edge type.</param>
    /// <param name="startingGraph"> is the original graph, that is used as the initial connected network. The rest of the calculations and growth of the network are performed on this graph.</param>
    /// <returns>A DiGraph</returns>
    static member init ((numberOfVertices: int),(numberOfEdgesPerIteration: int),(fVertexKey: int -> int),(fLabel: int -> int),(fWeight: int*int -> float),(startingGraph: DiGraph<'NodeKey, 'NodeData, 'EdgeData>)) =
        BarabasiAlbert.initDiGraph numberOfVertices numberOfEdgesPerIteration fVertexKey fLabel fWeight startingGraph
    