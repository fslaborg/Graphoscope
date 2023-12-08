namespace Graphoscope.Measures
open Graphoscope
open Graphoscope.Graphs
open FSharpAux

type Diameter() =
    
    /// <summary> 
    /// Get the diameter of graph calculated by their minimum Eccentricity
    /// </summary>
    /// <param name="floydWarshall">Result of the FloydWarshall shortest Path calculation of a graph</param> 
    /// <returns>A float of the shortest shortest Paths of a graph</returns>
    static member oFContextMap2D (floydWarshall : float [,]) = 
        floydWarshall
        |> FSharpAux.Array2D.maxBy id

    /// <summary> 
    /// Get the diameter of graph calculated by their minimum Eccentricity
    /// </summary>
    /// <param name="weigthF">Function to get a float edgeweight of the EdgeData</param> 
    /// <param name="graph">The graph to calculate the diameter for</param> 
    /// <returns>A float of the longest shortest Path of a graph</returns>
    static member ofFContextMap (weigthF:'EdgeData->float) (graph:Directed.FContextMap<'NodeKey,'NodeData,'EdgeData>)  = 
        Eccentricity.ofFContextMap weigthF graph
        |> Seq.maxBy snd
        |> snd

    /// <summary> 
    /// Get the diameter of graph calculated by their minimum Eccentricity
    /// </summary>
    /// <param name="weigthF">Function to get a float edgeweight of the EdgeData</param> 
    /// <param name="graph">The graph to calculate the diameter for</param> 
    /// <returns>A float of the longest shortest Path of a graph</returns>
    static member OfUndirectedFContextMap (weigthF:'EdgeData->float) (graph:Undirected.FContextMapU<'NodeKey,'NodeData,'EdgeData>)  = 
        Eccentricity.OfUndirectedFContextMap weigthF graph
        |> Seq.maxBy snd
        |> snd

    /// <summary> 
    /// Get the diameter of graph calculated by their minimum Eccentricity
    /// </summary>
    /// <param name="graph">The graph to calculate the diameter for</param> 
    /// <returns>A float of the longest shortest Path of a graph</returns>
    static member compute((graph:Directed.FContextMap<'NodeKey,'NodeData,'EdgeData>)) =
        Diameter.ofFContextMap (fun x -> 1.) graph

    /// <summary> 
    /// Get the diameter of graph calculated by their minimum Eccentricity
    /// </summary>
    /// <param name="graph">The graph to calculate the diameter for</param> 
    /// <returns>A float of the longest shortest Path of a graph</returns>
    static member computeUndirected((graph:Undirected.FContextMapU<'NodeKey,'NodeData,'EdgeData>)) =
        Diameter.OfUndirectedFContextMap (fun x -> 1.) graph

    /// <summary> 
    /// Get the diameter of graph calculated by their minimum Eccentricity
    /// </summary>
    /// <param name="graph">The graph to calculate the diameter for</param> 
    /// <returns>A float of the longest shortest Path of a graph</returns>
    static member computeWithEdgeData((graph:Directed.FContextMap<'NodeKey,'NodeData,float>)) =
        Diameter.ofFContextMap id graph

    /// <summary> 
    /// Get the diameter of graph calculated by their minimum Eccentricity
    /// </summary>
    /// <param name="graph">The graph to calculate the diameter for</param> 
    /// <returns>A float of the longest shortest Path of a graph</returns>
    static member computeUndirectedWithEdgeData((graph:Undirected.FContextMapU<'NodeKey,'NodeData,float>)) =
        Diameter.OfUndirectedFContextMap id graph


    /// <summary> 
    /// Get the diameter of graph calculated by their minimum Eccentricity
    /// </summary>
    /// <param name="weigthF">Function to get a float edgeweight of the EdgeData</param> 
    /// <param name="graph">The graph to calculate the diameter for</param> 
    /// <returns>A float of the longest shortest Path of a graph</returns>
    static member computeWithEdgeDataBy((weigthF:'EdgeData->float),(graph:Directed.FContextMap<'NodeKey,'NodeData,'EdgeData>)) =
        Diameter.ofFContextMap weigthF graph

    /// <summary> 
    /// Get the diameter of graph calculated by their minimum Eccentricity
    /// </summary>
    /// <param name="weigthF">Function to get a float edgeweight of the EdgeData</param> 
    /// <param name="graph">The graph to calculate the diameter for</param> 
    /// <returns>A float of the longest shortest Path of a graph</returns>
    static member computeUndirectedWithEdgeDataBy((weigthF:'EdgeData->float),(graph:Undirected.FContextMapU<'NodeKey,'NodeData,'EdgeData>)) =
        Diameter.OfUndirectedFContextMap weigthF graph