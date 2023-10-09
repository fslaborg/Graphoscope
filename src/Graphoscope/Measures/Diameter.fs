namespace Graphoscope.Measures
open Graphoscope
open FSharpAux

type Diameter() =
    
    /// <summary> 
    /// Get the diameter of graph calculated by their minimum Eccentricity
    /// </summary>
    /// <param name="floydWarshall">Result of the FloydWarshall shortest Path calculation of a graph</param> 
    /// <returns>A float of the shortest shortest Paths of a graph</returns>
    static member ofGraph2D (floydWarshall : float [,]) = 
        floydWarshall
        |> FSharpAux.Array2D.maxBy id

    /// <summary> 
    /// Get the diameter of graph calculated by their minimum Eccentricity
    /// </summary>
    /// <param name="weigthF">Function to get a float edgeweight of the EdgeData</param> 
    /// <param name="graph">The graph to calculate the diameter for</param> 
    /// <returns>A float of the longest shortest Path of a graph</returns>
    static member ofFGraph (weigthF:'EdgeData->float) (graph:FGraph<'NodeKey,'NodeData,'EdgeData>)  = 
        Eccentricity.ofFGraph weigthF graph
        |> Seq.maxBy snd
        |> snd

    /// <summary> 
    /// Get the diameter of graph calculated by their minimum Eccentricity
    /// </summary>
    /// <param name="weigthF">Function to get a float edgeweight of the EdgeData</param> 
    /// <param name="graph">The graph to calculate the diameter for</param> 
    /// <returns>A float of the longest shortest Path of a graph</returns>
    static member ofAdjGraph (weigthF:'EdgeData->float) (graph:AdjGraph<'NodeKey,'NodeData,'EdgeData>)  = 
        Eccentricity.ofAdjGraph weigthF graph
        |> Seq.maxBy snd
        |> snd

    /// <summary> 
    /// Get the diameter of graph calculated by their minimum Eccentricity
    /// </summary>
    /// <param name="weigthF">Function to get a float edgeweight of the EdgeData</param> 
    /// <param name="graph">The graph to calculate the diameter for</param> 
    /// <returns>A float of the longest shortest Path of a graph</returns>
    static member compute((weigthF:'EdgeData->float),(graph:FGraph<'NodeKey,'NodeData,'EdgeData>)) =
        Diameter.ofFGraph weigthF graph

    /// <summary> 
    /// Get the diameter of graph calculated by their minimum Eccentricity
    /// </summary>
    /// <param name="weigthF">Function to get a float edgeweight of the EdgeData</param> 
    /// <param name="graph">The graph to calculate the diameter for</param> 
    /// <returns>A float of the longest shortest Path of a graph</returns>
    static member compute((weigthF:'EdgeData->float),(graph:AdjGraph<'NodeKey,'NodeData,'EdgeData>)) =
        Diameter.ofAdjGraph weigthF graph