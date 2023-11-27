namespace Graphoscope.Measures
open Graphoscope
open FSharpAux

type Radius() =
    
    /// <summary> 
    /// Get the radius of graph calculated by their minimum Eccentricity
    /// </summary>
    /// <param name="floydWarshall">Result of the FloydWarshall shortest Path calculation of a graph</param> 
    /// <returns>A float of the shortest shortest Paths of a graph</returns>
    static member ofGraph2D (floydWarshall : float [,]) = 
        ///Returns the element of the array which is the smallest after projection according to the Operators.min operator
        let minBy projection (arr: _ [,])  =
            let n,m = arr |> Array2D.length1, arr |> Array2D.length2
            let rec compareMin i j min =
                if j = m then min
                elif i=j then
                    compareMin i (j+1) min 
                else
                    let value = arr.[i,j]
                    if (projection value) > (projection min) then compareMin i (j+1) min 
                    else compareMin i (j+1) value 
            let rec countRow min i = 
                if i = n then min
                else countRow (compareMin i 0 min) (i+1)
            countRow infinity 0

        floydWarshall
        |> minBy id

    /// <summary> 
    /// Get the radius of graph calculated by their minimum Eccentricity
    /// </summary>
    /// <param name="weigthF">Function to get a float edgeweight of the EdgeData</param> 
    /// <param name="graph">The graph to calculate the radius for</param> 
    /// <returns>A float of the longest shortest Path of a graph</returns>
    static member ofFGraph (weigthF:'EdgeData->float) (graph:FGraph<'NodeKey,'NodeData,'EdgeData>)  = 
        Eccentricity.ofFGraph weigthF graph
        |> Seq.minBy snd
        |> snd

    /// <summary> 
    /// Get the radius of graph calculated by their minimum Eccentricity
    /// </summary>
    /// <param name="weigthF">Function to get a float edgeweight of the EdgeData</param> 
    /// <param name="graph">The graph to calculate the radius for</param> 
    /// <returns>A float of the longest shortest Path of a graph</returns>
    static member ofAdjGraph (weigthF:'EdgeData->float) (graph:AdjGraph<'NodeKey,'NodeData,'EdgeData>)  = 
        Eccentricity.ofAdjGraph weigthF graph
        |> Seq.minBy snd
        |> snd

    /// <summary> 
    /// Get the radius of graph calculated by their minimum Eccentricity
    /// </summary>
    /// <param name="graph">The graph to calculate the radius for</param> 
    /// <returns>A float of the longest shortest Path of a graph</returns>
    static member compute((graph:FGraph<'NodeKey,'NodeData,'EdgeData>)) =
        Radius.ofFGraph (fun x -> 1.) graph

    /// <summary> 
    /// Get the radius of graph calculated by their minimum Eccentricity
    /// </summary>
    /// <param name="graph">The graph to calculate the radius for</param> 
    /// <returns>A float of the longest shortest Path of a graph</returns>
    static member compute((graph:AdjGraph<'NodeKey,'NodeData,'EdgeData>)) =
        Radius.ofAdjGraph (fun x -> 1.) graph


    /// <summary> 
    /// Get the radius of graph calculated by their minimum Eccentricity
    /// </summary>
    /// <param name="graph">The graph to calculate the radius for</param> 
    /// <returns>A float of the longest shortest Path of a graph</returns>
    static member computeWithEdgeData((graph:FGraph<'NodeKey,'NodeData,float>)) =
        Radius.ofFGraph id graph

    /// <summary> 
    /// Get the radius of graph calculated by their minimum Eccentricity
    /// </summary>
    /// <param name="graph">The graph to calculate the radius for</param> 
    /// <returns>A float of the longest shortest Path of a graph</returns>
    static member computeWithEdgeData((graph:AdjGraph<'NodeKey,'NodeData,float>)) =
        Radius.ofAdjGraph id graph

        /// <summary> 
    /// Get the radius of graph calculated by their minimum Eccentricity
    /// </summary>
    /// <param name="graph">The graph to calculate the radius for</param> 
    /// <returns>A float of the longest shortest Path of a graph</returns>
    static member computeWithEdgeDataBy((weightF:'EdgeData -> float),(graph:FGraph<'NodeKey,'NodeData,'EdgeData>)) =
        Radius.ofFGraph weightF graph

    /// <summary> 
    /// Get the radius of graph calculated by their minimum Eccentricity
    /// </summary>
    /// <param name="graph">The graph to calculate the radius for</param> 
    /// <returns>A float of the longest shortest Path of a graph</returns>computeWithEdgeDataBy
    static member computeWithEdgeDataBy((weightF:'EdgeData -> float),(graph:AdjGraph<'NodeKey,'NodeData,'EdgeData>)) =
        Radius.ofAdjGraph weightF graph
    