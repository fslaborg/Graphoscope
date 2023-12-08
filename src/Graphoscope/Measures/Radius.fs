namespace Graphoscope.Measures
open Graphoscope
open Graphoscope.Graphs
open FSharpAux

type Radius() =
    
    /// <summary> 
    /// Get the radius of graph calculated by their minimum Eccentricity
    /// </summary>
    /// <param name="floydWarshall">Result of the FloydWarshall shortest Path calculation of a graph</param> 
    /// <returns>A float of the shortest shortest Paths of a graph</returns>
    static member oFContextMap2D (floydWarshall : float [,]) = 
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
    static member ofFContextMap (weigthF:'EdgeData->float) (graph: Directed.FContextMap<'NodeKey,'NodeData,'EdgeData>)  = 
        Eccentricity.ofFContextMap weigthF graph
        |> Seq.minBy snd
        |> snd

    /// <summary> 
    /// Get the radius of graph calculated by their minimum Eccentricity
    /// </summary>
    /// <param name="weigthF">Function to get a float edgeweight of the EdgeData</param> 
    /// <param name="graph">The graph to calculate the radius for</param> 
    /// <returns>A float of the longest shortest Path of a graph</returns>
    static member OfUndirectedFContextMap (weigthF:'EdgeData->float) (graph:Undirected.FContextMapU<'NodeKey,'NodeData,'EdgeData>)  = 
        Eccentricity.OfUndirectedFContextMap weigthF graph
        |> Seq.minBy snd
        |> snd

    /// <summary> 
    /// Get the radius of graph calculated by their minimum Eccentricity
    /// </summary>
    /// <param name="graph">The graph to calculate the radius for</param> 
    /// <returns>A float of the longest shortest Path of a graph</returns>
    static member compute((graph: Directed.FContextMap<'NodeKey,'NodeData,'EdgeData>)) =
        Radius.ofFContextMap (fun x -> 1.) graph

    /// <summary> 
    /// Get the radius of graph calculated by their minimum Eccentricity
    /// </summary>
    /// <param name="graph">The graph to calculate the radius for</param> 
    /// <returns>A float of the longest shortest Path of a graph</returns>
    static member computeUndirected((graph: Undirected.FContextMapU<'NodeKey,'NodeData,'EdgeData>)) =
        Radius.OfUndirectedFContextMap (fun x -> 1.) graph


    /// <summary> 
    /// Get the radius of graph calculated by their minimum Eccentricity
    /// </summary>
    /// <param name="graph">The graph to calculate the radius for</param> 
    /// <returns>A float of the longest shortest Path of a graph</returns>
    static member computeWithEdgeData((graph: Directed.FContextMap<'NodeKey,'NodeData,float>)) =
        Radius.ofFContextMap id graph

    /// <summary> 
    /// Get the radius of graph calculated by their minimum Eccentricity
    /// </summary>
    /// <param name="graph">The graph to calculate the radius for</param> 
    /// <returns>A float of the longest shortest Path of a graph</returns>
    static member computeUndirectedWithEdgeData((graph: Undirected.FContextMapU<'NodeKey,'NodeData,float>)) =
        Radius.OfUndirectedFContextMap id graph

        /// <summary> 
    /// Get the radius of graph calculated by their minimum Eccentricity
    /// </summary>
    /// <param name="graph">The graph to calculate the radius for</param> 
    /// <returns>A float of the longest shortest Path of a graph</returns>
    static member computeWithEdgeDataBy((weightF:'EdgeData -> float),(graph: Directed.FContextMap<'NodeKey,'NodeData,'EdgeData>)) =
        Radius.ofFContextMap weightF graph

    /// <summary> 
    /// Get the radius of graph calculated by their minimum Eccentricity
    /// </summary>
    /// <param name="graph">The graph to calculate the radius for</param> 
    /// <returns>A float of the longest shortest Path of a graph</returns>computeWithEdgeDataBy
    static member computeUndirectedWithEdgeDataBy((weightF:'EdgeData -> float),(graph:Undirected.FContextMapU<'NodeKey,'NodeData,'EdgeData>)) =
        Radius.OfUndirectedFContextMap weightF graph
    