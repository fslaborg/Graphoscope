namespace Graphoscope.Graph

open Algorithms
open FSharpAux

module Measures = 

    /// <summary> 
    /// Returns the distance in numebr of directed edges between two nodes.
    /// </summary>
    /// <param name="origin">The starting node of the path</param> 
    /// <param name="destination">The finishing node of the path</param> 
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the distance</returns>
    let getShortestPath (origin :'NodeKey)  (destination :'NodeKey) (graph : Graph<'NodeKey, float>)  =
        Dijkstra.Compute graph origin 
        |> Array.tryFind(fun (d,_) -> d = destination)
        |> Option.map snd
    
    /// <summary> 
    /// Gets the total number of edges of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the total edges</returns>
    let getVolume(graph : Graph<'NodeKey, 'EdgeData>)  = 
        graph.Edges 
        |> ResizeArray.map(fun n -> n |> ResizeArray.length |> float)
        |> ResizeArray.toArray
        |> Array.sum
        |> fun v -> (v / 2. |> float) 
        |> int

    /// <summary> 
    /// Gets the total number of nodes of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the total nodes</returns>
    let getSize (graph : Graph<'NodeKey, 'EdgeData>) = 
        graph.NodeKeys |> ResizeArray.length

    /// <summary> 
    /// Get the mean degree of the graph. 
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the mean degree</returns>
    let getMeanDegree (graph : Graph<'NodeKey, 'EdgeData>)  = 
        2. * (getVolume graph |> float) / (getSize graph |> float)
    
    /// <summary> 
    /// Returns the degree distribution of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float array of degree values</returns>
    let getDegreeDistribution (graph : Graph<'NodeKey, 'EdgeData>) = 
        graph.Edges 
        |> Seq.map(fun n -> n |> ResizeArray.length |> float)
        |> Seq.sortDescending
        |> Array.ofSeq
