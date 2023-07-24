namespace Graphoscope.DiGraph

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
    let getShortestPath (origin :'NodeKey)  (destination :'NodeKey) (graph : DiGraph<'NodeKey, float>)  =
        Dijkstra.Compute graph origin 
        |> Array.tryFind(fun (d,_) -> d = destination)
        |> fun o -> 
            match o with 
            | Some (n,f) -> Some f
            | None -> None

    /// <summary> 
    /// Get the mean degree of the graph. 
    /// This is an undirected measure so inbound links add to a nodes degree.
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the mean degree</returns>
    let getMeanDegree (graph : DiGraph<'NodeKey, 'EdgeData>)  = 
        graph.OutEdges
        |> ResizeArray.map(fun n -> (n |> ResizeArray.length) * 2 |> float)
        |> ResizeArray.toArray
        |> Array.average
    
    /// <summary> 
    /// Gets the total number of edges of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the total edges</returns>
    let getVolume(graph : DiGraph<'NodeKey, 'EdgeData>)  = 
        graph.OutEdges 
        |> ResizeArray.map(fun n -> n |> ResizeArray.length |> float)
        |> ResizeArray.toArray
        |> Array.sum
        |> fun v -> (v|> float) 
        |> int

    /// <summary> 
    /// Gets the total number of nodes of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the total nodes</returns>
    let getSize (graph : DiGraph<'NodeKey, 'EdgeData>) = 
        graph.Nodes  |> ResizeArray.length
    
    /// <summary> 
    /// Returns the degree distribution of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float array of degree values</returns>
    let getDegreeDistribution (graph : DiGraph<'NodeKey, 'EdgeData>) = 
        graph.OutEdges 
        |> ResizeArray.map(fun n -> n |> ResizeArray.length |> float)
        |> ResizeArray.toArray
