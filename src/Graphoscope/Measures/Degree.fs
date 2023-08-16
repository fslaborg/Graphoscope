namespace Graphoscope.Measures

open Graphoscope
open FSharpAux

type Degree() =
    



    //Get Distribution




    /// <summary> 
    /// Returns the degree sequence of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float seq of degree values</returns>
    static member sequenceOfFGraph(graph : FGraph<'NodeKey,'NodeData,'EdgeData>) = 
        graph
        |> FGraph.mapContexts FContext.degree
        |> Seq.map (fun (_,d) -> float d)
        |> Seq.sortDescending

    /// <summary> 
    /// Returns the degree sequence of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An array of degree values in descending order</returns>
    static member sequenceOfDiGraph(graph : DiGraph<'NodeKey, 'EdgeData>) =
        Array.init graph.NodeKeys.Count (fun i -> graph.InEdges[i].Count + graph.OutEdges[i].Count)
        |> Array.sortDescending

    
    /// <summary> 
    /// Returns the degree sequence of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An array of degree values in descending order</returns>
    static member sequence(graph : DiGraph<'NodeKey, 'EdgeData>) = 
        Degree.sequenceOfDiGraph graph

    /// <summary> 
    /// Returns the degree sequence of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float seq of degree values</returns>
    static member sequence(graph : FGraph<'NodeKey,'NodeData,'EdgeData>) =
        Degree.sequenceOfFGraph graph




    //Get Average




    /// <summary> 
    /// Get the mean degree of the graph. 
    /// This is an undirected measure so inbound links add to a nodes degree.
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the mean degree</returns>
    static member averageofFGraph(graph : FGraph<'NodeKey,'NodeData,'EdgeData>) = 
        graph
        |> FGraph.mapContexts FContext.degree
        |> Seq.averageBy (fun (_,d) -> float d) 
    
    /// <summary> 
    /// Get the mean degree of the graph. 
    /// This is an undirected measure so inbound links add to a nodes degree.
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the mean degree</returns>
    static member averageofDiGraph(graph : DiGraph<'NodeKey, 'EdgeData>) =
        Degree.sequenceOfDiGraph graph
        |> Array.averageBy float

    /// <summary> 
    /// Get the mean degree of the graph. 
    /// This is an undirected measure so inbound links add to a nodes degree.
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the mean degree</returns>
    static member averageofUndirectedGraph(graph : Graph.UndirectedGraph<'NodeKey, 'EdgeData>) =
        graph.Edges
        |> ResizeArray.map(fun n -> (n |> ResizeArray.length) * 2 |> float)
        |> ResizeArray.toArray
        |> Array.average
   
    /// <summary> 
    /// Get the mean degree of the graph. 
    /// This is an undirected measure so inbound links add to a nodes degree.
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the mean degree</returns>
    static member average(graph : DiGraph<'NodeKey, 'EdgeData>) =
        Degree.averageofDiGraph graph

    /// <summary> 
    /// Get the mean degree of the graph. 
    /// This is an undirected measure so inbound links add to a nodes degree.
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the mean degree</returns>
    static member average (graph : FGraph<'NodeKey,'NodeData,'EdgeData>) =
        Degree.averageofFGraph graph

    /// <summary> 
    /// Get the mean degree of the graph. 
    /// This is an undirected measure so inbound links add to a nodes degree.
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the mean degree</returns>
    static member average (graph : Graph.UndirectedGraph<'NodeKey,'EdgeData>) =
        Degree.averageofUndirectedGraph graph


    // Get Max Degree




    /// <summary> 
    /// Get the max degree of the graph. 
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An int of the max degree</returns>
    static member maximumOfFGraph (graph : FGraph<'NodeKey,'NodeData,'EdgeData>) = 
        graph 
        |> FGraph.mapContexts FContext.degree
        |> Seq.maxBy (fun (_,d) -> float d) 
        |> snd 

    /// <summary> 
    /// Get the max degree of the graph. 
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An int of the max degree</returns>
    static member maximumOfDiGraph (graph : DiGraph<'NodeKey, 'EdgeData>) =
        Degree.sequenceOfDiGraph graph
        |> Array.head

    /// <summary> 
    /// Get the max degree of the graph. 
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An int of the max degree</returns>
    static member maximum (graph : FGraph<'NodeKey,'NodeData,'EdgeData>) = 
        Degree.maximumOfFGraph graph

    /// <summary> 
    /// Get the max degree of the graph. 
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An int of the max degree</returns>
    static member maximum (graph : DiGraph<'NodeKey, 'EdgeData>) =
        Degree.maximumOfDiGraph graph




    // Get Min Degree




    /// <summary> 
    /// Get the min degree of the graph. 
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An int of the min degree</returns>
    static member minimumOfFGraph (graph : FGraph<'NodeKey,'NodeData,'EdgeData>) = 
        graph 
        |> FGraph.mapContexts FContext.degree
        |> Seq.minBy (fun (_,d) -> float d) 
        |> snd 

    /// <summary> 
    /// Get the min degree of the graph. 
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An int of the min degree</returns>
    static member minimumOfDiGraph (graph : DiGraph<'NodeKey, 'EdgeData>) =
        Degree.sequenceOfDiGraph graph
        |> Array.last


    /// <summary> 
    /// Get the min degree of the graph. 
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An int of the min degree</returns>
    static member minimum (graph : FGraph<'NodeKey,'NodeData,'EdgeData>) = 
        Degree.minimumOfFGraph graph

    /// <summary> 
    /// Get the min degree of the graph. 
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An int of the min degree</returns>
    static member minimum (graph : DiGraph<'NodeKey, 'EdgeData>) =
        Degree.minimumOfDiGraph graph
