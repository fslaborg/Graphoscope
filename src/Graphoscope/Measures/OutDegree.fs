namespace Graphoscope.Measures

open Graphoscope
open FSharpAux

type OutDegree() =
        



    //Get Distribution
    
    
    
    
    /// <summary> 
    /// Returns the out-degree distribution of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float seq of out-degree values</returns>
    static member distributionOfFGraph(graph : FGraph<'NodeKey,'NodeData,'EdgeData>) = 
        graph
        |> FGraph.mapContexts FContext.outwardDegree
        |> Seq.map (fun (_,d) -> float d)

    /// <summary> 
    /// Returns the out-degree sequence of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An array of out-degree values in descending order</returns>
    static member sequenceOfDiGraph(graph : DiGraph<'NodeKey, 'EdgeData>) =
        graph.OutEdges
        |> ResizeArray.map(fun x -> x.Count)
        |> ResizeArray.toArray
        |> Array.sortDescending


    /// <summary> 
    /// Returns the out-degree distribution of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float seq of out-degree values</returns>
    static member distribution(graph : FGraph<'NodeKey,'NodeData,'EdgeData>) = 
        OutDegree.distributionOfFGraph graph

    /// <summary> 
    /// Returns the out-degree sequence of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An array of out-degree values in descending order</returns>
    static member sequence(graph : DiGraph<'NodeKey, 'EdgeData>) = 
        OutDegree.sequenceOfDiGraph graph




    //Get Average




    /// <summary> 
    /// Get the mean out-degree of the graph. 
    /// This is an undirected measure so inbound links add to a nodes out-degree.
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the mean out-degree</returns>
    static member averageofFGraph(graph : FGraph<'NodeKey,'NodeData,'EdgeData>) = 
        graph
        |> FGraph.mapContexts FContext.outwardDegree
        |> Seq.averageBy (fun (_,d) -> float d) 
    
    /// <summary> 
    /// Get the mean out-degree of the graph. 
    /// This is an undirected measure so inbound links add to a nodes out-degree.
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the mean out-degree</returns>
    static member averageofDiGraph(graph : DiGraph<'NodeKey, 'EdgeData>) =
        graph.OutEdges
        |> Seq.averageBy(fun x -> float x.Count)
   
    /// <summary> 
    /// Get the mean out-degree of the graph. 
    /// This is an undirected measure so inbound links add to a nodes out-degree.
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the mean out-degree</returns>
    static member average(graph : DiGraph<'NodeKey, 'EdgeData>) =
        OutDegree.averageofDiGraph graph

    /// <summary> 
    /// Get the mean out-degree of the graph. 
    /// This is an undirected measure so inbound links add to a nodes out-degree.
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the mean out-degree</returns>
    static member average (graph : FGraph<'NodeKey,'NodeData,'EdgeData>) =
        OutDegree.averageofFGraph graph




    // Get Max Degree




    /// <summary> 
    /// Get the max out-degree of the graph. 
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An int of the max out-degree</returns>
    static member maximumOfFGraph (graph : FGraph<'NodeKey,'NodeData,'EdgeData>) = 
        graph 
        |> FGraph.mapContexts FContext.outwardDegree
        |> Seq.maxBy (fun (_,d) -> float d) 
        |> snd 

    /// <summary> 
    /// Get the max out-degree of the graph. 
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An int of the max out-degree</returns>
    static member maximumOfDiGraph (graph : DiGraph<'NodeKey, 'EdgeData>) =
        graph.OutEdges
        |> ResizeArray.map(fun x -> x.Count)
        |> ResizeArray.max

    /// <summary> 
    /// Get the max out-degree of the graph. 
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An int of the max out-degree</returns>
    static member maximum (graph : FGraph<'NodeKey,'NodeData,'EdgeData>) = 
        OutDegree.maximumOfFGraph graph

    /// <summary> 
    /// Get the max out-degree of the graph. 
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An int of the max out-degree</returns>
    static member maximum (graph : DiGraph<'NodeKey, 'EdgeData>) =
        OutDegree.maximumOfDiGraph graph
    
    
    
    
    // Get Min Degree




    /// <summary> 
    /// Get the min out-degree of the graph. 
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An int of the min out-degree</returns>
    static member minimumOfFGraph (graph : FGraph<'NodeKey,'NodeData,'EdgeData>) = 
        graph 
        |> FGraph.mapContexts FContext.outwardDegree
        |> Seq.minBy (fun (_,d) -> float d) 
        |> snd 

    /// <summary> 
    /// Get the min out-degree of the graph. 
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An int of the min out-degree</returns>
    static member minimumOfDiGraph (graph : DiGraph<'NodeKey, 'EdgeData>) = 
        graph.OutEdges
        |> ResizeArray.minBy(fun x -> x.Count)
        |> ResizeArray.length


    /// <summary> 
    /// Get the min out-degree of the graph. 
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An int of the min out-degree</returns>
    static member minimum (graph : FGraph<'NodeKey,'NodeData,'EdgeData>) = 
        OutDegree.minimumOfFGraph graph

    /// <summary> 
    /// Get the min out-degree of the graph. 
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An int of the min out-degree</returns>
    static member minimum (graph : DiGraph<'NodeKey, 'EdgeData>) =
        OutDegree.minimumOfDiGraph graph
