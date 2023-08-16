namespace Graphoscope.Measures

open Graphoscope
open FSharpAux

type InDegree() =
        



    //Get Distribution
    
    
    
    
    /// <summary> 
    /// Returns the in-degree distribution of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float seq of in-degree values</returns>
    static member distributionOfFGraph(graph : FGraph<'NodeKey,'NodeData,'EdgeData>) = 
        graph
        |> FGraph.mapContexts FContext.inwardDegree
        |> Seq.map (fun (_,d) -> float d)

    /// <summary> 
    /// Returns the in-degree distribution of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An array of in-degree values in descending order</returns>
    static member sequenceOfDiGraph(graph : DiGraph<'NodeKey, 'EdgeData>) = 
        graph.InEdges
        |> ResizeArray.map(fun x -> x.Count)
        |> ResizeArray.toArray
        |> Array.sortDescending


    /// <summary> 
    /// Returns the in-degree distribution of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float seq of in-degree values</returns>
    static member distribution(graph : FGraph<'NodeKey,'NodeData,'EdgeData>) = 
        InDegree.distributionOfFGraph graph

    /// <summary> 
    /// Returns the in-degree distribution of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An array of in-degree values in descending order</returns>
    static member sequence(graph : DiGraph<'NodeKey, 'EdgeData>) = 
        InDegree.sequenceOfDiGraph graph




    //Get Average




    /// <summary> 
    /// Get the mean In-degree of the graph. 
    /// This is an undirected measure so inbound links add to a nodes In-degree.
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the mean In-degree</returns>
    static member averageofFGraph(graph : FGraph<'NodeKey,'NodeData,'EdgeData>) = 
        graph
        |> FGraph.mapContexts FContext.inwardDegree
        |> Seq.averageBy (fun (_,d) -> float d) 
    
    /// <summary> 
    /// Get the mean In-degree of the graph. 
    /// This is an undirected measure so inbound links add to a nodes In-degree.
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the mean In-degree</returns>
    static member averageofDiGraph(graph : DiGraph<'NodeKey, 'EdgeData>) =
        graph.InEdges
        |> Seq.averageBy(fun x -> float x.Count)
   
    /// <summary> 
    /// Get the mean In-degree of the graph. 
    /// This is an undirected measure so inbound links add to a nodes In-degree.
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the mean In-degree</returns>
    static member average(graph : DiGraph<'NodeKey, 'EdgeData>) =
        InDegree.averageofDiGraph graph

    /// <summary> 
    /// Get the mean In-degree of the graph. 
    /// This is an undirected measure so inbound links add to a nodes In-degree.
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the mean In-degree</returns>
    static member average (graph : FGraph<'NodeKey,'NodeData,'EdgeData>) =
        InDegree.averageofFGraph graph




    // Get Max Degree




    /// <summary> 
    /// Get the max In-degree of the graph. 
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An int of the max In-degree</returns>
    static member maximumOfFGraph (graph : FGraph<'NodeKey,'NodeData,'EdgeData>) = 
        graph 
        |> FGraph.mapContexts FContext.inwardDegree
        |> Seq.maxBy (fun (_,d) -> float d) 
        |> snd 

    /// <summary> 
    /// Get the max In-degree of the graph. 
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An int of the max In-degree</returns>
    static member maximumOfDiGraph (graph : DiGraph<'NodeKey, 'EdgeData>) =
        graph.InEdges
        |> ResizeArray.map(fun x -> x.Count)
        |> ResizeArray.max
   

    /// <summary> 
    /// Get the max In-degree of the graph. 
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An int of the max In-degree</returns>
    static member maximum (graph : FGraph<'NodeKey,'NodeData,'EdgeData>) = 
        InDegree.maximumOfFGraph graph

    /// <summary> 
    /// Get the max In-degree of the graph. 
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An int of the max In-degree</returns>
    static member maximum (graph : DiGraph<'NodeKey, 'EdgeData>) =
        InDegree.maximumOfDiGraph graph
    
    
    
    
    // Get Min Degree




    /// <summary> 
    /// Get the min In-degree of the graph. 
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An int of the min In-degree</returns>
    static member minimumOfFGraph (graph : FGraph<'NodeKey,'NodeData,'EdgeData>) = 
        graph 
        |> FGraph.mapContexts FContext.inwardDegree
        |> Seq.minBy (fun (_,d) -> float d) 
        |> snd 

    /// <summary> 
    /// Get the min In-degree of the graph. 
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An int of the min In-degree</returns>
    static member minimumOfDiGraph (graph : DiGraph<'NodeKey, 'EdgeData>) =
        graph.InEdges
        |> ResizeArray.map(fun x -> x.Count)
        |> ResizeArray.max

    /// <summary> 
    /// Get the min In-degree of the graph. 
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An int of the min In-degree</returns>
    static member minimum (graph : FGraph<'NodeKey,'NodeData,'EdgeData>) = 
        InDegree.minimumOfFGraph graph

    /// <summary> 
    /// Get the min In-degree of the graph. 
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An int of the min In-degree</returns>
    static member minimum (graph : DiGraph<'NodeKey, 'EdgeData>) =
        InDegree.minimumOfDiGraph graph
