namespace Graphoscope.Measures

open Graphoscope
open FSharpAux

type OutDegree() =
        



    //Get Distribution
    
    
    
    
    /// <summary> 
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float seq of out-degree values</returns>
    static member sequenceOfFGraph(graph : FGraph<'NodeKey,'NodeData,'EdgeData>) = 
        graph
        |> FGraph.mapContexts FContext.outwardDegree
        |> Seq.map (fun (_,d) -> float d)
        |> Seq.sortDescending

    /// <summary> 
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float seq of out-degree values</returns>
    static member sequenceOfAdjGraph (graph : AdjGraph<'NodeKey,'NodeData,'EdgeData>) = 
        Degree.sequenceOfAdjGraph graph

    /// <summary> 
    /// Returns the out-degree sequence of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An array of out-degree values in descending order</returns>
    static member sequenceOfDiGraph(graph : DiGraph<'NodeKey, 'NodeData, 'EdgeData>) =
        graph.OutEdges
        |> ResizeArray.map(fun x -> x.Count)
        |> ResizeArray.toArray
        |> Array.sortDescending


    /// <summary> 
    /// Returns the out-degree sequence of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float seq of out-degree values</returns>
    static member sequence(graph : FGraph<'NodeKey,'NodeData,'EdgeData>) = 
        OutDegree.sequenceOfFGraph graph

    /// <summary> 
    /// Returns the out-degree sequence of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float seq of out-degree values</returns>
    static member sequence(graph : AdjGraph<'NodeKey,'NodeData,'EdgeData>) = 
        OutDegree.sequenceOfAdjGraph graph

    /// <summary> 
    /// Returns the out-degree sequence of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An array of out-degree values in descending order</returns>
    static member sequence(graph : DiGraph<'NodeKey, 'NodeData, 'EdgeData>) = 
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
    static member averageofAdjGraph (graph : AdjGraph<'NodeKey,'NodeData,'EdgeData>) = 
        Degree.averageofAdjGraph graph
      
    /// <summary> 
    /// Get the mean out-degree of the graph. 
    /// This is an undirected measure so inbound links add to a nodes out-degree.
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the mean out-degree</returns>
    static member averageofDiGraph(graph : DiGraph<'NodeKey, 'NodeData, 'EdgeData>) =
        graph.OutEdges
        |> Seq.averageBy(fun x -> float x.Count)
   
    /// <summary> 
    /// Get the mean out-degree of the graph. 
    /// This is an undirected measure so inbound links add to a nodes out-degree.
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the mean out-degree</returns>
    static member average(graph : DiGraph<'NodeKey, 'NodeData, 'EdgeData>) =
        OutDegree.averageofDiGraph graph

    /// <summary> 
    /// Get the mean out-degree of the graph. 
    /// This is an undirected measure so inbound links add to a nodes out-degree.
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the mean out-degree</returns>
    static member average (graph : FGraph<'NodeKey,'NodeData,'EdgeData>) =
        OutDegree.averageofFGraph graph

    /// <summary> 
    /// Get the mean out-degree of the graph. 
    /// This is an undirected measure so inbound links add to a nodes out-degree.
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the mean out-degree</returns>
    static member average (graph : AdjGraph<'NodeKey,'NodeData,'EdgeData>) =
        OutDegree.averageofAdjGraph graph


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
    static member maximumOfAdjGraph (graph : AdjGraph<'NodeKey,'NodeData,'EdgeData>) = 
        Degree.maximumOfAdjGraph graph

    /// <summary> 
    /// Get the max out-degree of the graph. 
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An int of the max out-degree</returns>
    static member maximumOfDiGraph (graph : DiGraph<'NodeKey, 'NodeData, 'EdgeData>) =
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
    static member maximum (graph : AdjGraph<'NodeKey,'NodeData,'EdgeData>) = 
        OutDegree.maximumOfAdjGraph graph

    /// <summary> 
    /// Get the max out-degree of the graph. 
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An int of the max out-degree</returns>
    static member maximum (graph : DiGraph<'NodeKey, 'NodeData, 'EdgeData>) =
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
    /// <returns>An int of the max out-degree</returns>
    static member minimumOfAdjGraph (graph : AdjGraph<'NodeKey,'NodeData,'EdgeData>) = 
        Degree.minimumOfAdjGraph graph

    /// <summary> 
    /// Get the min out-degree of the graph. 
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An int of the min out-degree</returns>
    static member minimumOfDiGraph (graph : DiGraph<'NodeKey, 'NodeData, 'EdgeData>) = 
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
    static member minimum (graph : AdjGraph<'NodeKey,'NodeData,'EdgeData>) = 
        OutDegree.minimumOfAdjGraph graph

    /// <summary> 
    /// Get the min out-degree of the graph. 
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An int of the min out-degree</returns>
    static member minimum (graph : DiGraph<'NodeKey, 'NodeData, 'EdgeData>) =
        OutDegree.minimumOfDiGraph graph
