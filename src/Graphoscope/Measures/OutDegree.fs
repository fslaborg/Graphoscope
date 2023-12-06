namespace Graphoscope.Measures

open Graphoscope
open Graphoscope.Graphs
open FSharpAux

type OutDegree() =
        



    //Get Distribution
    
    
    
    
    /// <summary> 
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float seq of out-degree values</returns>
    static member sequenceOfFContextMap(graph : Directed.FContextMap<'NodeKey,'NodeData,'EdgeData>) = 
        graph
        |> Directed.FContextMap.mapContexts Directed.FContext.outwardDegree
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
    static member sequenceOfLilMatrix(graph : Directed.LilMatrix<'NodeKey, 'NodeData, 'EdgeData>) =
        graph.OutEdges
        |> ResizeArray.map(fun x -> x.Count)
        |> ResizeArray.toArray
        |> Array.sortDescending


    /// <summary> 
    /// Returns the out-degree sequence of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float seq of out-degree values</returns>
    static member sequence(graph : Directed.FContextMap<'NodeKey,'NodeData,'EdgeData>) = 
        OutDegree.sequenceOfFContextMap graph

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
    static member sequence(graph : Directed.LilMatrix<'NodeKey, 'NodeData, 'EdgeData>) = 
        OutDegree.sequenceOfLilMatrix graph




    //Get Average




    /// <summary> 
    /// Get the mean out-degree of the graph. 
    /// This is an undirected measure so inbound links add to a nodes out-degree.
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the mean out-degree</returns>
    static member averageofFContextMap(graph : Directed.FContextMap<'NodeKey,'NodeData,'EdgeData>) = 
        graph
        |> Directed.FContextMap.mapContexts Directed.FContext.outwardDegree
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
    static member averageofLilMatrix(graph : Directed.LilMatrix<'NodeKey, 'NodeData, 'EdgeData>) =
        graph.OutEdges
        |> Seq.averageBy(fun x -> float x.Count)
   
    /// <summary> 
    /// Get the mean out-degree of the graph. 
    /// This is an undirected measure so inbound links add to a nodes out-degree.
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the mean out-degree</returns>
    static member average(graph : Directed.LilMatrix<'NodeKey, 'NodeData, 'EdgeData>) =
        OutDegree.averageofLilMatrix graph

    /// <summary> 
    /// Get the mean out-degree of the graph. 
    /// This is an undirected measure so inbound links add to a nodes out-degree.
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the mean out-degree</returns>
    static member average (graph : Directed.FContextMap<'NodeKey,'NodeData,'EdgeData>) =
        OutDegree.averageofFContextMap graph

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
    static member maximumOfFContextMap (graph : Directed.FContextMap<'NodeKey,'NodeData,'EdgeData>) = 
        graph 
        |> Directed.FContextMap.mapContexts Directed.FContext.outwardDegree
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
    static member maximumOfLilMatrix (graph : Directed.LilMatrix<'NodeKey, 'NodeData, 'EdgeData>) =
        graph.OutEdges
        |> ResizeArray.map(fun x -> x.Count)
        |> ResizeArray.max

    /// <summary> 
    /// Get the max out-degree of the graph. 
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An int of the max out-degree</returns>
    static member maximum (graph : Directed.FContextMap<'NodeKey,'NodeData,'EdgeData>) = 
        OutDegree.maximumOfFContextMap graph

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
    static member maximum (graph : Directed.LilMatrix<'NodeKey, 'NodeData, 'EdgeData>) =
        OutDegree.maximumOfLilMatrix graph
    
    
    
    
    // Get Min Degree




    /// <summary> 
    /// Get the min out-degree of the graph. 
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An int of the min out-degree</returns>
    static member minimumOfFContextMap (graph : Directed.FContextMap<'NodeKey,'NodeData,'EdgeData>) = 
        graph 
        |> Directed.FContextMap.mapContexts Directed.FContext.outwardDegree
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
    static member minimumOfLilMatrix (graph : Directed.LilMatrix<'NodeKey, 'NodeData, 'EdgeData>) = 
        graph.OutEdges
        |> ResizeArray.minBy(fun x -> x.Count)
        |> ResizeArray.length


    /// <summary> 
    /// Get the min out-degree of the graph. 
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An int of the min out-degree</returns>
    static member minimum (graph : Directed.FContextMap<'NodeKey,'NodeData,'EdgeData>) = 
        OutDegree.minimumOfFContextMap graph

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
    static member minimum (graph : Directed.LilMatrix<'NodeKey, 'NodeData, 'EdgeData>) =
        OutDegree.minimumOfLilMatrix graph
