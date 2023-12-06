namespace Graphoscope.Measures

open Graphoscope
open Graphoscope.Graphs
open FSharpAux

type InDegree() =
        



    //Get Distribution
    
    
    
    
    /// <summary> 
    /// Returns the in-degree sequence of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float seq of in-degree values</returns>
    static member sequenceOfFContextMap(graph : Directed.FContextMap<'NodeKey,'NodeData,'EdgeData>) = 
        graph
        |> Directed.FContextMap.mapContexts Directed.FContext.inwardDegree
        |> Seq.map (fun (_,d) -> float d)
        |> Seq.sortDescending

    /// <summary> 
    /// Returns the in-degree sequence of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float seq of in-degree values</returns>
    static member sequenceOfAdjGraph (graph : AdjGraph<'NodeKey,'NodeData,'EdgeData>) = 
        Degree.sequenceOfAdjGraph graph

    /// <summary> 
    /// Returns the in-degree sequence of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An array of in-degree values in descending order</returns>
    static member sequenceOfLilMatrix(graph : Directed.LilMatrix<'NodeKey, 'NodeData, 'EdgeData>) = 
        graph.InEdges
        |> ResizeArray.map(fun x -> x.Count)
        |> ResizeArray.toArray
        |> Array.sortDescending


    /// <summary> 
    /// Returns the in-degree sequence of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float seq of in-degree values</returns>
    static member sequence(graph : Directed.FContextMap<'NodeKey,'NodeData,'EdgeData>) = 
        InDegree.sequenceOfFContextMap graph

    /// <summary> 
    /// Returns the in-degree sequence of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float seq of in-degree values</returns>
    static member sequence(graph : AdjGraph<'NodeKey,'NodeData,'EdgeData>) = 
        InDegree.sequenceOfAdjGraph graph

    /// <summary> 
    /// Returns the in-degree sequence of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An array of in-degree values in descending order</returns>
    static member sequence(graph : Directed.LilMatrix<'NodeKey, 'NodeData, 'EdgeData>) = 
        InDegree.sequenceOfLilMatrix graph




    //Get Average




    /// <summary> 
    /// Get the mean In-degree of the graph. 
    /// This is an undirected measure so inbound links add to a nodes In-degree.
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the mean In-degree</returns>
    static member averageofFContextMap(graph : Directed.FContextMap<'NodeKey,'NodeData,'EdgeData>) = 
        graph
        |> Directed.FContextMap.mapContexts Directed.FContext.inwardDegree
        |> Seq.averageBy (fun (_,d) -> float d) 

    /// <summary> 
    /// Get the mean In-degree of the graph. 
    /// This is an undirected measure so inbound links add to a nodes In-degree.
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the mean In-degree</returns>
    static member averageofAdjGraph (graph : AdjGraph<'NodeKey,'NodeData,'EdgeData>) = 
        Degree.averageofAdjGraph graph  

    /// <summary> 
    /// Get the mean In-degree of the graph. 
    /// This is an undirected measure so inbound links add to a nodes In-degree.
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the mean In-degree</returns>
    static member averageofLilMatrix(graph : Directed.LilMatrix<'NodeKey, 'NodeData, 'EdgeData>) =
        graph.InEdges
        |> Seq.averageBy(fun x -> float x.Count)
   
    /// <summary> 
    /// Get the mean In-degree of the graph. 
    /// This is an undirected measure so inbound links add to a nodes In-degree.
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the mean In-degree</returns>
    static member average(graph : Directed.LilMatrix<'NodeKey, 'NodeData, 'EdgeData>) =
        InDegree.averageofLilMatrix graph

    /// <summary> 
    /// Get the mean In-degree of the graph. 
    /// This is an undirected measure so inbound links add to a nodes In-degree.
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the mean In-degree</returns>
    static member average (graph : Directed.FContextMap<'NodeKey,'NodeData,'EdgeData>) =
        InDegree.averageofFContextMap graph

    /// <summary> 
    /// Get the mean In-degree of the graph. 
    /// This is an undirected measure so inbound links add to a nodes In-degree.
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the mean In-degree</returns>
    static member average (graph : AdjGraph<'NodeKey,'NodeData,'EdgeData>) =
        InDegree.averageofAdjGraph graph




    // Get Max Degree




    /// <summary> 
    /// Get the max In-degree of the graph. 
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An int of the max In-degree</returns>
    static member maximumOfFContextMap (graph : Directed.FContextMap<'NodeKey,'NodeData,'EdgeData>) = 
        graph 
        |> Directed.FContextMap.mapContexts Directed.FContext.inwardDegree
        |> Seq.maxBy (fun (_,d) -> float d) 
        |> snd 
    /// <summary> 
    /// Get the max In-degree of the graph. 
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An int of the max In-degree</returns>
    static member maximumOfAdjGraph (graph : AdjGraph<'NodeKey,'NodeData,'EdgeData>) = 
        Degree.maximumOfAdjGraph graph
    
    /// <summary> 
    /// Get the max In-degree of the graph. 
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An int of the max In-degree</returns>
    static member maximumOfLilMatrix (graph : Directed.LilMatrix<'NodeKey, 'NodeData, 'EdgeData>) =
        graph.InEdges
        |> ResizeArray.map(fun x -> x.Count)
        |> ResizeArray.max
   

    /// <summary> 
    /// Get the max In-degree of the graph. 
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An int of the max In-degree</returns>
    static member maximum (graph : Directed.FContextMap<'NodeKey,'NodeData,'EdgeData>) = 
        InDegree.maximumOfFContextMap graph

    /// <summary> 
    /// Get the max In-degree of the graph. 
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An int of the max In-degree</returns>
    static member maximum (graph : AdjGraph<'NodeKey,'NodeData,'EdgeData>) = 
        InDegree.maximumOfAdjGraph graph

    /// <summary> 
    /// Get the max In-degree of the graph. 
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An int of the max In-degree</returns>
    static member maximum (graph : Directed.LilMatrix<'NodeKey, 'NodeData, 'EdgeData>) =
        InDegree.maximumOfLilMatrix graph
    
    
    
    
    // Get Min Degree




    /// <summary> 
    /// Get the min In-degree of the graph. 
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An int of the min In-degree</returns>
    static member minimumOfFContextMap (graph : Directed.FContextMap<'NodeKey,'NodeData,'EdgeData>) = 
        graph 
        |> Directed.FContextMap.mapContexts Directed.FContext.inwardDegree
        |> Seq.minBy (fun (_,d) -> float d) 
        |> snd 

    /// <summary> 
    /// Get the min In-degree of the graph. 
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An int of the min In-degree</returns>
    static member minimumOfAdjGraph (graph : AdjGraph<'NodeKey,'NodeData,'EdgeData>) = 
        Degree.minimumOfAdjGraph graph

    /// <summary> 
    /// Get the min In-degree of the graph. 
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An int of the min In-degree</returns>
    static member minimumOfLilMatrix (graph : Directed.LilMatrix<'NodeKey, 'NodeData, 'EdgeData>) =
        graph.InEdges
        |> ResizeArray.map(fun x -> x.Count)
        |> ResizeArray.max

    /// <summary> 
    /// Get the min In-degree of the graph. 
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An int of the min In-degree</returns>
    static member minimum (graph : Directed.FContextMap<'NodeKey,'NodeData,'EdgeData>) = 
        InDegree.minimumOfFContextMap graph

    /// <summary> 
    /// Get the min In-degree of the graph. 
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An int of the min In-degree</returns>
    static member minimum (graph : AdjGraph<'NodeKey,'NodeData,'EdgeData>) = 
        InDegree.minimumOfAdjGraph graph

    /// <summary> 
    /// Get the min In-degree of the graph. 
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An int of the min In-degree</returns>
    static member minimum (graph : Directed.LilMatrix<'NodeKey, 'NodeData, 'EdgeData>) =
        InDegree.minimumOfLilMatrix graph
