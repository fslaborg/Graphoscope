namespace Graphoscope.Measures

open Graphoscope
open Graphoscope.Graphs
open FSharpAux

type Degree() =
    



    //Get Distribution




    /// <summary> 
    /// Returns the degree sequence of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float seq of degree values</returns>
    static member sequenceOfFContextMap(graph : Directed.FContextMap<'NodeKey,'NodeData,'EdgeData>) = 
        graph
        |> Directed.FContextMap.mapContexts Directed.FContext.degree
        |> Seq.map (fun (_,d) -> float d)
        |> Seq.sortDescending
    
    /// <summary> 
    /// Returns the degree sequence of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float seq of out-degree values</returns>
    static member sequenceOfUndirectedFContextMap(graph : Undirected.FContextMapU<'NodeKey,'NodeData,'EdgeData>) = 
        graph.Keys
        |> Seq.map(fun k -> 
            Undirected.FContext.degree graph.[k]
            |> float
        )
        |> Seq.sortDescending

    /// <summary> 
    /// Returns the degree sequence of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An array of degree values in descending order</returns>
    static member sequenceOfLilMatrix(graph : Directed.LilMatrix<'NodeKey, 'NodeData, 'EdgeData>) =
        Array.init graph.NodeKeys.Count (fun i -> graph.InEdges[i].Count + graph.OutEdges[i].Count)
        |> Array.sortDescending

    /// <summary> 
    /// Returns the degree sequence of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An array of degree values in descending order</returns>
    static member sequenceOfUndirected(graph : Undirected.UndirectedGraph<'NodeKey, 'NodeData, 'EdgeData>) =
        Array.init graph.NodeKeys.Count (fun i -> graph.Edges[i].Count)
        |> Array.sortDescending

    /// <summary> 
    /// Returns the degree sequence of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An array of degree values in descending order</returns>
    static member sequence(graph : Undirected.UndirectedGraph<'NodeKey, 'NodeData, 'EdgeData>) = 
        Degree.sequenceOfUndirected graph


    /// <summary> 
    /// Returns the degree sequence of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An array of degree values in descending order</returns>
    static member sequence(graph : Directed.LilMatrix<'NodeKey, 'NodeData, 'EdgeData>) = 
        Degree.sequenceOfLilMatrix graph

    /// <summary> 
    /// Returns the degree sequence of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float seq of degree values</returns>
    static member sequence(graph : Directed.FContextMap<'NodeKey,'NodeData,'EdgeData>) =
        Degree.sequenceOfFContextMap graph

    /// <summary> 
    /// Returns the degree sequence of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float seq of degree values</returns>
    static member sequenceUndirected(graph : Undirected.FContextMapU<'NodeKey,'NodeData,'EdgeData>) =
        Degree.sequenceOfUndirectedFContextMap graph



    //Get Average




    /// <summary> 
    /// Get the mean degree of the graph. 
    /// This is an undirected measure so inbound links add to a nodes degree.
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the mean degree</returns>
    static member averageofFContextMap(graph : Directed.FContextMap<'NodeKey,'NodeData,'EdgeData>) = 
        graph
        |> Directed.FContextMap.mapContexts Directed.FContext.degree
        |> Seq.averageBy (fun (_,d) -> float d) 

    /// <summary> 
    /// Get the mean degree of the graph. 
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the mean out-degree</returns>
    static member averageOfUndirectedFContextMap(graph : Undirected.FContextMapU<'NodeKey,'NodeData,'EdgeData>) = 
        graph.Keys
        |> Seq.map(fun k -> 
            Undirected.FContext.degree graph.[k]
            |> float
        )
        |> Seq.average

    /// <summary> 
    /// Get the mean degree of the graph. 
    /// This is an undirected measure so inbound links add to a nodes degree.
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the mean degree</returns>
    static member averageofLilMatrix(graph : Directed.LilMatrix<'NodeKey, 'NodeData, 'EdgeData>) =
        Degree.sequenceOfLilMatrix graph
        |> Array.averageBy float

    /// <summary> 
    /// Get the mean degree of the graph. 
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the mean degree</returns>
    static member averageofUndirected(graph : Undirected.UndirectedGraph<'NodeKey, 'NodeData, 'EdgeData>) =
        Degree.sequenceOfUndirected graph
        |> Array.averageBy float
   
    /// <summary> 
    /// Get the mean degree of the graph. 
    /// This is an undirected measure so inbound links add to a nodes degree.
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the mean degree</returns>
    static member average(graph : Directed.LilMatrix<'NodeKey, 'NodeData, 'EdgeData>) =
        Degree.averageofLilMatrix graph

    /// <summary> 
    /// Get the mean degree of the graph. 
    /// This is an undirected measure so inbound links add to a nodes degree.
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the mean degree</returns>
    static member average (graph : Directed.FContextMap<'NodeKey,'NodeData,'EdgeData>) =
        Degree.averageofFContextMap graph

    /// <summary> 
    /// Get the mean degree of the graph. 
    /// This is an undirected measure so inbound links add to a nodes degree.
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the mean degree</returns>
    static member averageUndirected (graph : Undirected.FContextMapU<'NodeKey,'NodeData,'EdgeData>) =
        Degree.averageOfUndirectedFContextMap graph

    /// <summary> 
    /// Get the mean degree of the graph. 
    /// This is an undirected measure so inbound links add to a nodes degree.
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the mean degree</returns>
    static member average (graph : Undirected.UndirectedGraph<'NodeKey, 'NodeData, 'EdgeData>) =
        Degree.averageofUndirected graph


    // Get Max Degree




    /// <summary> 
    /// Get the max degree of the graph. 
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns> An int of the max degree</returns>
    static member maximumOfFContextMap (graph : Directed.FContextMap<'NodeKey,'NodeData,'EdgeData>) = 
        graph 
        |> Directed.FContextMap.mapContexts Directed.FContext.degree
        |> Seq.maxBy (fun (_,d) -> float d) 
        |> snd 

    /// <summary> 
    /// Get the max degree of the graph. 
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An int of the max degree</returns>
    static member maximumOfUndirected (graph : Undirected.UndirectedGraph<'NodeKey, 'NodeData, 'EdgeData>) =
        Degree.sequenceOfUndirected graph
        |> Array.head

    /// <summary> 
    /// Get the max degree of the graph. 
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An int of the max degree</returns>
    static member maximumOfUndirectedFContextMap (graph : Undirected.FContextMapU<'NodeKey,'NodeData,'EdgeData>) = 
        graph.Keys
        |> Seq.map(fun k -> 
            Undirected.FContext.degree graph.[k]
            |> float
        )
        |> Seq.max
        |> int

    /// <summary> 
    /// Get the max degree of the graph. 
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An int of the max degree</returns>
    static member maximumOfLilMatrix (graph : Directed.LilMatrix<'NodeKey, 'NodeData, 'EdgeData>) =
        Degree.sequenceOfLilMatrix graph
        |> Array.head

    /// <summary> 
    /// Get the max degree of the graph. 
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An int of the max degree</returns>
    static member maximum (graph : Directed.FContextMap<'NodeKey,'NodeData,'EdgeData>) = 
        Degree.maximumOfFContextMap graph

    /// <summary> 
    /// Get the max degree of the graph. 
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An int of the max degree</returns>
    static member maximum (graph : Undirected.UndirectedGraph<'NodeKey, 'NodeData, 'EdgeData>) =
        Degree.maximumOfUndirected graph

    /// <summary> 
    /// Get the max degree of the graph. 
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An int of the max degree</returns>
    static member maximumUndirected (graph : Undirected.FContextMapU<'NodeKey,'NodeData,'EdgeData>) = 
        Degree.maximumOfUndirectedFContextMap graph

    /// <summary> 
    /// Get the max degree of the graph. 
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An int of the max degree</returns>
    static member maximum (graph : Directed.LilMatrix<'NodeKey, 'NodeData, 'EdgeData>) =
        Degree.maximumOfLilMatrix graph




    // Get Min Degree




    /// <summary> 
    /// Get the min degree of the graph. 
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An int of the min degree</returns>
    static member minimumOfFContextMap (graph : Directed.FContextMap<'NodeKey,'NodeData,'EdgeData>) = 
        graph 
        |> Directed.FContextMap.mapContexts Directed.FContext.degree
        |> Seq.minBy (fun (_,d) -> float d) 
        |> snd 

    /// <summary> 
    /// Get the min degree of the graph. 
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An int of the min degree</returns>
    static member minimumOfUndirected (graph : Undirected.UndirectedGraph<'NodeKey, 'NodeData, 'EdgeData>) =
        Degree.sequenceOfUndirected graph
        |> Array.last


    /// <summary> 
    /// Get the min degree of the graph. 
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An int of the min degree</returns>
    static member minimumOfUndirectedFContextMap (graph : Undirected.FContextMapU<'NodeKey,'NodeData,'EdgeData>) = 
        graph.Keys
        |> Seq.map(fun k -> 
            Undirected.FContext.degree graph.[k]
            |> float
        )
        |> Seq.min
        |> int

    /// <summary> 
    /// Get the min degree of the graph. 
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An int of the min degree</returns>
    static member minimumOfLilMatrix (graph : Directed.LilMatrix<'NodeKey, 'NodeData, 'EdgeData>) =
        Degree.sequenceOfLilMatrix graph
        |> Array.last


    /// <summary> 
    /// Get the min degree of the graph. 
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An int of the min degree</returns>
    static member minimum (graph : Directed.FContextMap<'NodeKey,'NodeData,'EdgeData>) = 
        Degree.minimumOfFContextMap graph

    /// <summary> 
    /// Get the min degree of the graph. 
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An int of the min degree</returns>
    static member minimum (graph : Undirected.UndirectedGraph<'NodeKey, 'NodeData, 'EdgeData>) =
        Degree.minimumOfUndirected graph

    /// <summary> 
    /// Get the min degree of the graph. 
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An int of the min degree</returns>
    static member minimumUndirected (graph : Undirected.FContextMapU<'NodeKey,'NodeData,'EdgeData>) = 
        Degree.minimumOfUndirectedFContextMap graph

    /// <summary> 
    /// Get the min degree of the graph. 
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An int of the min degree</returns>
    static member minimum (graph : Directed.LilMatrix<'NodeKey, 'NodeData, 'EdgeData>) =
        Degree.minimumOfLilMatrix graph

    /// <summary> 
    /// Get the weighted degree of the node with the 'NodeKey nk of the graph. 
    /// </summary>
    /// <param name="nk">The NodeKey to get the weighted Degree</param> 
    /// <param name="weightF">Function to get a float weight of the EdgeData</param> 
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the weighted degree of a node</returns>
    static member weightedDegreeOfFContextMapNode (nk:'NodeKey) (weightF:'EdgeData->float) (graph : Directed.FContextMap<'NodeKey,'NodeData,'EdgeData>) = 
        let (p, _, s) = 
            graph.Item nk 
        let pDegreeWeighted = 
            p.Values|>Seq.sumBy(fun edgeData -> weightF edgeData)
        let sDegreeWeighted = 
            s.Values|>Seq.sumBy(fun edgeData -> weightF edgeData)
        pDegreeWeighted+sDegreeWeighted

    /// <summary> 
    /// Return the cummulative Degree of a FContextMap sorted from smallest degree to highest as tupel of degree and Nodecount with that degree.
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the weighted degree of a node</returns>
    static member cumulativeDegreeOfFContextMap (graph : Directed.FContextMap<'NodeKey,'NodeData,'EdgeData>) = 
        graph
        |> Directed.FContextMap.mapContexts Directed.FContext.degree
        |> Seq.groupBy (fun (_,d) -> float d)
        |> Seq.map (fun (degree,members) -> 
            degree,
            Seq.length members
        )
        |> Seq.sortBy fst


