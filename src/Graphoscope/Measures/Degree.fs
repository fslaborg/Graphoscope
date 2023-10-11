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
    /// <returns>A float seq of out-degree values</returns>
    static member sequenceOfAdjGraph(graph : AdjGraph<'NodeKey,'NodeData,'EdgeData>) = 
        graph.Keys
        |> Seq.map(fun k -> 
            AdjGraph.getDegree graph k
            |> float
        )
        |> Seq.sortDescending

    /// <summary> 
    /// Returns the degree sequence of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An array of degree values in descending order</returns>
    static member sequenceOfDiGraph(graph : DiGraph<'NodeKey, 'NodeData, 'EdgeData>) =
        Array.init graph.NodeKeys.Count (fun i -> graph.InEdges[i].Count + graph.OutEdges[i].Count)
        |> Array.sortDescending

    /// <summary> 
    /// Returns the degree sequence of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An array of degree values in descending order</returns>
    static member sequenceOfUndirected(graph : UndirectedGraph<'NodeKey, 'EdgeData>) =
        Array.init graph.NodeKeys.Count (fun i -> graph.Edges[i].Count)
        |> Array.sortDescending

    /// <summary> 
    /// Returns the degree sequence of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An array of degree values in descending order</returns>
    static member sequence(graph : UndirectedGraph<'NodeKey, 'EdgeData>) = 
        Degree.sequenceOfUndirected graph


    /// <summary> 
    /// Returns the degree sequence of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An array of degree values in descending order</returns>
    static member sequence(graph : DiGraph<'NodeKey, 'NodeData, 'EdgeData>) = 
        Degree.sequenceOfDiGraph graph

    /// <summary> 
    /// Returns the degree sequence of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float seq of degree values</returns>
    static member sequence(graph : FGraph<'NodeKey,'NodeData,'EdgeData>) =
        Degree.sequenceOfFGraph graph

    /// <summary> 
    /// Returns the degree sequence of the graph
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float seq of degree values</returns>
    static member sequence(graph : AdjGraph<'NodeKey,'NodeData,'EdgeData>) =
        Degree.sequenceOfAdjGraph graph



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
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the mean out-degree</returns>
    static member averageofAdjGraph(graph : AdjGraph<'NodeKey,'NodeData,'EdgeData>) = 
        graph.Keys
        |> Seq.map(fun k -> 
            AdjGraph.getDegree graph k
            |> float
        )
        |> Seq.average

    /// <summary> 
    /// Get the mean degree of the graph. 
    /// This is an undirected measure so inbound links add to a nodes degree.
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the mean degree</returns>
    static member averageofDiGraph(graph : DiGraph<'NodeKey, 'NodeData, 'EdgeData>) =
        Degree.sequenceOfDiGraph graph
        |> Array.averageBy float

    /// <summary> 
    /// Get the mean degree of the graph. 
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the mean degree</returns>
    static member averageofUndirected(graph : UndirectedGraph<'NodeKey, 'EdgeData>) =
        Degree.sequenceOfUndirected graph
        |> Array.averageBy float
   
    /// <summary> 
    /// Get the mean degree of the graph. 
    /// This is an undirected measure so inbound links add to a nodes degree.
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the mean degree</returns>
    static member average(graph : DiGraph<'NodeKey, 'NodeData, 'EdgeData>) =
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
    static member average (graph : AdjGraph<'NodeKey,'NodeData,'EdgeData>) =
        Degree.averageofAdjGraph graph

    /// <summary> 
    /// Get the mean degree of the graph. 
    /// This is an undirected measure so inbound links add to a nodes degree.
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the mean degree</returns>
    static member average (graph : UndirectedGraph<'NodeKey,'EdgeData>) =
        Degree.averageofUndirected graph


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
    static member maximumOfUndirected (graph : UndirectedGraph<'NodeKey, 'EdgeData>) =
        Degree.sequenceOfUndirected graph
        |> Array.head

    /// <summary> 
    /// Get the max degree of the graph. 
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An int of the max degree</returns>
    static member maximumOfAdjGraph (graph : AdjGraph<'NodeKey,'NodeData,'EdgeData>) = 
        graph.Keys
        |> Seq.map(fun k -> 
            AdjGraph.getDegree graph k
            |> float
        )
        |> Seq.max
        |> int

    /// <summary> 
    /// Get the max degree of the graph. 
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An int of the max degree</returns>
    static member maximumOfDiGraph (graph : DiGraph<'NodeKey, 'NodeData, 'EdgeData>) =
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
    static member maximum (graph : UndirectedGraph<'NodeKey, 'EdgeData>) =
        Degree.maximumOfUndirected graph

    /// <summary> 
    /// Get the max degree of the graph. 
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An int of the max degree</returns>
    static member maximum (graph : AdjGraph<'NodeKey,'NodeData,'EdgeData>) = 
        Degree.maximumOfAdjGraph graph

    /// <summary> 
    /// Get the max degree of the graph. 
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An int of the max degree</returns>
    static member maximum (graph : DiGraph<'NodeKey, 'NodeData, 'EdgeData>) =
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
    static member minimumOfUndirected (graph : UndirectedGraph<'NodeKey, 'EdgeData>) =
        Degree.sequenceOfUndirected graph
        |> Array.last


    /// <summary> 
    /// Get the min degree of the graph. 
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An int of the min degree</returns>
    static member minimumOfAdjGraph (graph : AdjGraph<'NodeKey,'NodeData,'EdgeData>) = 
        graph.Keys
        |> Seq.map(fun k -> 
            AdjGraph.getDegree graph k
            |> float
        )
        |> Seq.min
        |> int

    /// <summary> 
    /// Get the min degree of the graph. 
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An int of the min degree</returns>
    static member minimumOfDiGraph (graph : DiGraph<'NodeKey, 'NodeData, 'EdgeData>) =
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
    static member minimum (graph : UndirectedGraph<'NodeKey, 'EdgeData>) =
        Degree.minimumOfUndirected graph

    /// <summary> 
    /// Get the min degree of the graph. 
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An int of the min degree</returns>
    static member minimum (graph : AdjGraph<'NodeKey,'NodeData,'EdgeData>) = 
        Degree.minimumOfAdjGraph graph

    /// <summary> 
    /// Get the min degree of the graph. 
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>An int of the min degree</returns>
    static member minimum (graph : DiGraph<'NodeKey, 'NodeData, 'EdgeData>) =
        Degree.minimumOfDiGraph graph

    /// <summary> 
    /// Get the weighted degree of the node with the 'NodeKey nk of the graph. 
    /// </summary>
    /// <param name="nk">The NodeKey to get the weighted Degree</param> 
    /// <param name="weightF">Function to get a float weight of the EdgeData</param> 
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the weighted degree of a node</returns>
    static member weightedDegreeOfFGraphNode (nk:'NodeKey) (weightF:'EdgeData->float) (graph : FGraph<'NodeKey,'NodeData,'EdgeData>) = 
        let (p, _, s) = 
            graph.Item nk 
        let pDegreeWeighted = 
            p.Values|>Seq.sumBy(fun edgeData -> weightF edgeData)
        let sDegreeWeighted = 
            s.Values|>Seq.sumBy(fun edgeData -> weightF edgeData)
        pDegreeWeighted+sDegreeWeighted

    /// <summary> 
    /// Return the cummulative Degree of a FGraph sorted from smallest degree to highest as tupel of degree and Nodecount with that degree.
    /// </summary>
    /// <param name="graph">The graph to be analysed</param> 
    /// <returns>A float of the weighted degree of a node</returns>
    static member cumulativeDegreeOfFGraph (graph : FGraph<'NodeKey,'NodeData,'EdgeData>) = 
        graph
        |> FGraph.mapContexts FContext.degree
        |> Seq.groupBy (fun (_,d) -> float d)
        |> Seq.map (fun (degree,members) -> 
            degree,
            Seq.length members
        )
        |> Seq.sortBy fst


