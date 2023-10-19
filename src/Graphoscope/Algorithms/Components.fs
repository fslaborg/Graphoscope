namespace Graphoscope.Algorithms

open Graphoscope
open System.Collections.Generic

type Components() =

    static member getComponentOfAdjGraphNode (graph:AdjGraph<'NodeKey,'NodeData,'EdgeData>) (nodeID:'NodeKey) :AdjGraph<'NodeKey,'NodeData,'EdgeData> =
        let nodesInComponent = 
            Algorithms.BFS.ofAdjGraph nodeID graph
            |> Seq.map(fst)
            |> Set.ofSeq
        graph
        |> AdjGraph.toSeq
        |> Seq.filter(fun (nk1,nd1,nk2,nd2,w) -> 
            Set.contains nk1 nodesInComponent && Set.contains nk2 nodesInComponent
        )
        |> AdjGraph.ofSeq

    static member getGraphComponentsOfAdjGraph (graph:AdjGraph<'NodeKey,'NodeData,'EdgeData>) :seq<AdjGraph<'NodeKey,'NodeData,'EdgeData>>=
        let rec getSubgraph toVisitSet (bfElementSet:Set<Set<'NodeKey>>) =
            if Set.count toVisitSet = 0 then
                bfElementSet
            else
                let node = Set.minElement toVisitSet
                let bfElements =
                    Algorithms.BFS.ofAdjGraph node graph
                    |> Seq.map(fst)
                    |> Set.ofSeq
                let reducedToVisit = Set.difference toVisitSet bfElements
                getSubgraph reducedToVisit (Set.add (bfElements) bfElementSet)
        let bfNodes = getSubgraph (Set.ofSeq graph.Keys) Set.empty
        bfNodes
        |> Seq.map(fun x -> 
            graph
            |> AdjGraph.toSeq
            |> Seq.filter(fun (nk1,nd1,nk2,nd2,w) -> 
                Set.contains nk1 x && Set.contains nk2 x
            )
            |> AdjGraph.ofSeq 
        )
        
    static member getBiggestComponentOfAdjGraph (graph:AdjGraph<'NodeKey,'NodeData,'EdgeData>) =
        graph
        |> Components.getGraphComponentsOfAdjGraph 
        |> Seq.maxBy (fun x -> AdjGraph.countNodes x)     
    

    /// DiGraph

    /// <summary> 
    /// Returns true if all nodes in the graph are weakly connected into one component.
    /// </summary>
    /// <param name="graph">The graph to analyse</param> 
    /// <returns>Returns true or false</returns>
    static member isWeakComponentOfDiGraph (g: DiGraph<'NodeKey, 'NodeData, 'EdgeData>) =  
        if  (DFS.ofDiGraphUndirected (g.NodeKeys |> Seq.head) g
            |> Seq.length) = g.NodeKeys .Count then true
                else false


    /// <summary> 
    /// Finds seperate weakly connected components of the graph and returns sets of nodes
    /// </summary>
    /// <param name="graph">The graph to analyse</param> 
    /// <returns>returns set of sets of nodes making up each component.</returns>
    static member getWeakComponentsOfDiGraph (g: DiGraph<'NodeKey, 'NodeData, 'EdgeData>) = 
        g.NodeKeys
        |> Seq.map(fun k -> DFS.ofDiGraphUndirected  k g |> Set.ofSeq)
        |> Set.ofSeq

    /// <summary> 
    /// Finds the largest weakly connected component and returns it's size.
    /// </summary>
    /// <param name="graph">The graph to analyse</param> 
    /// <returns>returns an int indicating numner of nodes</returns>
    static member getLargestWeakComponentSizeOfDiGraph (g: DiGraph<'NodeKey, 'NodeData, 'EdgeData>)= 
        g
        |> Components.getWeakComponentsOfDiGraph
        |> Set.map(fun s -> s.Count)
        |> Set.toSeq
        |> Seq.max

    /// <summary> 
    /// Finds the largest weakly connected component and returns it as a new graph
    /// </summary>
    /// <param name="graph">The graph to analyse</param> 
    /// <returns>returns a new graph</returns>
    static member getLargestWeakComponentOfDiGraph (g: DiGraph<'NodeKey, 'NodeData, 'EdgeData>)= 
        g
        |> Components.getWeakComponentsOfDiGraph
        |> Seq.sortByDescending(fun c -> c |> Set.count)
        |> Seq.head
        |> fun c -> 
            DiGraph.empty
            |> DiGraph.addNodes (c |> Set.toArray)
            |> DiGraph.addEdges (
                    DiGraph.getAllEdges g 
                    |> Array.filter(fun (f,t,_) -> 
                        (c |> Set.map fst).Contains f && (c |> Set.map fst).Contains t)
                        )


    