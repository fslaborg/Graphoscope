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
    