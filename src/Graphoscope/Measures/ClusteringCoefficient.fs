namespace Graphoscope.Measures
open Graphoscope

type ClusteringCoefficient() =
    ///Evaluates the clustering coefficient of the vertex.
    static member clusteringCoefficientOfFGraphVertex (context:FContext<'NodeKey, 'NodeData, 'EdgeData>) (g: FGraph<'NodeKey, 'NodeData, 'EdgeData>) : float=
        context
        |> fun c ->     
            if FContext.degree c < 2 then 0.
            else        
                let add1IfInSeq acc x set = 
                    if Seq.contains x set then acc + 1
                    else acc
                let neighbours = FContext.neighbours c|>Seq.map fst
                let neighbourEdges = 
                    Seq.fold (fun edgeAmount v' -> 
                        (g.Item v'
                        |> fun x  ->
                            (FContext.predecessors x|>Seq.map fst
                            |> Seq.fold (fun acc (x) -> add1IfInSeq acc x neighbours) 0))
                        + edgeAmount
                    ) 0 neighbours
                let degree = Seq.length neighbours
                ((float neighbourEdges) / (float (degree * (degree - 1)))) / 2.
    
    static member clusteringCoefficientOfFGraph (g: FGraph<'NodeKey, 'NodeData, 'EdgeData>) : float=
        g
        |> FGraph.mapContexts (fun c -> ClusteringCoefficient.clusteringCoefficientOfFGraphVertex c g)
        |> Seq.sumBy snd

     static member clusteringCoefficientOfAdjGraphNode (n:'NodeKey) (g: AdjGraph<'NodeKey, 'NodeData, 'EdgeData>) : float=  
         if (AdjGraph.getDegree g n) < 2 then 0.
         else        
             let add1IfInSeq acc x set = 
                 if Seq.contains x set then acc + 1
                 else acc
             let neighbours = AdjGraph.getNeighbours n g|>Seq.map fst
             let neighbourEdges = 
                 Seq.fold (fun edgeAmount v' -> 
                     (AdjGraph.getNeighbours v' g
                     |> fun (p) -> 
                         (p|>Seq.map fst
                         |> Seq.fold (fun acc (x) -> add1IfInSeq acc x neighbours) 0))
                     + edgeAmount
                 ) 0 neighbours
             let degree = Seq.length neighbours
             ((float neighbourEdges) / (float (degree * (degree - 1)))) / 2.

     static member clusteringCoefficientOfAdjGraph (g: AdjGraph<'NodeKey, 'NodeData, 'EdgeData>) : float=
         g.Keys
         |> Seq.map (fun c -> ClusteringCoefficient.clusteringCoefficientOfAdjGraphNode c g)
         |> Seq.sum
    static member clusteringCoefficientOfDiGraph (g: DiGraph<'NodeKey, 'NodeData, 'EdgeData>) : float=
        System.NotImplementedException() |> raise
    
    static member clusteringCoefficientOfUndirectedGraph (g: UndirectedGraph<'NodeKey, 'NodeData, 'EdgeData>) : float=
        System.NotImplementedException() |> raise
    
    static member compute (g: FGraph<'NodeKey, 'NodeData, 'EdgeData>) =
        ClusteringCoefficient.clusteringCoefficientOfFGraph g
    
    static member compute (g: DiGraph<'NodeKey, 'NodeData, 'EdgeData>) =
        ClusteringCoefficient.clusteringCoefficientOfDiGraph g
    
    static member compute (g: UndirectedGraph<'NodeKey, 'NodeData, 'EdgeData>) =
        ClusteringCoefficient.clusteringCoefficientOfUndirectedGraph g
    