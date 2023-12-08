namespace Graphoscope.Measures
open Graphoscope
open Graphoscope.Graphs

type ClusteringCoefficient() =
    ///Evaluates the clustering coefficient of the vertex.
    static member clusteringCoefficientOfFContextMapVertex (context:Directed.FContext<'NodeKey, 'NodeData, 'EdgeData>) (g: Directed.FContextMap<'NodeKey, 'NodeData, 'EdgeData>) : float=
        context
        |> fun c ->     
            if Directed.FContext.degree c < 2 then 0.
            else        
                let add1IfInSeq acc x set = 
                    if Seq.contains x set then acc + 1
                    else acc
                let neighbours = Directed.FContext.neighbours c|>Seq.map fst
                let neighbourEdges = 
                    Seq.fold (fun edgeAmount v' -> 
                        (g.Item v'
                        |> fun x  ->
                            (Directed.FContext.predecessors x|>Seq.map fst
                            |> Seq.fold (fun acc (x) -> add1IfInSeq acc x neighbours) 0))
                        + edgeAmount
                    ) 0 neighbours
                let degree = Seq.length neighbours
                ((float neighbourEdges) / (float (degree * (degree - 1)))) / 2.
    
    static member clusteringCoefficientOfFContextMap (g: Directed.FContextMap<'NodeKey, 'NodeData, 'EdgeData>) : float=
        g
        |> Directed.FContextMap.mapContexts (fun c -> ClusteringCoefficient.clusteringCoefficientOfFContextMapVertex c g)
        |> Seq.sumBy snd

    static member clusteringCoefficientOfUndirectedFContextMapNode (n:'NodeKey) (g: Undirected.FContextMapU<'NodeKey, 'NodeData, 'EdgeData>) : float=  
        if (Undirected.FContext.degree g.[n]) < 2 then 
            0.
        else        
            let add1IfInSeq acc x set = 
                if Seq.contains x set then 
                    acc + 1
                else 
                    acc
            let neighbours = Undirected.FContext.neighbours g.[n] |>Seq.map fst
            let neighbourEdges = 
                Seq.fold (fun edgeAmount v' -> 
                    (Undirected.FContext.neighbours g.[v']
                    |> fun (p) -> 
                        (p|>Seq.map fst
                        |> Seq.fold (fun acc (x) -> add1IfInSeq acc x neighbours) 0))
                    + edgeAmount
                ) 0 neighbours
            let degree = Seq.length neighbours
            ((float neighbourEdges) / (float (degree * (degree - 1)))) / 2.

    static member clusteringCoefficientOfUndirectedFContextMap (g: Undirected.FContextMapU<'NodeKey, 'NodeData, 'EdgeData>) : float=
        g.Keys
        |> Seq.map (fun c -> ClusteringCoefficient.clusteringCoefficientOfUndirectedFContextMapNode c g)
        |> Seq.sum
    static member clusteringCoefficientOfLilMatrix (g: Directed.LilMatrix<'NodeKey, 'NodeData, 'EdgeData>) : float=
        System.NotImplementedException() |> raise
    
    static member clusteringCoefficientOfUndirectedGraph (g: Undirected.UndirectedGraph<'NodeKey, 'NodeData, 'EdgeData>) : float=
        System.NotImplementedException() |> raise
    
    static member compute (g: Directed.FContextMap<'NodeKey, 'NodeData, 'EdgeData>) =
        ClusteringCoefficient.clusteringCoefficientOfFContextMap g
        
    static member computeUndirected (g: Undirected.FContextMapU<'NodeKey, 'NodeData, 'EdgeData>) =
        ClusteringCoefficient.clusteringCoefficientOfUndirectedFContextMap g
    
    static member compute (g: Directed.LilMatrix<'NodeKey, 'NodeData, 'EdgeData>) =
        ClusteringCoefficient.clusteringCoefficientOfLilMatrix g
    
    static member compute (g: Undirected.UndirectedGraph<'NodeKey, 'NodeData, 'EdgeData>) =
        ClusteringCoefficient.clusteringCoefficientOfUndirectedGraph g
    