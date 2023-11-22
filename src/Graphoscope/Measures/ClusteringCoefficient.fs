namespace Graphoscope.Measures
open Graphoscope
open FSharpAux
open System

[<AutoOpen>]
module private Helpers =
    module LocalClusteringCoefficient =
        let ofUndirected (g: UndirectedGraph<'NodeKey, 'NodeData, 'EdgeData>) (nIx: int): float =
            let neighbors = g.Edges[nIx] |> ResizeArray.map fst |> ResizeArray.filter(fun nbrIx -> nIx <> nbrIx)
            if
                neighbors.Count < 2
            then 0.
            else
                let potentialClosingEdges =
                    // This can be optimized by taking combinations instead of permuting neighbors.
                    Seq.allPairs neighbors neighbors
                    |> Seq.map(fun (n1,n2) -> Math.Min(n1,n2), Math.Max(n1,n2))
                    |> Seq.distinct
                    |> Seq.filter(fun (x, y) -> x <> y)

                let existingClosingEdges =
                    (0, potentialClosingEdges)
                    ||> Seq.fold(fun acc (x,y) ->
                        g.Edges[x]
                        |> ResizeArray.tryFind(fun (dest,_) -> y = dest)
                        |> Option.map(fun _ -> acc + 1)
                        |> Option.defaultValue acc
                    )

                float existingClosingEdges / (float (Seq.length potentialClosingEdges))

        let ofDiGraph (g: DiGraph<'NodeKey, 'NodeData, 'EdgeData>) (nIx: int): float =
            let outNeighbors = g.OutEdges[nIx] |> ResizeArray.map fst |> ResizeArray.filter(fun nbrIx -> nIx <> nbrIx)
            let inNeighbors = g.InEdges[nIx] |> ResizeArray.map fst |> ResizeArray.filter(fun nbrIx -> nIx <> nbrIx)
            if
                outNeighbors.Count + inNeighbors.Count < 2
            then 0.
            else
                let potentialClosingEdges =
                    // This can be optimized by taking combinations instead of permuting neighbors.
                    Seq.allPairs outNeighbors inNeighbors
                    |> Seq.filter(fun (x, y) -> x <> y)

                let existingClosingEdges =
                    (0, potentialClosingEdges)
                    ||> Seq.fold(fun acc (x,y) ->
                        g.OutEdges[x]
                        |> ResizeArray.tryFind(fun (dest,_) -> y = dest)
                        |> Option.map(fun _ -> acc + 1)
                        |> Option.defaultValue acc
                    )

                float existingClosingEdges / (float (Seq.length potentialClosingEdges))

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

    static member clusteringCoefficientOfAdjGraphNode (n:'NodeKey) (g: AdjGraph<'NodeKey, 'NodeData, 'EdgeData>) : float =
        if
            (AdjGraph.getDegree g n) < 2
        then 0.
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

    static member localClusteringCoefficientOfUndirected (g: UndirectedGraph<'NodeKey, 'NodeData, 'EdgeData>) (nodeKey: 'NodeKey): float =
        let nIx = g.IdMap[nodeKey]
        LocalClusteringCoefficient.ofUndirected g nIx

    static member localClusteringCoefficientOfDiGraph (g: DiGraph<'NodeKey, 'NodeData, 'EdgeData>) (nodeKey: 'NodeKey): float=
        let nIx = g.IdMap[nodeKey]
        LocalClusteringCoefficient.ofDiGraph g nIx

    static member globalClusteringCoefficientOfDiGraph (g: DiGraph<'NodeKey, 'NodeData, 'EdgeData>): float =
        let triangleClosers =
            g.OutEdges
            |> Seq.mapi(fun org outEdges ->
                let outNbrs =
                    outEdges
                    |> Seq.map fst
                    |> Seq.filter(fun dest -> org <> dest)
                let inNbrs =
                    g.InEdges[org]
                    |> Seq.map fst
                    |> Seq.filter(fun dest -> org <> dest)
                Seq.allPairs outNbrs inNbrs
                |> Seq.filter(fun (n1, n2) -> n1 <> n2)
            )
            |> Seq.concat
            |> Seq.cache

        let closedTriangles =
            (0, triangleClosers)
            ||> Seq.fold(fun acc (orgIx, destIx) ->
                g.OutEdges[orgIx]
                |> ResizeArray.tryFind(fun (dest,_) -> dest = destIx)
                |> Option.map(fun _ -> acc + 1)
                |> Option.defaultValue acc
            )
        float (closedTriangles) / float(triangleClosers|> Seq.length)

    static member localClusteringCoefficientUndirected (nodeKey: 'NodeKey) (g: UndirectedGraph<'NodeKey, 'NodeData, 'EdgeData>): float =
        let nIx = g.IdMap[nodeKey]
        LocalClusteringCoefficient.ofUndirected g nIx

    static member averageClusteringCoefficientUndirected (g: UndirectedGraph<'NodeKey, 'NodeData, 'EdgeData>): float =
        Seq.init g.NodeKeys.Count (LocalClusteringCoefficient.ofUndirected g)
        |> Seq.average

    // Testing:
    // Test: CompleteGraph
    // Test: Graph with two complete components
    // Test: karate = 0.255_682
    // Test (global specific): when each node has a degree of 0 or 1 = nan 
    // Test: where two different nodes form triangles with a pair of other nodes (e.g. 0 and 1 both connected to 2 and 3, individually)
    static member globalClusteringCoefficientOfUndirected (g: UndirectedGraph<'NodeKey, 'NodeData, 'EdgeData>): float =
        let triangleClosers =
            g.Edges
            |> Seq.mapi(fun org edges ->
                let nbrs =
                    edges
                    |> Seq.map fst
                    |> Seq.filter(fun dest -> org <> dest)
                Seq.allPairs nbrs nbrs
                |> Seq.filter(fun (n1, n2) -> n1 <> n2)
                |> Seq.map(fun (n1,n2) -> Math.Min(n1,n2), Math.Max(n1,n2))
            )
            |> Seq.concat
            |> Seq.cache

        let closedTriangles =
            (0, triangleClosers)
            ||> Seq.fold(fun acc (orgIx, destIx) ->
                g.Edges[orgIx]
                |> ResizeArray.tryFind(fun (dest,_) -> dest = destIx)
                |> Option.map(fun _ -> acc + 1)
                |> Option.defaultValue acc
            )
        float (closedTriangles) / float(triangleClosers|> Seq.length)
            
    
    static member compute (g: FGraph<'NodeKey, 'NodeData, 'EdgeData>) =
        ClusteringCoefficient.clusteringCoefficientOfFGraph g
        
    static member compute (g: AdjGraph<'NodeKey, 'NodeData, 'EdgeData>) =
        ClusteringCoefficient.clusteringCoefficientOfAdjGraph g
    
    static member compute (g: DiGraph<'NodeKey, 'NodeData, 'EdgeData>) =
        ClusteringCoefficient.globalClusteringCoefficientOfDiGraph g
    
    static member compute (g: UndirectedGraph<'NodeKey, 'NodeData, 'EdgeData>) =
        ClusteringCoefficient.globalClusteringCoefficientOfUndirected g
    