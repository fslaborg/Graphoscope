namespace Graphoscope.Measures

open Graphoscope
open FSharpAux

type Modularity() =
    static member private isValidPartitionDiGraph (partition: 'NodeKey Set []) (graph: DiGraph<'NodeKey, _, 'EdgeData>) =
        Set graph.NodeKeys = Set.unionMany partition

    static member private isValidPartitionUndirected (partition: 'NodeKey Set []) (graph: UndirectedGraph<'NodeKey, _, 'EdgeData>) =
        Set graph.NodeKeys = Set.unionMany partition

    static member ofUndirectedGraph (getWeight: 'EdgeData -> float) (resolution: float) (partition: 'NodeKey Set []) (graph: UndirectedGraph<'NodeKey, _, 'EdgeData>)=
        if Modularity.isValidPartitionUndirected partition graph |> not then
            failwith "`partition` is not a valid partition of DiGraph."
        let degrees = graph.Edges |> ResizeArray.map(fun x -> (0.,x)||>ResizeArray.fold(fun acc (_, ed) -> acc + getWeight ed))
        let degSum = (0.,degrees)||>ResizeArray.fold(fun acc c -> acc + c)
        let m = degSum / 2.

        let normalizer = 1. / degSum**2
        
        partition
        |> Array.sumBy(fun community ->
            let commIdxs = community|>Set.map(fun nk -> graph.IdMap[nk])
            let lc =
                commIdxs
                |> Seq.sumBy(fun nIx ->
                    (0., graph.Edges[nIx])
                    ||> ResizeArray.fold(fun acc (n,w) ->
                        if community|>Set.contains graph.NodeKeys[n] then
                            acc + getWeight w
                        else acc
                    )
                )
                |> fun x -> x/2.

            let degreeSum = commIdxs|>Seq.sumBy(fun x -> degrees[x])
            lc / m - resolution * degreeSum * degreeSum * normalizer
        )

    static member ofDiGraph (getWeight: 'EdgeData -> float) (resolution: float) (partition: 'NodeKey Set []) (graph: DiGraph<'NodeKey, _, 'EdgeData>)=
        if Modularity.isValidPartitionDiGraph partition graph |> not then
            failwith "`partition` is not a valid partition of DiGraph."
        let inDegrees = graph.InEdges |> ResizeArray.map(fun x -> (0.,x)||>ResizeArray.fold(fun acc (_, ed) -> acc + getWeight ed))
        let outDegrees = graph.OutEdges |> ResizeArray.map(fun x -> (0.,x)||>ResizeArray.fold(fun acc (_, ed) -> acc + getWeight ed))
        let m = 
            (0., graph.InEdges)
            ||> ResizeArray.fold(fun acc1 edges ->
                (0., edges)
                ||> ResizeArray.fold(fun acc2 (_, ed) -> acc2 + getWeight ed)
                |> fun x -> acc1 + x
            )
        
        let normalizer = 1. / m**2
        
        partition
        |> Array.sumBy(fun community ->
            let commIdxs = community|>Set.map(fun nk -> graph.IdMap[nk])
            let lc =
                commIdxs
                |> Seq.sumBy(fun nIx ->
                    (0., graph.InEdges[nIx])
                    ||> ResizeArray.fold(fun acc (n,w) ->
                        if community|>Set.contains graph.NodeKeys[n] then
                            acc + getWeight w
                        else acc
                    )
                )
            let outDegreeSum = commIdxs|>Seq.sumBy(fun x -> outDegrees[x])
            let inDegreeSum = commIdxs|>Seq.sumBy(fun x -> inDegrees[x])
            lc / m - resolution * outDegreeSum * inDegreeSum * normalizer
        )

    /// <summary> 
    /// Finds the modularity of a given <paramref name="partition"/> of the <paramref name="graph"/>,
    /// where <paramref name="partition"/> is a sequence of Set of nodes that collectively exhaust all the nodes in the<paramref name="graph"/>.
    /// </summary>
    /// <param name="graph">The graph to analyse</param> 
    /// <param name="partition">A sequence of Set of nodes that collectively exhaust all the nodes in the<paramref name="graph"/></param>
    /// <param name="getWeight">Function to get the edge weight from 'EdgeData.
    /// Optional; defaults to each edge weight being equal to 1.0.
    /// </param>
    /// <param name="resolution">If resolution is less than 1, modularity favors
    /// larger communities. Greater than 1 favors smaller communities.
    /// Optional; default = 1.0</param>
    /// <exception>Throws if <paramref name="partition"/> isn't a valid partition of <paramref name="graph"/></exception>
    static member compute (graph: UndirectedGraph<'NodeKey, 'NodeData, 'EdgeData>, partition: 'NodeKey Set [], ?getWeight: 'EdgeData -> float, ?resolution: float) =
        let getWeight = defaultArg getWeight (fun _ -> 1.)
        let resolution = defaultArg resolution 1.
        Modularity.ofUndirectedGraph getWeight resolution partition graph

    /// <summary> 
    /// Finds the modularity of a given <paramref name="partition"/> of the <paramref name="graph"/>,
    /// where <paramref name="partition"/> is a sequence of Set of nodes that collectively exhaust all the nodes in the<paramref name="graph"/>.
    /// </summary>
    /// <param name="graph">The graph to analyse</param> 
    /// <param name="partition">A sequence of Set of nodes that collectively exhaust all the nodes in the<paramref name="graph"/></param>
    /// <param name="getWeight">Function to get the edge weight from 'EdgeData.
    /// Optional; defaults to each edge weight being equal to 1.0.
    /// </param>
    /// <param name="resolution">If resolution is less than 1, modularity favors
    /// larger communities. Greater than 1 favors smaller communities.
    /// Optional; default = 1.0</param>
    /// <exception>Throws if <paramref name="partition"/> isn't a valid partition of <paramref name="graph"/></exception>
    static member compute (graph: DiGraph<'NodeKey, 'NodeData, 'EdgeData>, partition: 'NodeKey Set [], ?getWeight: 'EdgeData -> float, ?resolution: float) =
        let getWeight = defaultArg getWeight (fun _ -> 1.)
        let resolution = defaultArg resolution 1.
        Modularity.ofDiGraph getWeight resolution partition graph