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
        // let outDegrees = graph.OutEdges |> ResizeArray.map(fun x -> (0.,x)||>ResizeArray.fold(fun acc (_, ed) -> acc + getWeight ed))
        let m = 
            (0., graph.Edges)
            ||> ResizeArray.fold(fun acc1 edges ->
                (0., edges)
                ||> ResizeArray.fold(fun acc2 (_, ed) -> acc2 + getWeight ed)
                |> fun x -> acc1 + x
            )
            |> fun x -> x / 2.

        let normalizer = 1. / m**2
        
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