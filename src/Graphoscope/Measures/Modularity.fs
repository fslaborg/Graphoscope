namespace Graphoscope.Measures

open Graphoscope
open FSharpAux

type Modularity() =
    static member ofDiGraph (getWeight: 'EdgeData -> float) (resolution: float) (communities: 'NodeKey [][]) (graph: DiGraph<'NodeKey, _, 'EdgeData>)=
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
        
        communities
        |> Array.sumBy(fun community ->
            let commIdxs = community|>Array.map(fun nk -> graph.IdMap[nk])
            let lc =
                commIdxs
                |> Array.sumBy(fun nIx ->
                    (0., graph.InEdges[nIx])
                    ||> ResizeArray.fold(fun acc (n,w) ->
                        if community|>Array.contains graph.NodeKeys[n] then
                            acc + getWeight w
                        else acc
                    )
                )
            let outDegreeSum = commIdxs|>Array.sumBy(fun x -> outDegrees[x])
            let inDegreeSum = commIdxs|>Array.sumBy(fun x -> inDegrees[x])
            lc / m - resolution * outDegreeSum * inDegreeSum * normalizer
        )