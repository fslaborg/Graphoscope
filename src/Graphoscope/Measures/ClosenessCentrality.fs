namespace Graphoscope.Measures
open Graphoscope
open System.Collections.Generic


type ClosenessCentrality() =
    
    static member ofFGraphNode (dijkstraF:'NodeKey -> ('EdgeData -> float) ->  FGraph<'NodeKey,'NodeData,'EdgeData> -> Dictionary<'NodeKey,float>) (getEdgeWeightF:'EdgeData -> float) (graph :  FGraph<'NodeKey, 'NodeData, 'EdgeData>) (nodeKey:'NodeKey) =    
        let dic = dijkstraF nodeKey getEdgeWeightF graph
        let shortestPathSum = 
            seq {
                    for nv in dic do
                        nv.Value
            }
            |>Seq.sum
        1. / shortestPathSum

    static member ofFGraphNodeNormalised (dijkstraF:'NodeKey -> ('EdgeData -> float) ->  FGraph<'NodeKey,'NodeData,'EdgeData> -> Dictionary<'NodeKey,float>) (getEdgeWeightF:'EdgeData -> float) (graph :  FGraph<'NodeKey, 'NodeData, 'EdgeData>) (nodeKey:'NodeKey) =    
        let nodeCount = Measures.Size.compute graph |> float
        let dic = dijkstraF nodeKey getEdgeWeightF graph
        let shortestPathSum = 
            seq {
                    for nv in dic do
                        nv.Value
            }
            |>Seq.sum
        (nodeCount-1.) / shortestPathSum

    static member ofFGraph (dijkstraF:'NodeKey -> ('EdgeData -> float) ->  FGraph<'NodeKey,'NodeData,'EdgeData> -> Dictionary<'NodeKey,float>) (getEdgeWeightF:'EdgeData -> float) (graph :  FGraph<'NodeKey, 'NodeData, 'EdgeData>) =    
        let dict = new Dictionary<'NodeKey,float>()
        for i in graph.Keys do
            let closeness = ClosenessCentrality.ofFGraphNode dijkstraF getEdgeWeightF graph i
            dict.Add(i,closeness)
        dict

    static member ofFGraphNormalised (dijkstraF:'NodeKey -> ('EdgeData -> float) ->  FGraph<'NodeKey,'NodeData,'EdgeData> -> Dictionary<'NodeKey,float>) (getEdgeWeightF:'EdgeData -> float) (graph :  FGraph<'NodeKey, 'NodeData, 'EdgeData>) (nodeKey:'NodeKey) =    
        let dict = new Dictionary<'NodeKey,float>()
        for i in graph.Keys do
            let closeness = ClosenessCentrality.ofFGraphNodeNormalised dijkstraF getEdgeWeightF graph i
            dict.Add(i,closeness)
        dict
