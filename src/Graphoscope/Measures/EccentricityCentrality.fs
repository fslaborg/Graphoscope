namespace Graphoscope.Measures

open Graphoscope
open System.Collections.Generic
open FSharpAux

type EccentricityCentrality() =
    
    
    static member ofFGraphNode (dijkstraF:'NodeKey -> ('EdgeData -> float) ->  FGraph<'NodeKey,'NodeData,'EdgeData> -> Dictionary<'NodeKey,float>) (getEdgeWeightF:'EdgeData -> float) (graph :  FGraph<'NodeKey, 'NodeData, 'EdgeData>) (nodeKey:'NodeKey) =    
        let dic = dijkstraF nodeKey getEdgeWeightF graph
        let eccentricity = Seq.max(dic.Values)
        1./eccentricity

    static member ofFGraph2D (graph :  float array2d) =    
        let shortestPaths = graph
        // let indexToNode = graph.Keys|>Seq.map(fun x -> nodeIndexer x,x)|> Map.ofSeq
        let dict = new Dictionary<'NodeKey,'Eccentricity>()
        let getDict (arr: _ [,])  =
            let n,m = arr |> Array2D.length1, arr |> Array2D.length2
            let rec getMax i j max =
                if j = m then max
                else
                    let value = arr.[i,j]
                    if value < max then getMax i (j+1) max 
                    else getMax i (j+1) value 
            
            for i=0 to ((Array2D.length1 arr)-1) do
                let eccentricity = getMax i 0 arr.[i,0]
                let key = i//indexToNode.Item i
                dict.Add(key,eccentricity)
            
            dict
        getDict shortestPaths

