namespace Graphoscope.Measures

open Graphoscope
open System.Collections.Generic


type ShortestPath={
    target:int
    path:Set<int>
}

type BetweennessCentrality() =
        
    static member returnPaths (shortestPathAlgorithm:'NodeKey->('EdgeData -> float) ->FGraph<'NodeKey,'NodeData,'EdgeData> -> Dictionary<'NodeKey,'NodeKey * float>) (nodeIndexer:'NodeKey->int) (getEdgeWeight : 'EdgeData -> float) (graph: FGraph<'NodeKey,'NodeData,'EdgeData>) = 

        let paths :Dictionary<('NodeKey),Set<ShortestPath>> = Dictionary<('NodeKey),Set<ShortestPath>>()

        let getPathsOfDic key (dic:Dictionary<'NodeKey,('NodeKey*float)>) starting=
            let getPrev key =
                dic.Item key |> fst
            let rec getPath key (set:Set<'NodeKey>) =
                let prev = getPrev key
                //printfn $"Key:{key};prev:{prev}"
                if prev = starting then 
                    set
                else
                    let newSet = Set.add prev set
                    getPath prev newSet
            getPath key Set.empty


        let rec buildSet (set:Set<ShortestPath>) (starting:'NodeKey) (sp: Dictionary<'NodeKey,('NodeKey * float)>) (keysList:'NodeKey list) counter =
            if counter=keysList.Length then
                set
            else
                let x = keysList.[counter] 
                if x=starting then 
                    buildSet set starting sp keysList (counter+1)
                else
                    let setToSave = {
                        target=x
                        path=getPathsOfDic x sp starting
                    }
                    let newSet = set.Add(setToSave)
                    buildSet newSet starting sp keysList (counter+1)


        let rec getPaths (keysList:'NodeKey list)  counter =
            if counter = Seq.length keysList then
                paths
            else
                let starting = keysList.[counter]
                let sp = shortestPathAlgorithm (starting) getEdgeWeight graph
                let set = buildSet Set.empty starting sp keysList 0

                paths.Add(starting,set)
                    
                getPaths keysList (counter+1)
        getPaths (graph.Keys|>Seq.map(fun x -> nodeIndexer x)|>List.ofSeq) 0

    static member getBetweennessOfPathsAndNode (paths:Dictionary<int,Set<ShortestPath>>) (nodeIndexer:'NodeKey->int) (node:'NodeKey)=
        let pathCount = paths.Count*(paths.Count-1)|>float
        
        paths
        |> Seq.choose(fun nkv -> 
            let nodeV = nodeIndexer node
            if nkv.Key<>(nodeV) then
                Some (nkv.Value|>Seq.sumBy(fun (x:ShortestPath) -> if x.target<>nodeV && x.path.Contains nodeV then 1. else 0.))
            else    
                None
        )
        |> Seq.sum
        |> fun x -> x/pathCount
