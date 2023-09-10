namespace Graphoscope.Measures

open Graphoscope
open System.Collections.Generic

type BetweennessCentrality() =
    
    static member getPathsForBetweennessOfFGraph (getEdgeWeight : 'EdgeData -> float) (graph :  FGraph<'NodeKey, 'NodeData, 'EdgeData> ) =
        let ofUndirectedFGraphIncludingPath (starting : 'NodeKey) (toCheck: Set<'NodeKey>) (getEdgeWeight : 'EdgeData -> float) (pathDict:Dictionary<('NodeKey*'NodeKey),Set<'NodeKey>>) (graph :  FGraph<'NodeKey, 'NodeData, 'EdgeData> ) =
            let getPathsOfDic key (dic:Dictionary<'NodeKey,('NodeKey*float)>) =
                let getPrev key =
                    dic.Item key |> fst
                let rec getPath key (set:Set<'NodeKey>) =
                    let prev = getPrev key
                    if prev = starting then 
                        set
                    else
                        let newSet = Set.add prev set
                        getPath prev newSet
                getPath key Set.empty
                
            let distance = Dictionary<'NodeKey,('NodeKey*float)>()
            let priorityQueue = SortedSet<'NodeKey * float>(Comparer<'NodeKey * float>.Create(fun (_, d1) (_, d2) -> compare d1 d2))
            let infinity = System.Double.MaxValue

            // Initialize distances to infinity for all nodes except the starting node
            // TODO: this can be improved by getOrDefault
            for nodeKey in toCheck do
                if nodeKey = starting then
                    distance.[nodeKey] <- (starting,0.)
                else
                    distance.[nodeKey] <- (starting,infinity)

            priorityQueue.Add((starting, 0)) |> ignore

            while priorityQueue.Count > 0 do
                let (currentNode, currentDistance) = priorityQueue.Min
                priorityQueue.Remove(priorityQueue.Min) |> ignore
            
                let neighbours = graph.[currentNode] |> FContext.neighbours |> Seq.filter(fun (nk,ed) -> Set.contains nk toCheck)

                for node,rawDistance in neighbours do
                    let weightedDistance = rawDistance |> getEdgeWeight
                    if weightedDistance < 0. then failwithf "Dijkstra does not handle neg. edge weigth"
                    let totalDistance = (currentDistance + weightedDistance) // Assuming edgeWeight is always 1 in this example
                    // Impove getValue
                    let prevNode,prevDistance = distance.[node]
                    if totalDistance < prevDistance then
                        distance.[node] <- (currentNode,totalDistance)
                        priorityQueue.Add((node, totalDistance)) |> ignore
            
            
            for nkV in distance do
                let key = nkV.Key 
                let path = getPathsOfDic key distance
                pathDict.Add((starting,key),path)

            pathDict

        let pathDict = Dictionary<('NodeKey*'NodeKey),Set<'NodeKey>>()

        let rec getPaths (keysList:'NodeKey list) =
            if keysList = List.empty then
                pathDict
            else
                ofUndirectedFGraphIncludingPath keysList.Head (keysList|>Set.ofSeq) getEdgeWeight pathDict graph
                getPaths keysList.Tail

        getPaths (graph.Keys|>List.ofSeq)


    static member getBetweennessOfFGraphNode (result:Dictionary<'NodeKey * 'NodeKey,Set<'NodeKey>>) (key:'NodeKey) =
        let countPaths =
            result.Keys.Count*2|>float
        let countKeys =
            let mutable count = 0.
            for nkv in result do
                if nkv.Value.Contains key then
                    count <- count+2.
                else
                    ()
            count
        countKeys / countPaths
    
