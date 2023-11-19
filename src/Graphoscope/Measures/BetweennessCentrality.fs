namespace Graphoscope.Measures

open Graphoscope
open System.Collections.Generic

type BetweennessCentrality() =

    // Function to perform Dijkstra's shortest path algorithm
    static member ofAdjGraph (getEdgeWeight : 'EdgeData -> float) (graph :  AdjGraph<'NodeKey, 'NodeData, 'EdgeData> ) =
        let getDjikstra  (checkedNodes:Set<'NodeKey>) (starting : 'NodeKey)  =
            
            let distance = Dictionary<'NodeKey,('NodeKey*float)>()
            let priorityQueue: Queue<('NodeKey * float)> = System.Collections.Generic.Queue()//Priority_Queue.SimplePriorityQueue<('NodeKey*float),float>()
            let infinity = System.Double.MaxValue

            // Initialize distances to infinity for all nodes except the starting node
            // TODO: this can be improved by getOrDefault
            for nodeKey in graph.Keys do
                if nodeKey = starting then
                    distance.[nodeKey] <- (starting,0.)
                else
                    distance.[nodeKey] <- (starting,infinity)

            priorityQueue.Enqueue((starting, 0)) |> ignore

            while priorityQueue.Count > 0 do
                let (currentNode, currentDistance) = priorityQueue.Dequeue()
                //priorityQueue.Remove(priorityQueue.Min) |> ignore
            
                let neighbours = AdjGraph.getNeighbours currentNode graph

                for node,rawDistance in neighbours do
                    let weightedDistance = rawDistance |> getEdgeWeight
                    if weightedDistance < 0. then failwithf "Dijkstra does not handle neg. edge weigth"
                    let totalDistance = (currentDistance + weightedDistance) // Assuming edgeWeight is always 1 in this example
                    // Impove getValue
                    let prevNode,prevDistance = distance.[node]
                    if totalDistance < prevDistance then
                        distance.[node] <- (currentNode,totalDistance)
                        priorityQueue.Enqueue(node,totalDistance) |> ignore
                        Seq.sortBy snd priorityQueue |>ignore

            distance

        //let betweennessCollection: Dictionary<'NodeKey,Dictionary<'NodeKey,LinkedList<'NodeKey>>> =  Dictionary<'NodeKey, Dictionary<'NodeKey,LinkedList<'NodeKey>>>()

        let betweennessCount: Dictionary<'NodeKey,float> = Dictionary<'NodeKey,float>() 

        let addToExistingLinkedList (starting:'NodeKey) (existingLL:LinkedList<'NodeKey>) (sndLL:LinkedList<'NodeKey>) =
    
            existingLL
            |> Seq.rev
            |> Seq.iter(fun x -> 
                sndLL.AddFirst(x)|>ignore
                if x<>starting then
                    let c = betweennessCount[x]
                    //printfn $"AddAdd +1 {x}"

                    betweennessCount[x] <- (c+0.5)
            )

            sndLL

        //Return the path between the key and the staring node based on a Djikstra Dictionary
        let getPathsOfDic (keyEnd: 'NodeKey) (pathCollection: Dictionary<'NodeKey,LinkedList<'NodeKey>>) (dic:Dictionary<'NodeKey,('NodeKey*float)>) (starting: 'NodeKey)=
            //printfn $"start:{starting} end:{keyEnd}"
            let linkedList: LinkedList<'NodeKey> = LinkedList<'NodeKey>()
            linkedList.AddLast(keyEnd) |> ignore

            let getPrev key =
                dic.Item key |> fst
            
            let rec getPath (key: 'NodeKey) =
                let prev: 'NodeKey = getPrev key
                if prev = starting then 
                    linkedList.AddBefore((linkedList.Find(key)),starting)|>ignore
                    if key<>keyEnd then
                        let c = betweennessCount[key]
                        //printfn $"Add +1 {key}"
                        betweennessCount[key] <- (c+0.5)    
                    linkedList

                elif pathCollection.ContainsKey key then
                        let ogLL = pathCollection.Item key
                        addToExistingLinkedList starting ogLL linkedList
                else
                    if key<>keyEnd then
                        let c = betweennessCount[key]
                        //printfn $"Add +1 {key}"
                        betweennessCount[key] <- (c+0.5)    
                    linkedList.AddBefore((linkedList.Find(key)),prev)|>ignore
                    getPath prev
           
            pathCollection.Add(keyEnd,getPath keyEnd)
                

        let calculateLinkedList (checkedNodes:Set<'NodeKey>)  (starting:'NodeKey) = 
            let djikstra = getDjikstra  (checkedNodes) starting
            
            let pathCollection = new Dictionary<'NodeKey,LinkedList<'NodeKey>>()

            let seqIncreasing = 
                djikstra
                |> Seq.filter(fun kvp -> kvp.Key<>starting)
                |> Seq.sortBy(fun kvp -> kvp.Value)

            seqIncreasing
            |> Seq.iter(fun x -> 
                getPathsOfDic x.Key pathCollection djikstra starting
            )

            //betweennessCollection.Add(starting,pathCollection)

            Set.add starting checkedNodes// checkedNodes.Add(starting)
        
        graph.Keys
        |> Seq.iter(fun x -> 
            betweennessCount.Add(x,0.))

        graph.Keys
        |> Seq.fold(fun folder node -> 
            calculateLinkedList folder node
        ) Set.empty
        |>ignore

        //betweennessCollection,
        let c = (float(betweennessCount.Count*(betweennessCount.Count-1))) /2.
        betweennessCount
        |> Seq.iter(fun kvp ->
            let v = kvp.Value / c
            betweennessCount[kvp.Key] <- v
        )

        betweennessCount




    static member compute (graph :  AdjGraph<'NodeKey, 'NodeData, 'EdgeData> ) =
        BetweennessCentrality.ofAdjGraph (fun x -> 1.) graph
    
    static member computeWithEdgeData (graph :  AdjGraph<'NodeKey, 'NodeData, float> ) =
        BetweennessCentrality.ofAdjGraph id graph
    
    static member computeWithEdgeDataBy ((getEdgeWeight : 'EdgeData -> float),(graph :  AdjGraph<'NodeKey, 'NodeData, 'EdgeData> )) =
        BetweennessCentrality.ofAdjGraph getEdgeWeight graph
    

//     /// <summary> 
//     /// Get all of the shortest paths of a FGraph
//     /// </summary>
//     /// <param name="shortestPathAlgorithm">Function to calculate the shortest Path via floydWarshall</param> 
//     /// <param name="nodeIndexer">Function to index nodes based on their 'NodeKey</param>    
//     /// <param name="getEdgeWeightF">Function to get the edgeweight out of the 'EdgeData</param>      
//     /// <param name="graph">The graph to be analysed</param>     
//     /// <returns>A Dictionary with the indexed Node as the key and the shortest Paths as Set as value </returns>
//     static member returnPaths (shortestPathAlgorithm:'NodeKey->('EdgeData -> float) ->FGraph<'NodeKey,'NodeData,'EdgeData> -> Dictionary<'NodeKey,'NodeKey * float>) (nodeIndexer:'NodeKey->int) (getEdgeWeight : 'EdgeData -> float) (graph: FGraph<'NodeKey,'NodeData,'EdgeData>) = 
//         //Opens a Dictionary for ease of use
//         let paths :Dictionary<('NodeKey),Set<ShortestPath>> = Dictionary<('NodeKey),Set<ShortestPath>>()

//         //Return the shortest Paths out of a Dijkstra-Result: The Result contains node; previously visited node, shortest Path. From there you can get the complete path to the node from the starting node.
//         let getPathsOfDic key (dic:Dictionary<'NodeKey,('NodeKey*float)>) starting=
//             let getPrev key =
//                 dic.Item key |> fst
//             let rec getPath key (set:Set<'NodeKey>) =
//                 let prev = getPrev key
//                 //printfn $"Key:{key};prev:{prev}"
//                 if prev = starting then 
//                     set
//                 else
//                     let newSet = Set.add prev set
//                     getPath prev newSet
//             getPath key Set.empty

//         //Function to build the path set of a given starting node 
//         let rec buildSet (set:Set<ShortestPath>) (starting:'NodeKey) (sp: Dictionary<'NodeKey,('NodeKey * float)>) (keysList:'NodeKey list) counter =
//             if counter=keysList.Length then
//                 set
//             else
//                 let x = keysList.[counter] 
//                 if x=starting then 
//                     buildSet set starting sp keysList (counter+1)
//                 else
//                     let setToSave = {
//                         target=x
//                         path=getPathsOfDic x sp starting
//                     }
//                     let newSet = set.Add(setToSave)
//                     buildSet newSet starting sp keysList (counter+1)

//         //Get the shortest Paths Set collection for each node in the graph
//         let rec getPaths (keysList:'NodeKey list)  counter =
//             if counter = Seq.length keysList then
//                 paths
//             else
//                 let starting = keysList.[counter]
//                 let sp = shortestPathAlgorithm (starting) getEdgeWeight graph
//                 let set = buildSet Set.empty starting sp keysList 0

//                 paths.Add(starting,set)
                    
//                 getPaths keysList (counter+1)
//         getPaths (graph.Keys|>List.ofSeq) 0 //(graph.Keys|>Seq.map(fun x -> nodeIndexer x)|>List.ofSeq) 0


//     /// <summary> 
//     /// Get all of the shortest paths of a FGraph
//     /// </summary>
//     /// <param name="paths">The result of the returnPaths function. A collection of all shortestPaths </param> 
//     /// <param name="nodeIndexer">Function to index nodes based on their 'NodeKey</param>    
//     /// <param name="node">The NodeKey to get the Betweenness of</param>     
//     /// <returns>A float of the betweenness of the given node </returns>
//     static member getBetweennessOfPathsAndNode (paths:Dictionary<int,Set<ShortestPath>>) (nodeIndexer:'NodeKey->int) (node:'NodeKey)=
//         //Returns how many shortest paths can exist by combinatorics
//         let pathCount = paths.Count*(paths.Count-1)|>float
        
//         //Filter all shortest paths that are started or go to the node of interest
//         paths
//         |> Seq.choose(fun nkv -> 
//             let nodeV = nodeIndexer node
//             if nkv.Key<>(nodeV) then
//                 Some (nkv.Value|>Seq.sumBy(fun (x:ShortestPath) -> if x.target<>nodeV && x.path.Contains nodeV then 1. else 0.))
//             else    
//                 None
//         )
//         |> Seq.sum
//         |> fun x -> x/pathCount
