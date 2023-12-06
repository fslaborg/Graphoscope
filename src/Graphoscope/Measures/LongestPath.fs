namespace Graphoscope.Measures

open Graphoscope
open Graphoscope.Graphs
open System.Collections.Generic

type LongestPath() =
    
    /// <summary>
    /// Determines whether a cycle can be formed in the given FContextMap starting from the specified node, 
    /// using an algorithm based on priority queues and breadth-first traversal.
    /// </summary>
    /// <param name="starting">The node from which to check for the existence of a cycle.</param>
    /// <param name="graph">The FContextMap in which to search for the cycle.</param>
    /// <returns>True if a cycle is found, otherwise false.</returns>
    static member hasCycleFromNodeInFContextMap (starting : 'NodeKey) (graph : Directed.FContextMap<'NodeKey, 'NodeData, 'EdgeData>) =
        let visited = HashSet<'NodeKey>()
        //let stack = Stack<'NodeKey>()
        let priorityQueue: Queue<('NodeKey)> = System.Collections.Generic.Queue()//Priority_Queue.SimplePriorityQueue<('NodeKey*float),float>()

        //stack.Push(starting)
        priorityQueue.Enqueue(starting)
        visited.Add(starting) |> ignore


        let rec outerLoop counter =
            if priorityQueue.Count>0 then
            //if stack.Count > 0 then 
                let nodeKey = priorityQueue.Dequeue() //et nodeKey = (stack.Pop())
                let (_, nd, s) = graph.[nodeKey]

                printfn $"{nodeKey}"
                let rec innerLoops counter =
                    if counter=s.Count then
                        outerLoop (counter+1)
                    else
                        let node = s.Keys|>Seq.item counter
                        if node=starting then 
                            true  
                        elif not(visited.Contains(node)) then
                            //stack.Push(node)
                            priorityQueue.Enqueue(node)
                            visited.Add(node) |> ignore
                            innerLoops (counter+1)
                        else
                            innerLoops (counter+1)
                innerLoops 0
            else
                false
        outerLoop 0

    /// <summary>
    /// Determines whether a cycle exists in the given FContextMap.
    /// </summary>
    /// <param name="graph">The FContextMap to check for the presence of a cycle</param>
    /// <returns>True if a cycle is found, otherwise false.</returns>
    static member hasCycleInFContextMap (graph : Directed.FContextMap<'NodeKey, 'NodeData, 'EdgeData>) =

        let nodes = graph|>Seq.map(fun kvp -> kvp.Key)

        let rec isCyclic (counter:int) = 
            if counter = (nodes|>Seq.length) then
                false
            else
                let node  = nodes |>Seq.item counter
                if LongestPath.hasCycleFromNodeInFContextMap node graph then
                    true
                else
                    isCyclic (counter+1)
        isCyclic 0

    /// <summary> 
    /// Computes longest paths from <paramref name="starting"/> for <paramref name="graph"/> using an inverse Dijkstra's algorithm.
    /// </summary>
    /// <param name="starting"> Calculate the longest paths from this node.</param>    
    /// <param name="getEdgeWeight"> Function to convert the EdgeData to a float.</param>    
    /// <param name="graph"> The FContextMap for which to compute the longest path. Has to be an Directed Acyclic Graph.</param>
    /// <remarks>If there isn't a path between two edges, the distance is set to `infinity`.</remarks>
    /// <returns>Tuples of an ordered list containing the path and the distance of the longest path.</returns>
    static member getLongestPathOfFContextMapUnchecked (starting : 'NodeKey) (getEdgeWeight : 'EdgeData -> float) (graph :  Directed.FContextMap<'NodeKey, 'NodeData, 'EdgeData> ) =

        //Inverse Version of Djikstra, transforming the edgeWeights into negatives
        let getLongestPathDictOfFContextMap (starting : 'NodeKey) (getEdgeWeight : 'EdgeData -> float) (graph :  Directed.FContextMap<'NodeKey, 'NodeData, 'EdgeData> ) =
            let distance = Dictionary<'NodeKey, ('NodeKey*float)>()
            //let priorityQueue = SortedSet<'NodeKey * float>(Comparer<'NodeKey * float>.Create(fun (_, d1) (_, d2) -> compare d1 d2))
            let priorityQueue: Queue<('NodeKey * float)> = System.Collections.Generic.Queue()//Priority_Queue.SimplePriorityQueue<('NodeKey*float),float>()
            let infinity = System.Double.MaxValue

            // Initialize distances to infinity for all nodes except the starting node
            // TODO: this can be improved by getOrDefault
            for nodeKey in graph.Keys do
                if nodeKey = starting then
                    distance.[nodeKey] <- (starting,0.)
                else
                    distance.[nodeKey] <- (starting,infinity)

            priorityQueue.Enqueue((starting, 0.)) |> ignore

            while priorityQueue.Count > 0 do
                let (currentNode, currentDistance) = priorityQueue.Dequeue()
            
                let (_, _, predecessors) = graph.[currentNode]

                for kv in predecessors do
                    if kv.Key <> currentNode then
                        let kvValue = kv.Value |> getEdgeWeight |> fun  x -> -x
                        //if kvValue < 0. then failwithf "Dijkstra does not handle neg. edge weight"
                        let totalDistance = (currentDistance + kvValue) // Assuming edgeWeight is always 1 in this example
                        let prevNode,prevDistance = distance.[kv.Key]

                        // Improve getValue
                        if totalDistance < prevDistance then
                            distance.[kv.Key] <- (currentNode,totalDistance)
                            priorityQueue.Enqueue(kv.Key,totalDistance) |> ignore
                            Seq.sortBy snd priorityQueue |>ignore

            distance

        //Contains the dictionary create by getLongestPathDictOfFContextMap of nodeKey,(pred,pathLenght)
        let longestPathDict = getLongestPathDictOfFContextMap (starting : 'NodeKey) (getEdgeWeight : 'EdgeData -> float) (graph :  Directed.FContextMap<'NodeKey, 'NodeData, 'EdgeData> )

        //Contains the most distant nodeKey and the negative path lenght to it
        let mostDistantNode,longestPath = 
            longestPathDict
            |>Seq.minBy(fun kvp ->  
                kvp.Value 
                |>snd
            )
            |>fun kvp -> 
                kvp.Key,snd kvp.Value

        //Function to recreate the path to the node with the longest path based on the inverse Djikstra. Returns an ordered List of the path from the starting node to the most distant node
        let rec reconstructPath (path: 'NodeKey list) =
            if List.head path = starting then
                path
            else 
                let currentHead = List.head path
                let pred: 'NodeKey = longestPathDict.[currentHead] |>fst
                reconstructPath (pred::path)

        reconstructPath [mostDistantNode] , (longestPath|>fun x -> x*(-1.))

    /// <summary> 
    /// Computes longest paths from <paramref name="starting"/> for <paramref name="graph"/> using an inverse Dijkstra's algorithm.
    /// </summary>
    /// <param name="starting"> Calculate the longest paths from this node.</param>    
    /// <param name="getEdgeWeight"> Function to convert the EdgeData to a float.</param>    
    /// <param name="graph"> The FContextMap for which to compute the longest path. Has to be an Directed Acyclic Graph.</param>
    /// <remarks>Uses hasCycleInFContextMap to check for cyclic character. If the given FContextMap is not a Directed Acyclic Graph this will fail.</remarks>
    /// <returns>Tuples of an ordered list containing the path and the distance of the longest path.</returns>
    static member getLongestPathOfFContextMap (starting : 'NodeKey) (getEdgeWeight : 'EdgeData -> float) (graph :  Directed.FContextMap<'NodeKey, 'NodeData, 'EdgeData> ) =
        if LongestPath.hasCycleInFContextMap graph then 
            failwith "The given FContextMap is not a Directed Acyclic Graph!"
        else
            LongestPath.getLongestPathOfFContextMapUnchecked starting getEdgeWeight graph


    /// <summary> 
    /// Computes longest paths from <paramref name="starting"/> for <paramref name="graph"/> using an inverse Dijkstra's algorithm.
    /// </summary>
    /// <param name="starting"> Calculate the longest paths from this node.</param>    
    /// <param name="graph"> The FContextMap for which to compute the longest path. Has to be an Directed Acyclic Graph.</param>
    /// <remarks>If there isn't a path between two edges, the distance is set to `infinity`.</remarks>
    /// <remarks>Compute sets all EdgeWeights to 1. If you do not want this, consider computeByEdgeData of computeByEdgeDataWith.</remarks>
    /// <returns>Tuples of an ordered list containing the path and the distance of the longest path.</returns>
    static member compute((starting : 'NodeKey),(graph :  Directed.FContextMap<'NodeKey, 'NodeData, 'EdgeData> )) =
        LongestPath.getLongestPathOfFContextMap starting (fun x -> 1.) graph

    /// <summary> 
    /// Computes longest paths from <paramref name="starting"/> for <paramref name="graph"/> using an inverse Dijkstra's algorithm.
    /// </summary>
    /// <param name="starting"> Calculate the longest paths from this node.</param>    
    /// <param name="graph"> The FContextMap for which to compute the longest path. Has to be an Directed Acyclic Graph.</param>
    /// <remarks>If there isn't a path between two edges, the distance is set to `infinity`.</remarks>
    /// <remarks>computeByEdgeData uses the value in EdgeData as the distance. Only usable with float weights as EdgeData.</remarks>
    /// <returns>Tuples of an ordered list containing the path and the distance of the longest path.</returns>
    static member computeByEdgeData((starting : 'NodeKey),(graph :  Directed.FContextMap<'NodeKey, 'NodeData, float> )) =
        LongestPath.getLongestPathOfFContextMap starting id graph

    /// <summary> 
    /// Computes longest paths from <paramref name="starting"/> for <paramref name="graph"/> using an inverse Dijkstra's algorithm.
    /// </summary>
    /// <param name="starting"> Calculate the longest paths from this node.</param>    
    /// <param name="getEdgeWeight"> Function to convert the EdgeData to a float.</param>    
    /// <param name="graph"> The FContextMap for which to compute the longest path. Has to be an Directed Acyclic Graph.</param>
    /// <remarks>If there isn't a path between two edges, the distance is set to `infinity`.</remarks>
    /// <remarks>computeByEdgeDataWith uses <paramref name="getEdgeWeight"/>  to convert the EdgeData into the needed float weights.</remarks>
    /// <returns>Tuples of an ordered list containing the path and the distance of the longest path.</returns>
    static member computeByEdgeDataWith((starting : 'NodeKey),(getEdgeWeight : 'EdgeData -> float),(graph :  Directed.FContextMap<'NodeKey, 'NodeData, 'EdgeData> )) =
        LongestPath.getLongestPathOfFContextMap starting getEdgeWeight graph
