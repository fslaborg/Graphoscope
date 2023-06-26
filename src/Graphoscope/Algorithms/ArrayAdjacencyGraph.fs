﻿namespace ArrayAdjacencyGraph.Model

open Graphoscope.ArrayAdjacencyGraph
open FSharpx.Collections
open System
open System.Collections.Generic



module Measures = 
//Much of the logic is taken from the measure formulas detailed in the cytoscape documentation here 
//https://med.bioinf.mpi-inf.mpg.de/netanalyzer/help/2.7/index.html#figure7

    let private infinity = Double.PositiveInfinity 

    // dijkstra implementation is based on and extended from the psuedo code here https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm#Pseudocode
    // Didnt rewrite with recursion and immutability for performance reasons and to keep it close to the psuedo code 
    let private dijkstra(graph: ArrayAdjacencyGraph<'Vertex,'Label,'Edge>) 
                        (edgeFinder: 'Vertex -> 'Vertex array) // used to filter directed/undirected edges
                        (weightCalc: 'Vertex * 'Vertex -> float ) // can be controled for unweighted paths or domain specific use cases.
                        (source: 'Vertex) = 

        let q = new ResizeArray<'Vertex>()
        let dist = new Dictionary<'Vertex, float>()

        graph.GetVertices()
        |> Array.iter(fun v ->   
            q.Add v
            if v <> source then 
                dist.Add(v, infinity) 
            else dist.Add(v, 0.0) )
    
        while q.Count > 0 do
            dist
            |> Seq.filter(fun (KeyValue(v,d)) -> q.Contains v)
            |> Seq.minBy(fun (KeyValue(_ ,d)) -> d)
            |> fun (KeyValue(v,_)) -> 
                q.Remove v |> ignore
                edgeFinder v
                |> Array.iter(fun n -> 
                    let alt = dist.[v] + (weightCalc (v, n))
                    if alt < dist.[n] then dist.[n] <- alt; 
                    ) 
        dist
        |> Seq.map(fun (KeyValue(v,d)) -> v, if d = infinity then None else Some d)
        |> Map 

    // all the weighted functions require Edge to be a float
    let private getWeight (graph: ArrayAdjacencyGraph<'Vertex,'Label,float>) (v, n) = 
        match graph.TryGetWeight(v, n) with
                | Some w -> w
                | None -> infinity 

    let private meanShortestPathBase (vertices: 'Vertex array) (fn: 'Vertex -> Map<'Vertex,option<float>> ) = 
        vertices
        |> Seq.map(fun v ->  
            fn v
            |> Map.toSeq
            |> Seq.choose(fun (_,v) -> v)
        )
        |> Seq.concat
        |> Seq.filter (fun v -> v > 0.0)
        |> Seq.average

    /// Returns a Some of the undirected shortest path from source to target vertices, else None.         
    let tryGetShortestPath  (source: 'Vertex) (target: 'Vertex) (graph: ArrayAdjacencyGraph<'Vertex,'Label, 'Edge>)= 
        (dijkstra graph graph.Neighbours (fun (n, v) -> 1.0) source).[target]

    /// Returns a Some of the outward directed shortest path from source to target vertices, else None.       
    let tryGetShortestPathDirected (source: 'Vertex) (target: 'Vertex) (graph: ArrayAdjacencyGraph<'Vertex,'Label,'Edge>) = 
        (dijkstra graph graph.Successors (fun (n, v) -> 1.0) source).[target]
    
    /// Returns a Some of the sum of edge weights along the outward directed shortest path from source to target vertices, else None.    
    let tryGetShortestPathDirectedhWeighted  (source: 'Vertex) (target: 'Vertex) (graph: ArrayAdjacencyGraph<'Vertex,'Label,float>) = 
        (dijkstra graph graph.Successors (getWeight graph) source).[target]

    /// Returns the average of all the undirected shortest paths between connected vertices in the graph.
    let meanShortestUnDirected (graph: ArrayAdjacencyGraph<'Vertex,'Label,'Edge>) =
        meanShortestPathBase (graph.GetVertices()) (dijkstra graph graph.Neighbours (fun (_, _) -> 1.0))
    
    /// Returns the average of all the directed shortest paths between connected vertices in the graph.
    let meanShortestPathDirected (graph: ArrayAdjacencyGraph<'Vertex,'Label,'Edge>) =
        meanShortestPathBase (graph.GetVertices()) (dijkstra graph graph.Successors (fun (_, _) -> 1.0))

    /// Returns the average of all the summed weights on directed edges on shortest paths between connected vertices in the graph.
    let meanShortestPathDirectedhWeighted (graph: ArrayAdjacencyGraph<'Vertex,'Label,float>) =
        meanShortestPathBase (graph.GetVertices()) (dijkstra graph graph.Successors (getWeight graph))
   
    let private meanShortestPathVertexBase (paths: Map<'Vertex,option<float>>) =
        paths
        |> Map.toSeq
        |> Seq.choose(fun (_,v) -> v)
        |> Seq.filter (fun v -> v > 0.0)
        |> Seq.average

    //Averages Shortest Paths

    /// Returns the average of all the shortest paths from the source vertex to the connected vertices.
    let meanShortestPathUnDirectedVertex (source: 'Vertex) (graph: ArrayAdjacencyGraph<'Vertex,'Label,'Edge>) =
        (dijkstra graph graph.Neighbours (fun (_, _) -> 1.0) source)
        |> meanShortestPathVertexBase

    /// Returns the average of all the outward directed shortest paths from the source vertex to the connected vertices.
    let meanShortestPathDirectedVertex  (source: 'Vertex) (graph: ArrayAdjacencyGraph<'Vertex,'Label,'Edge>)=
        (dijkstra graph graph.Successors (fun (_, _) -> 1.0) source)
        |> meanShortestPathVertexBase

     /// Returns the average of all the summed weights on outward directed edges on shortest paths from the source vertex to the connected vertices.
    let meanShortestPathDirectedhWeightedVertex  (source: 'Vertex) (graph: ArrayAdjacencyGraph<'Vertex,'Label,float>) =
        (dijkstra graph graph.Successors (getWeight graph) source)
        |> meanShortestPathVertexBase
      
    // Closeness
    let private getClosenessBase (graph: ArrayAdjacencyGraph<'Vertex,'Label,'Edge>) 
                    (source: 'Vertex) 
                    (edgeFinder: 'Vertex -> 'Vertex array) = 
        dijkstra graph edgeFinder (fun (_, _) -> 1.0) source
        |> Map.toSeq
        |> Seq.choose(fun (k,v) -> v)
        |> Seq.filter (fun v -> v > 0.0)
        |> fun v -> 
            1.0 / (v |> Seq.average) 
    
    /// Returns closeness centrality of the source vertex.
    let getClosenessUnDirected  (source: 'Vertex) (graph: ArrayAdjacencyGraph<'Vertex,'Label,'Edge>)=
        getClosenessBase graph source (graph.Neighbours)

    /// Returns outward directed closeness centrality of the source vertex.
    let getClosenessOutward  (source: 'Vertex) (graph: ArrayAdjacencyGraph<'Vertex,'Label,'Edge>)=
        getClosenessBase graph source (graph.Successors)

    /// Returns inward directed closeness centrality of the source vertex.
    let getClosenessInward  (source: 'Vertex) (graph: ArrayAdjacencyGraph<'Vertex,'Label,'Edge>) =
        getClosenessBase graph source (graph.Predecessors)

    /// Returns Neighborhood Connectivity as defined in cytoscape documentation for source vertex.
    let getNeighborhoodConnectivity(source: 'Vertex) (graph: ArrayAdjacencyGraph<'Vertex,'Label,'Edge>) =
        graph.Neighbours source
        |> Seq.map(fun v -> graph.Degree v |> float)
        |> Seq.average

    // Clustering Coeffcient 
    let rec private combinations acc size set = seq {
        match size, set with 
        | n, x::xs -> 
            if n > 0 then yield! combinations (x::acc) (n - 1) xs
            if n >= 0 then yield! combinations acc n xs 
        | 0, [] -> yield acc 
        | _, [] -> () }

    /// Returns Clustering Coeffcient as defined in cytoscape documentation for source vertex.
    let getClusteringCoefficient (source: 'Vertex) (graph: ArrayAdjacencyGraph<'Vertex,'Label,'Edge>) =
        graph.Neighbours source
        |> Array.toList
        |> combinations [] 2
                |> Seq.map(fun l -> (l|> List.head), (l |> List.last))
        |> Seq.map(fun (v1, v2) -> 
            if graph.TryGetUndirectedEdge(v1, v2).IsSome then 1.0 else 0.0 
            )
        |> fun s ->  (s |> Seq.sum) /(s |> Seq.length |> float)

    let private depthFirstSearch (source: 'Vertex)  (getNeightbours : 'Vertex -> 'Vertex array) (graph: ArrayAdjacencyGraph<'Vertex,'Label,'Edge>) =
        let vertices = new Dictionary<'Vertex, bool>()
        graph.GetVertices() |> Array.map(fun v -> vertices.Add(v, false)) |> ignore
        let rec dfs (graph: ArrayAdjacencyGraph<'Vertex,'Label,'Edge>) (v:'Vertex)  =
            vertices[v] <- true
            getNeightbours v 
            |> Array.filter(fun w -> not vertices[w])
            |> Array.map(fun x -> dfs graph x ) 
            |> ignore     
        dfs graph source
        vertices
            
    let isStronglyConnected (graph: ArrayAdjacencyGraph<'Vertex,'Label,'Edge>) : bool =
        let firstPass = 
            (depthFirstSearch (graph.GetVertices()[0])  (graph.Successors) graph
            |> Seq.exists(fun (KeyValue(v,b)) -> not b))
            |> not
        let reverseGraphPass =
            (depthFirstSearch (graph.GetVertices()[0])  (graph.Predecessors) graph
            |> Seq.exists(fun (KeyValue(v,b)) -> not b))
            |> not
        firstPass && reverseGraphPass




///Louvain method for community detection
//Blondel, Vincent D; Guillaume, Jean-Loup; Lambiotte, Renaud; Lefebvre, Etienne (9 October 2008). "Fast unfolding of communities in large networks". Journal of Statistical Mechanics: Theory and Experiment. 2008
module Algorithms =                   
    
    //All functions connected to dictionaries used.
    module private Dictionary = 
        
        //Return the value to the key k if it is bound, else fail.        
        let getValue k (dict:Dictionary<'K,'V>) =
            try 
                dict.Item k
            with
            | _ -> failwithf "Error get k %O dict %O" k dict

    //All functions connected to the randomization progress.
    module private Randomize =
        let rand = new System.Random()

        //Swaps the position of item x and item y in the array a.
        let swap (a: _[]) x y =
            let tmp = a.[x]
            a.[x] <- a.[y]
            a.[y] <- tmp

        // shuffle an array (in-place)
        let shuffle a =
            Array.iteri (fun i _ -> swap a i (rand.Next(i, Array.length a))) a  
    
    //All functions connected to grouping values.
    module private GroupingFunctions =
        
        //Group values of an array by the groupingF and sum the values of each group after applying the valueF on each of them.
        let inline sumGroupBy (groupingF : 'T -> 'group) (valueF : 'T -> 'V) (input : ('T) []) =
        
            let length = input.Length
            let dict = System.Collections.Generic.Dictionary<'group,'V> ()
    
            // Build the groupings
            for i = 0 to length - 1 do

                let item = input.[i]
                let safeKey,v = groupingF item, valueF item
                let mutable prev = Unchecked.defaultof<'V>
                if dict.TryGetValue(safeKey, &prev) then
                    dict.[safeKey] <- prev + v
                else 
                    //dict.Add(safeKey,v)
                    dict.[safeKey] <- v
             
            // Return the array-of-sums.
            let result = Array.zeroCreate dict.Count
            let mutable i = 0
            for group in dict do
                result.[i] <- group.Key, group.Value
                i <- i + 1
            result

        //Find the summed up weights to the original community of the vertex
        let findWeightofConnectionToOldCommunity connectedCommunities originalCommunity     =   
        
            match (Array.tryFind (fun (community,weight) -> community=originalCommunity) connectedCommunities) with
                | Some x    -> (x|> snd)
                | None      -> 0.
    
    //Contains the code for the Louvain method for community detection.
    //Blondel, Vincent D; Guillaume, Jean-Loup; Lambiotte, Renaud; Lefebvre, Etienne (9 October 2008). "Fast unfolding of communities in large networks". Journal of Statistical Mechanics: Theory and Experiment. 2008 
    
    let private louvainMethod (g1:ArrayAdjacencyGraph<'Vertex,'Label,float>) (randomized:bool) (modularityIncreaseThreshold: float) (resolution: float) : (ArrayAdjacencyGraph<'Vertex,'Label*int,float>) = 
        
        //Create the vertices for the output graph and a new one for further computation
        let vertices,vertices2 : Dictionary<'Vertex,'Label*int>*Dictionary<'Vertex,int*int>=
            let vertices = g1.LabelMap().Keys
            let newDictionary = System.Collections.Generic.Dictionary<'Vertex,'Label*int>()
            let newDictionary2 = System.Collections.Generic.Dictionary<'Vertex,int*int>()
            let mutable counter = 0
            for vertex in vertices do
                let newLabel = (g1.GetLabel vertex),counter
                newDictionary.Add (vertex,newLabel)
                newDictionary2.Add (vertex,(counter,counter))
                counter <- counter+1
            newDictionary,newDictionary2
        
        //Create the edges for the output graph
        let edges =
            g1.AdjacencyGraph()
        
        //Create the edges for the computation graph
        let edges2 =
            let newEdges = System.Collections.Generic.Dictionary<int,(int*int*float)ResizeArray>()
            for v in edges do
                let key     = vertices2.Item (v.Key) |> fst
                let newEdgesResize : ResizeArray<int*int*float> = ResizeArray()
                let edges   = 
                    v.Value.ToArray() 
                    |> Array.map (fun (s,t,w) -> 
                        newEdgesResize.Add ((vertices2.Item s |> fst),(vertices2.Item t |> fst),w)
                    )
                newEdges.Add (key,newEdgesResize)
            newEdges

        //Update the vertices for the computation graph
        let verticesUpdated =
            let newVertices = System.Collections.Generic.Dictionary<int,int*int>()
            for i in vertices2 do
                let v = i.Value
                let key = fst v
                newVertices.Add (key,v)
            newVertices

        //let currentResolution =
        //    if resolution <= 0. then
        //        failwith "The resolution has to be at least 1."
        //    else
        //        resolution


        //The output graph
        let g : (ArrayAdjacencyGraph<'Vertex,'Label*int,float>) = ArrayAdjacencyGraph(edges,vertices)
        //The computation graph
        let g2 :(ArrayAdjacencyGraph<int,int*int,float>)        = ArrayAdjacencyGraph(edges2,verticesUpdated)


        let louvainCycleInPlace (graph:ArrayAdjacencyGraph<int,int*int,float>) (randomized:bool) (modularityIncreaseThreshold: float) (numberOfLoops:int) (previousModularity:float) :(int*ArrayAdjacencyGraph<int,int*int,float>*float)=
                
            //Array of all vertices in the graph
            let verti =
                graph.GetVertices()
            
            //Shuffles the verti array if radomize is true
            if randomized then
                Randomize.shuffle verti|>ignore

            //Total weight of all edges combined
            let totalWeight =      
                    [|
                        for i in graph.GetVertices() do
                            //graph.WeightedDegree ((Array.sumBy(fun (s,t,w) -> if s = t then (w/2.) else w)),i)
                            //graph.WeightedDegree ((Array.sumBy(fun (s,t,w) -> (w))),i)
                            graph.WeightedDegree ((id),i)
                    |]
                    |> Array.sum
            

            //Array of all neighbouring vertices, returned as (vertex,edgeweight) array. The index of the element is the same as the vertex in verti.
            let neighbours =
                [|
                    for i in verti do
                        (graph.GetConnectedEdges i).ToArray()
                        |> Array.map(fun (s, t, w) ->
                            if s=i then (t,w)
                            else (s,w))
                        |> Array.sortBy fst
                        
                |]
            
            //weighted Degree of the vertex. The index of the element is the same as the vertex in verti.
            let ki =
                [|
                    for i in verti do 
                        //graph.WeightedDegree ((Array.sumBy(fun (s,t,w) -> if s=t then (w/2.) else w)),i)
                        graph.WeightedDegree ((id),i)

                |]
                                                                              
            //The weight of all self-referencing loops of the vertices. The index of the element is the same as the vertex in verti.
            let selfLoops =                                                
                //[|
                //    for i in verti do 
                //        graph.WeightedDegree ((Array.sumBy(fun (s,t,w) -> if s=t then (w/2.) else 0.)),i)

                //|]
                [|
                    for i=0 to verti.Length-1 do
                        neighbours.[i]
                        |> Array.sumBy (fun (v,w) -> if v=(verti.[i]) then 2.*w else 0.)
                |]

            //A Dictionary, where the key is the community and the value is a tupel of the weighted degree of the community and the sum of all internal edges.
            let communitySumtotalSumintern =
                let output = System.Collections.Generic.Dictionary<int,float*float>() 
                for i=0 to graph.VertexCount-1 do
                    let vertex = verti.[i]
                    let originalLabel,label = graph.GetLabel vertex
                    let communityWeightTotalStart =  ki.[i]
                    let selfLoopsStart = selfLoops.[i] 
                    output.Add(label,(communityWeightTotalStart,selfLoopsStart))
                output       
            
            //Function to calculate the modularity of the graph.
            let modularityQuality resolution =
                let mutable q = 0.
                for i in communitySumtotalSumintern do
                    let (totalSumC,sumIntern) = i.Value
                    if totalSumC > 0. then 
                        let calculation = resolution*((sumIntern/2.)/(totalWeight/2.))-((totalSumC/totalWeight)**2.)

                        q <- (q+(calculation))
                q

            let newModQ resolution :float =
                let f = resolution
                let mutable q = 0. 
                for i in communitySumtotalSumintern do
                    let (totalSumC,sumIntern) = i.Value
                    if totalSumC > 0. then 
                        let calculation = ((sumIntern)-(totalSumC*totalSumC) / totalWeight)
                
                        q <- (q+(calculation))

                (q/totalWeight)
                       
            //Minimal increase in modularity Quality that has to be achieved. If the increase in modularity is lower, then the first phase of the louvain Algorithm ends and a new iteration can begin.
            let increaseMin = modularityIncreaseThreshold //0.000001

            //Runs once over all vertices in the graph and move the vertex into the community to which the modularity gain is maximal. In case of no positive gain, the original community is kept.
            let rec louvainOneLevel (counter:int) (nbOfMoves:int) =
                
                //Do until
                if counter = graph.VertexCount then 

                    nbOfMoves > 0

                else            
                       
                    //Vertex that is looked at.
                    let node                                 = verti.[counter]
                    
                    //The weighted degree of the node.
                    let ki                                   = ki.[counter] 

                    //The weight of all self-referencing loops of the vertex.
                    let selfloopNode                         = selfLoops.[counter]
                    
                    //Community of the node before potential improvement.
                    let (fixedCommunity,originalCommunity)   = (graph.GetLabel node)

                    //Weighted degree of the community,the sum of all internal edges.
                    let (originalCommunityTotalSum,originalCommunitySumIntern)       = Dictionary.getValue originalCommunity communitySumtotalSumintern
                              
                    //Remove node from its original community.                   
                    graph.SetLabel(node,(fixedCommunity,-1)) |> ignore

                    //All neighbors of the node with their edgeWeight.         
                    let neighbors           = 
                       
                        neighbours.[counter]
                        |> Array.filter (fun (vertex,weight) -> vertex <> node) 
                   
                    //This if condition prevents problems If the node is isolated and has 0 edges. 
                    if neighbors = Array.empty then  
                           
                        graph.SetLabel(node,(fixedCommunity, originalCommunity))|> ignore
                        louvainOneLevel (counter+1) (nbOfMoves)
                   
                    else
                                      
                        //All communities the node is connected to with their edgeweight.
                        let connectedCommunities     = 
                                                  
                            neighbors
                            |> Array.map (fun (vertex,weight) -> (((graph.GetLabel vertex)|>snd),weight)) 
                           
                        //All communities the node is connected to with their edgeweight, removing duplicates. 
                        let connectedCommunitiesCondensed =
                           
                            GroupingFunctions.sumGroupBy fst snd connectedCommunities        
                           
                        //All weights to the original community of the node.
                        let weightofConnectionToOldCommunity         =   
                           
                            GroupingFunctions.findWeightofConnectionToOldCommunity connectedCommunitiesCondensed originalCommunity

                        //Removing the node from its community, updating community values communityWeightTotal and sumIntern.
                        let communityWeightTotalUpdate =  (originalCommunityTotalSum-ki)
                        let sumInternUpdate            =  (originalCommunitySumIntern-((2.*(weightofConnectionToOldCommunity))+(selfloopNode)))                  

                        communitySumtotalSumintern.Item originalCommunity <- (communityWeightTotalUpdate,sumInternUpdate)

                        let connectedCommunitiesCondensedNew =
                            Array.append [|originalCommunity,weightofConnectionToOldCommunity|] connectedCommunitiesCondensed
                            |> Array.distinct

                        //Calculating the best possible community for the node, based on modularity gain. 
                        //Outputs the bestCommunity, the gain acived by moving the node to that community and the weight of the connection to that new Community.  
                        let (bestCommunity,modularityGain,connectionToBestCommunity) =                        

                            let calculations = 
                                connectedCommunitiesCondensedNew
                                |> Array.map (fun (community,connectionToCommunity) -> 
                                        (
                                        community,
                                        (resolution*connectionToCommunity-((Dictionary.getValue community communitySumtotalSumintern|>fst)*ki/totalWeight)),
                                        connectionToCommunity
                                        )
                                    )

                            calculations
                            |> Array.maxBy (fun (community,modularityGain,connectionToCommunity) -> modularityGain)
                        
                        //If there is a gain in modularity bigger than 0.
                        if modularityGain < 0.  then 
                           
                            //Resetting the community to its original state.                       
                            graph.SetLabel (node,(fixedCommunity,originalCommunity)) |> ignore
                            communitySumtotalSumintern.Item originalCommunity <- (originalCommunityTotalSum,originalCommunitySumIntern)
                       
                            louvainOneLevel (counter+1) (nbOfMoves)

                        else                                           
                            let (communityNewSum,communityNewIn) = Dictionary.getValue bestCommunity communitySumtotalSumintern

                            //Moving the node to its new community.
                            let sumInternBestCommunity              =      (communityNewIn+((2.*(connectionToBestCommunity)+(selfloopNode))))
                            let communityWeightTotalBestCommunity   =      (communityNewSum+ki)
                           
                            graph.SetLabel (node,(fixedCommunity,bestCommunity)) |> ignore
                            communitySumtotalSumintern.Item bestCommunity <- (communityWeightTotalBestCommunity,sumInternBestCommunity)

                            (if bestCommunity <> originalCommunity then (nbOfMoves+1) else nbOfMoves)
                            |> louvainOneLevel (counter+1) 
         
            //A loop that executes louvainOneLevel as long as none of the exit conditions are met.
            //The exit conditions are
            // 1) No improvement was preformed 
            // 2) The increase in modularityQuality by preforming the louvainOneLevel results in a score lower than the increaseMin.
            let rec loop nbOfMoves currentQuality improvement :(int*ArrayAdjacencyGraph<int,int*int,float>*float)=

                let qualityNew = modularityQuality resolution

                let build (shouldIBuild:bool) :int*ArrayAdjacencyGraph<int,(int*int),float>*float=

                    if not shouldIBuild then
                        failwith "ERROR"
                    else
                       
                       //Returns a Map oldCommunity -> updatedCommunity; Returns a dictionary where the key is the vertex and the value is the new community
                        let (vertexToLabelMap,vertexNewLabel) :((Map<int,int>)*(Dictionary<int,int>))=
                            let labelMap =
                                graph.GetLabels()
                                 |> Array.map snd
                                |> Array.distinct
                                |> Array.mapi (fun i label -> (label,i))
                                |> Map.ofArray
                            let labelMap2 = 
                                [|
                                    for (oldCommunity,newCommunity) in graph.GetLabels() do
                                        oldCommunity,labelMap.[newCommunity]
                                |]
                                |> Map.ofArray

                            let vertexDict = System.Collections.Generic.Dictionary<int,int>()
                            for i in verti do
                                vertexDict.Add (i,(labelMap.[(graph.GetLabel i)|>snd]))

                            labelMap2,vertexDict                         
                        
                        //Updates the second set of labels in the outputgraph
                        for i in g.GetVertices() do
                            let (originalLabel,currentLabel) = g.GetLabel(i)
                            let updateLabel     = vertexToLabelMap.[currentLabel]
                            g.SetLabel(i,(originalLabel,updateLabel))
                            |> ignore
                        
                        //Returns the vertices for the next iteration of the louvain algorithm.
                        let vert = 
                            vertexToLabelMap
                            |> Map.toArray
                            |> Array.map snd
                            |> Array.distinct
                            |> Array.map (fun x -> (x,(x,x)))
                            |> Array.toList
                        
                        //Return the edgeList for the next iteration of the louvain algorithm.
                        let edgeListUpdated :(int*int*float)[]=

                            let getLabel vertex =
                                Dictionary.getValue vertex vertexNewLabel
                            
                            //let edgeListToSum :(int*int*float)[] =
                            //    [|
                            //        for vertex in verti do
                            //            graph.GetConnectedEdges vertex
                            //    |]
                            //    |> Array.concat
                            //    |> Array.map(fun (s,t,w) -> if s=t then (s,t,(w/2.)) else (s,t,w))

                            //let edgesToLabelEdges :(int*int*float)[] =
                            //    edgeListToSum
                            //    |> Array.map (fun (s,t,w) -> ((getLabel s),(getLabel t),w))

                            let edgesToLabelEdges :(int*int*float)[] = 
                                //let result = Array.zeroCreate (graph.AdjacencyGraph()).Count
                                //let mutable i = 0
                                //for group in (graph.AdjacencyGraph()) do
                                //    result.[i] <- group.Value
                                //    i <- i+1
                                //result
                                //|> Array.concat
                                [|
                                    for vertex in verti do
                                        (graph.GetConnectedEdges vertex).ToArray()
                                |]
                                |> Array.concat
                                |> Array.map (fun (s,t,w) -> ((getLabel s),(getLabel t),w))

                            let output = System.Collections.Generic.Dictionary<int*int,float>()
                            for (s,t,w) in edgesToLabelEdges do
                                if output.ContainsKey (s,t) then 
                                    let value = Dictionary.getValue ((s,t)) output
                                    output.Item ((s,t)) <- (value+(w/2.))
    
                                elif output.ContainsKey (t,s) then
                                    let value = Dictionary.getValue ((t,s)) output
                                    output.Item ((s,t)) <- (value+(w/2.))
    
                                else
                                    
                                    output.Add ((s,t),(w/2.))

                            let result = Array.zeroCreate output.Count
                            let mutable i = 0
                            for group in output do
                                let (s,t)   = group.Key
                                let (w)     = group.Value
                                result.[i] <- (s,t,w)
                                i <- i + 1
                            result
                        
                            

                        nbOfMoves,                                    
                        ArrayAdjacencyGraph(
                            (vert),
                            (edgeListUpdated |> Array.toList)
                        ),
                        qualityNew
                
                //Start of the cycle
                if nbOfMoves = 0 then 

                    let hasImProved = louvainOneLevel 0 0
                    
                    loop (nbOfMoves+1) currentQuality hasImProved
          
                   
                elif improvement then 
                      
                    if (qualityNew-currentQuality) > increaseMin then 

                        loop (nbOfMoves+1) (qualityNew) (louvainOneLevel 0 0)

                    else                    

                        build true


                elif numberOfLoops > 0 && currentQuality < previousModularity then

                    nbOfMoves,
                    graph,
                    qualityNew

                elif improvement = false && nbOfMoves = 1 then 

                    nbOfMoves,
                    graph,
                    qualityNew

                else 

                    build true
                    
            //Start the louvainApplication
            loop 0 (modularityQuality resolution) false

        //The louvainLoop combines the two phases of the louvain Algorithm. As long as improvments can be performed, the louvainApplication is executed.
        let rec louvainInPlace_ nbOfLoops (newG:ArrayAdjacencyGraph<int,int*int,float>) (modularityIncreaseThreshold: float) (modulartiy:float) =
        
            let (nbOfMoves,newGraph,newModularity) = 
            
                louvainCycleInPlace newG randomized modularityIncreaseThreshold nbOfLoops modulartiy           

            if nbOfMoves < 2 || ((nbOfLoops>0) && (newModularity<modulartiy)) then 
                

                printfn "new modularity= %A" modulartiy
                g

            else 

                louvainInPlace_ (nbOfLoops+1) newGraph modularityIncreaseThreshold newModularity


        louvainInPlace_ 0 g2 modularityIncreaseThreshold 0.


    /// Takes an ArrayAdjacencyGraph and returns a new graph whose Labels have been transformed into tupels, where the second part is the community accorging to modularity-optimization. 
    /// Parameters:
    ///
    /// graph : ArrayAdjacencyGraph, that is used as the template for the modularity optimization.
    ///
    /// modularityIncreaseThreshold : Threshold of modularity-difference that has to be exceeded in order to be recognized as a modularity-increase.
    /// The value has to be between 0. and 1. to get a meaningful result. The smaller the value, the longer the calculation takes.
    let louvain (modularityIncreaseThreshold: float) (graph:ArrayAdjacencyGraph<'Vertex,'Label,float>) : (ArrayAdjacencyGraph<'Vertex,'Label*int,float>) =
        louvainMethod graph false modularityIncreaseThreshold 1.
    
    /// Takes an ArrayAdjacencyGraph and returns a new graph whose Labels have been transformed into tupels, where the second part is the community accorging to modularity-optimization.
    /// In addition, the modularity optimization is carried out in a random order
    ///
    /// Parameters:
    ///
    /// graph : ArrayAdjacencyGraph, that is used as the template for the modularity optimization.
    ///
    /// modularityIncreaseThreshold : Threshold of modularity-difference that has to be exceeded in order to be recognized as a modularity-increase.
    /// The value has to be between 0. and 1. to get a meaningful result. The smaller the value, the longer the calculation takes.
    let louvainRandom (modularityIncreaseThreshold: float) (graph:ArrayAdjacencyGraph<'Vertex,'Label,float>) : (ArrayAdjacencyGraph<'Vertex,'Label*int,float>)=
        louvainMethod graph true modularityIncreaseThreshold 1.

    /// Takes an ArrayAdjacencyGraph and returns a new graph whose Labels have been transformed into tupels, where the second part is the community accorging to modularity-optimization.
    /// Parameters:
    ///
    /// graph : ArrayAdjacencyGraph, that is used as the template for the modularity optimization.
    ///
    /// randomized : Boolean, if true randomizes the order in which the vertices are checked. Else the calculations are ordered by the index of the vertices.
    ///
    /// modularityIncreaseThreshold : Threshold of modularity-difference that has to be exceeded in order to be recognized as a modularity-increase.
    /// The value has to be between 0. and 1. to get a meaningful result. The smaller the value, the longer the calculation takes.
    ///
    ///resolution : The higher the resolution, the smaller the number of communities. The value has to be 1. or higher. Based on : "R. Lambiotte, J.-C. Delvenne, M. Barahona Laplacian Dynamics and Multiscale Modular Structure in Networks 2009", 	arXiv:0812.1770 [physics.soc-ph].
    let louvainResolution (randomized: bool) (modularityIncreaseThreshold: float) (resolution: float) (graph:ArrayAdjacencyGraph<'Vertex,'Label,float>) : (ArrayAdjacencyGraph<'Vertex,'Label*int,float>) =
        louvainMethod graph randomized modularityIncreaseThreshold resolution

