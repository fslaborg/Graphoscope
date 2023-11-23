namespace Graphoscope.Algorithms

open Graphoscope
open System.Collections.Generic
open FSharpAux

module private LouvainHelpers =
    let updateNeighborWeights (nIx: int) (getWeight: 'EdgeData -> float) (weights: Dictionary<int, float>) (node2Community: int []) (edges: ResizeArray<ResizeArray<int * 'EdgeData>>) =
        edges[nIx]
        |> ResizeArray.iter(fun (nbr, ed) ->
            if nIx <> nbr then
                weights
                |> Dictionary.addOrUpdateInPlaceBy ((+)) node2Community[nbr] (getWeight ed)
                |> ignore
        )

    let getNeighborCommunityWeights (neighbors: Dictionary<int, float>) (node2Community: int [])  =
        let weights: Dictionary<int, float> = Dictionary()
        for KeyValue(nbr, w) in neighbors do
            weights
            |> Dictionary.addOrUpdateInPlaceBy ((+)) node2Community[nbr] w
            |>ignore 

        weights

    module UndirectedGraph =
        let getAllNeighborWeights (graph: UndirectedGraph<_, _, 'EdgeData>) (getWeight: 'EdgeData -> float) (node2Community: int []) =
            fun nIx ->
                let weights: Dictionary<int, float> = Dictionary()
                updateNeighborWeights nIx getWeight weights node2Community  graph.Edges
                weights
            |> Array.init graph.NodeKeys.Count

        let oneLevel (getWeight: 'EdgeData -> float) (m: float) (resolution: float) (prevPartition: int Set array option) (graph: UndirectedGraph<'NodeKey, _, 'EdgeData>) (rng: unit -> float)=
            let nodeIdxs = Array.init graph.NodeKeys.Count id |> Array.sortBy (fun _ -> rng())
            let node2Community = Array.init graph.NodeKeys.Count id

            let neighbors = getAllNeighborWeights graph getWeight node2Community
            
            let degrees = graph.Edges |> ResizeArray.map(fun x -> (0.,x)||>ResizeArray.fold(fun acc (_, ed) -> acc + getWeight ed))
            let sigmaTot = degrees |> Array.ofSeq
            
            let mutable improvement = false

            let rec loop () = 
                let mutable moveCounter = 0
                nodeIdxs
                |> Array.iter(fun nIx ->
                    let neighborCommunities = getNeighborCommunityWeights neighbors[nIx] node2Community

                    let degree = degrees[nIx]
                    let mutable bestCommunity  = node2Community[nIx]
                    let mutable bestModularity  = 0.

                    sigmaTot[bestCommunity] <- sigmaTot[bestCommunity] - degree
                    
                    let diC =
                        neighborCommunities
                        |> Dictionary.tryFind bestCommunity
                        |> Option.defaultValue 0.
             
                    let removalCost = 
                        -diC / m
                        + resolution
                        * (degree * sigmaTot[bestCommunity])
                        / (2. * m**2)
                    
                    neighborCommunities
                    |> Seq.iter(fun (KeyValue(nc, w)) ->
                        let gain =
                            removalCost
                            + w / m
                            - resolution
                            * (degree * sigmaTot[nc])
                            / (2. * m**2)
                        if gain > bestModularity then
                            bestModularity <- gain
                            bestCommunity <- nc
                    )

                    sigmaTot[bestCommunity] <- sigmaTot[bestCommunity] + degree
                    if bestCommunity <> node2Community[nIx] then
                        moveCounter <- moveCounter + 1
                        improvement <- true

                        node2Community[nIx] <- bestCommunity
                )

                if moveCounter > 0 then loop ()

            loop ()

            let prePartition =
                node2Community
                |> Seq.indexed
                |> Seq.groupBy snd
                |> Seq.cache

            let innerPartition =
                prePartition
                |> Seq.map (fun (_,community) ->
                    community
                    |> Seq.map(fun (ix,_) -> graph.NodeKeys[ix])
                    |> Set
                )
                |> Array.ofSeq

            let partition =
                prePartition
                |> Seq.map (fun (_,community) ->
                    prevPartition
                    |> Option.map(fun p -> community |>Seq.collect(fun (ix,_) ->p[ix]))
                    |> Option.defaultValue (community|>Seq.map fst)
                    |> Set.ofSeq
                )
                |> Array.ofSeq

            node2Community, innerPartition, partition, improvement

        let genSuperNodeGraph (getWeight: 'EdgeData -> float) (node2Community: int []) (graph: UndirectedGraph<'NodeKey, _, 'EdgeData>) : UndirectedGraph<int,_,float> =
            let communities = node2Community |> Array.distinct

            let g = UndirectedGraph.createFromNodes (Array.zip communities communities)
            let edges = 
                graph.Edges
                |> Seq.mapi(fun i x -> x|>Seq.map(fun (dest,w) -> i, dest, w))
                |> Seq.concat
                |> Seq.groupBy(fun (origin, destination, _) ->
                    node2Community[origin], node2Community[destination]
                )
                |> Seq.map(fun ((origin, destination), edges) ->
                    origin, destination, edges|> Seq.sumBy(fun (_,_,ed) -> getWeight ed)
                )
                |> Array.ofSeq
            g
            |>UndirectedGraph.addEdges edges

    module DiGraph =
        let getAllNeighborWeights (graph: DiGraph<_, _, 'EdgeData>) (getWeight: 'EdgeData -> float) (node2Community: int []) =
            fun nIx ->
                let weights: Dictionary<int, float> = Dictionary()
                let updateWeights =
                    updateNeighborWeights nIx getWeight weights node2Community 
                
                updateWeights graph.OutEdges
                updateWeights graph.InEdges

                weights
            |> Array.init graph.NodeKeys.Count


        // https://hal.science/hal-01231784
        let oneLevel (getWeight: 'EdgeData -> float) (m: float) (resolution: float) (prevPartition: int Set array option) (graph: DiGraph<'NodeKey, _, 'EdgeData>) (rng: unit -> float)=
            let nodeIdxs = Array.init graph.NodeKeys.Count id |> Array.sortBy (fun _ -> rng())
            let node2Community = Array.init graph.NodeKeys.Count id

            let neighbors = getAllNeighborWeights graph getWeight node2Community

            let inDegrees = graph.InEdges |> ResizeArray.map(fun x -> (0.,x)||>ResizeArray.fold(fun acc (_, ed) -> acc + getWeight ed))
            let outDegrees = graph.OutEdges |> ResizeArray.map(fun x -> (0.,x)||>ResizeArray.fold(fun acc (_, ed) -> acc + getWeight ed))
            let sigmaTotIn = inDegrees |> Array.ofSeq
            let sigmaTotOut = outDegrees |> Array.ofSeq
    
            let mutable improvement = false

            let rec loop () = 
                let mutable moveCounter = 0
                nodeIdxs
                |> Array.iter(fun nIx ->
                    let neighborCommunities = getNeighborCommunityWeights neighbors[nIx] node2Community

                    let inDegree = inDegrees[nIx]
                    let outDegree = outDegrees[nIx]
                    let mutable bestCommunity  = node2Community[nIx]
                    let mutable bestModularity  = 0.
                    sigmaTotIn[bestCommunity] <- sigmaTotIn[bestCommunity] - inDegree
                    sigmaTotOut[bestCommunity] <- sigmaTotOut[bestCommunity] - outDegree
                    
                    let diC =
                        neighborCommunities
                        |> Dictionary.tryFind bestCommunity
                        |> Option.defaultValue 0.
             
                    let removalCost = 
                        -diC / m
                        + resolution
                        * (outDegree * sigmaTotIn[bestCommunity] + inDegree * sigmaTotOut[bestCommunity])
                        / m**2
                    
                    neighborCommunities
                    |> Seq.iter(fun (KeyValue(nc, w)) ->
                        let gain =
                            removalCost
                            + w / m
                            - resolution
                            * (outDegree * sigmaTotIn[nc]  + inDegree * sigmaTotOut[nc])
                            / m**2
                        if gain > bestModularity then
                            bestModularity <- gain
                            bestCommunity <- nc
                    )

                    sigmaTotIn[bestCommunity] <- sigmaTotIn[bestCommunity] + inDegree
                    sigmaTotOut[bestCommunity] <- sigmaTotOut[bestCommunity] + outDegree
                    if bestCommunity <> node2Community[nIx] then
                        moveCounter <- moveCounter + 1
                        improvement <- true

                        node2Community[nIx] <- bestCommunity
                )

                if moveCounter > 0 then loop ()

            loop ()

            let prePartition =
                node2Community
                |> Seq.indexed
                |> Seq.groupBy snd
                |> Seq.cache

            let innerPartition =
                prePartition
                |> Seq.map (fun (_,community) ->
                    community
                    |> Seq.map(fun (ix,_) -> graph.NodeKeys[ix])
                    |> Set
                )
                |> Array.ofSeq

            let partition =
                prePartition
                |> Seq.map (fun (_,community) ->
                    prevPartition
                    |> Option.map(fun p -> community |>Seq.collect(fun (ix,_) ->p[ix]))
                    |> Option.defaultValue (community|>Seq.map fst)
                    |> Set.ofSeq
                )
                |> Array.ofSeq

            node2Community, innerPartition, partition, improvement

        let genSuperNodeGraph (getWeight: 'EdgeData -> float) (node2Community: int []) (graph: DiGraph<'NodeKey, _, 'EdgeData>) : DiGraph<int,_,float> =
            let communities = node2Community |> Array.distinct

            let g = DiGraph.createFromNodes (Array.zip communities communities)
            let edges = 
                graph.InEdges
                |> Seq.mapi(fun i x -> x|>Seq.map(fun (dest,w) -> i, dest, w))
                |> Seq.concat
                |> Seq.groupBy(fun (origin, destination, _) ->
                    node2Community[origin], node2Community[destination]
                )
                |> Seq.map(fun ((origin, destination), edges) ->
                    origin, destination, edges|> Seq.sumBy(fun (_,_,ed) -> getWeight ed)
                )
                |> Array.ofSeq
            g
            |>DiGraph.addEdges edges


///Louvain method for community detection
//Blondel, Vincent D; Guillaume, Jean-Loup; Lambiotte, Renaud; Lefebvre, Etienne (9 October 2008). "Fast unfolding of communities in large networks". Journal of Statistical Mechanics: Theory and Experiment. 2008
type Louvain() =                   

    /// <summary> 
    /// Takes a AdjGraph and returns a new graph whose NodeData has been transformed into tupels, where the second part is the community according to modularity-optimization by the Louvain Algorithm (Blondel, Vincent D; Guillaume, Jean-Loup; Lambiotte, Renaud; Lefebvre, Etienne (9 October 2008). "Fast unfolding of communities in large networks". Journal of Statistical Mechanics: Theory and Experiment. 2008 ).
    /// </summary>
    /// <param name="randomized">Boolean, if true randomizes the order in which the vertices are checked. Else the calculations are ordered by the index of the vertices.</param> 
    /// <param name="weightF">Function to get an edgeweight out of the EdgeData</param> 
    /// <param name="modularityIncreaseThreshold">Threshold of modularity-difference that has to be exceeded in order to be recognized as a modularity-increase. The value has to be between 0. and 1. to get a meaningful result. The smaller the value, the longer the calculation takes.</param> 
    /// <param name="resolution">The higher the resolution, the smaller the number of communities. The value has to be 1. or higher. Based on : "R. Lambiotte, J.-C. Delvenne, M. Barahona Laplacian Dynamics and Multiscale Modular Structure in Networks 2009", 	arXiv:0812.1770 [physics.soc-ph].</param> 
    /// <param name="graph">AdjGraph, that is used as the template for the modularity optimization.</param> 
    /// <returns>A new AdjGraph whose NodeData has been transformed into tupels, where the second part is the community accorging to modularity-optimization.</returns>
    static member louvainResolution (randomized: bool) (weightF:'Edge -> float) (modularityIncreaseThreshold: float) (resolution: float) (graph:AdjGraph<'Node,'Label,'Edge>) : (AdjGraph<'Node,'Label*int,'Edge>) =

                //Return the value to the key k if it is bound, else fail.        
        //Return the value to the key k if it is bound, else fail.        
        let getValue k (dict:Dictionary<'K,'V>) =
            try 
                dict.Item k
            with
            | _ -> failwithf "Error get k %O dict %O" k dict

        //All functions connected to the randomization progress.
        //Swaps the position of item x and item y in the array a.
        let swap (a: _[]) x y =
            
            let tmp = a.[x]
            a.[x] <- a.[y]
            a.[y] <- tmp

        // shuffle an array (in-place)
        let shuffle a =
            let rand = new System.Random()
            Seq.iteri (fun i _ -> swap a i (rand.Next(i, Seq.length a))) a  


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


        let louvainMethod (g:AdjGraph<'Node,'Label,'Edge>) (weightF:'Edge -> float) (randomized:bool) (modularityIncreaseThreshold: float) (resolution: float) : (AdjGraph<'Node,'Label*int,'Edge>) =

            let nodeToCommunity =
                g.Keys
                |>Seq.mapi(fun i x -> x,i)
                |>Map.ofSeq

            let graphSeq:seq<'Node*'Label*'Node*'Label*'Edge>=
                g
                |> AdjGraph.toSeq

            let copiedGraph :AdjGraph<'Node,'Label*int,'Edge> =
                graphSeq
                |>Seq.map(fun (nk1,nd1,nk2,nd2,e) ->
                    nk1,(nd1,(nodeToCommunity.Item nk1)),
                    nk2,(nd2,(nodeToCommunity.Item nk2)),
                    e
                )
                |>AdjGraph.ofSeq

            let copiedGraph2:AdjGraph<int,int*int,float> =
                graphSeq
                |>Seq.map(fun (nk1,nd1,nk2,nd2,e) ->
                    let nk1I = nodeToCommunity.Item nk1
                    let nk2I = nodeToCommunity.Item nk2
                    
                    nk1I,(nk1I,(nk1I)),
                    nk2I,(nk2I,(nk2I)),
                    weightF e
                )
                |>AdjGraph.ofSeq

            let louvainCycleInPlace (graph:AdjGraph<int,int*int,float>) (randomized:bool) (modularityIncreaseThreshold: float) (numberOfLoops:int) (previousModularity:float) :(int*AdjGraph<int,int*int,float>*float) =
                    
                //Array of all vertices in the graph
                let verti =
                    graph.Keys|>Array.ofSeq

                //Shuffles the verti array if radomize is true
                if randomized then
                    shuffle verti|>ignore


                //Array of all neighbouring vertices, returned as (vertex,edgeweight) array. The index of the element is the same as the vertex in verti.
                let neighbours =
                    verti
                    |> Array.map(fun x -> AdjGraph.getNeighbours x graph|>Seq.distinct|>Array.ofSeq)

                            
                //weighted Degree of the vertex. The index of the element is the same as the vertex in verti.
                let ki =
                    neighbours|>Array.map(fun adj -> Seq.sumBy snd adj)
                
                
                //Total weight of all edges combined
                let totalWeight =      
                    ki
                    |>Seq.sum
                
                //The weight of all self-referencing loops of the vertices. The index of the element is the same as the vertex in verti.
                let selfLoops =                                                
                    neighbours
                    |>Array.mapi(fun i adj ->
                        adj|> Seq.sumBy (fun (v,w) -> if v=(verti.[i]) then w else 0.)
                    )

                //A Dictionary, where the key is the community and the value is a tupel of the weighted degree of the community and the sum of all internal edges.
                let communitySumtotalSumintern =
                    let output = System.Collections.Generic.Dictionary<int,float*float>() 
                    for i=0 to (AdjGraph.countNodes graph)-1 do
                        let vertex = verti.[i]
                        let originalLabel,label = graph.Item vertex|>fun (l,_) -> l
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
                    if counter = AdjGraph.countNodes graph then 

                        nbOfMoves > 0

                    else                
                        //Vertex that is looked at.
                        let node                                 = verti.[counter]
                        
                        //The weighted degree of the node.
                        let ki                                   = ki.[counter] 

                        //The weight of all self-referencing loops of the vertex.
                        let selfloopNode                         = selfLoops.[counter]
                        
                        //Community of the node before potential improvement.
                        let (fixedCommunity,originalCommunity),_   = (graph.Item node)

                        //Weighted degree of the community,the sum of all internal edges.
                        let (originalCommunityTotalSum,originalCommunitySumIntern)       = getValue originalCommunity communitySumtotalSumintern
                                
                        //Remove node from its original community.                   
                        AdjGraph.setNodeData node (fixedCommunity,-1) graph |> ignore

                        //All neighbors of the node with their edgeWeight.         
                        let neighbors           = 
                        
                            neighbours.[counter]
                            |> Array.filter (fun (vertex,weight) -> vertex <> node) 


                        //This if condition prevents problems If the node is isolated and has 0 edges. 
                        if neighbors = Array.empty then  
                            
                            AdjGraph.setNodeData node (fixedCommunity, originalCommunity) graph|> ignore
                            louvainOneLevel (counter+1) (nbOfMoves)


                        else
                                        
                            //All communities the node is connected to with their edgeweight.
                            let connectedCommunities     = 
                                                    
                                neighbors
                                |> Array.map (fun (vertex,weight) -> (((graph.Item vertex)|>fun ((l1,l2),_) -> l2),weight)) 
                            
                            //All communities the node is connected to with their edgeweight, removing duplicates. 
                            let connectedCommunitiesCondensed =
                            
                                sumGroupBy fst snd connectedCommunities        
                            
                            //All weights to the original community of the node.
                            let weightofConnectionToOldCommunity         =   
                            
                                findWeightofConnectionToOldCommunity connectedCommunitiesCondensed originalCommunity

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
                                            (resolution*connectionToCommunity-((getValue community communitySumtotalSumintern|>fst)*ki/totalWeight)),
                                            connectionToCommunity
                                            )
                                        )

                                calculations
                                |> Array.maxBy (fun (community,modularityGain,connectionToCommunity) -> modularityGain)

                            //If there is a gain in modularity bigger than 0.
                            if modularityGain < 0.  then 

                                //Resetting the community to its original state.                       
                                AdjGraph.setNodeData node (fixedCommunity,originalCommunity) graph|> ignore
                                communitySumtotalSumintern.Item originalCommunity <- (originalCommunityTotalSum,originalCommunitySumIntern)
                        
                                louvainOneLevel (counter+1) (nbOfMoves)

                            else                                           

                                let (communityNewSum,communityNewIn) = getValue bestCommunity communitySumtotalSumintern

                                //Moving the node to its new community.
                                let sumInternBestCommunity              =      (communityNewIn+((2.*(connectionToBestCommunity)+(selfloopNode))))
                                let communityWeightTotalBestCommunity   =      (communityNewSum+ki)
                            
                                AdjGraph.setNodeData node (fixedCommunity,bestCommunity) graph|> ignore
                                communitySumtotalSumintern.Item bestCommunity <- (communityWeightTotalBestCommunity,sumInternBestCommunity)

                                (if bestCommunity <> originalCommunity then (nbOfMoves+1) else nbOfMoves)
                                |> louvainOneLevel (counter+1) 
            
                //A loop that executes louvainOneLevel as long as none of the exit conditions are met.
                //The exit conditions are
                // 1) No improvement was preformed 
                // 2) The increase in modularityQuality by preforming the louvainOneLevel results in a score lower than the increaseMin.
                let rec loop nbOfMoves currentQuality improvement :(int*AdjGraph<int,int*int,float>*float)=

                    let qualityNew = modularityQuality resolution

                    let build (shouldIBuild:bool) :int*AdjGraph<int,int*int,float>*float=

                        if not shouldIBuild then
                            failwith "ERROR"
                        else
                        
                        //Returns a Map oldCommunity -> updatedCommunity; Returns a dictionary where the key is the vertex and the value is the new community
                            let (vertexToLabelMap,vertexNewLabel) :((Map<int,int>)*(Dictionary<int,int>))=
                                let labelC = graph.Values|> Seq.map (fun ((l1,l2),_) -> l1,l2)
                                let labelMap =                           
                                    labelC
                                    |> Seq.map snd
                                    |> Seq.distinct
                                    |> Seq.mapi (fun i label -> (label,i))
                                    |> Map.ofSeq
                                let labelMap2 = 

                                    [|
                                        for (oldCommunity,newCommunity) in labelC do
                                            oldCommunity,labelMap.[newCommunity]
                                    |]
                                    |> Map.ofArray

                                let vertexDict = System.Collections.Generic.Dictionary<int,int>()

                                for i in verti do
                                    vertexDict.Add (i,(labelMap.[(graph.Item i|>fun ((l1,l2),_) -> l2)]))

                                labelMap2,vertexDict                         

                            //Updates the second set of labels in the outputgraph
                            for i in copiedGraph.Keys do

                                let (originalLabel,currentLabel) = copiedGraph.Item i|> fun (s,_) ->s
        
                                let updateLabel     = vertexToLabelMap.[currentLabel]
                                AdjGraph.setNodeData i (originalLabel,updateLabel) copiedGraph
                                |> ignore

                            //Return the edgeList for the next iteration of the louvain algorithm.
                            let elementSeq :(int*(int*int)*int*(int*int)*float)seq=

                                let getLabel vertex =
                                    getValue vertex vertexNewLabel
                                
                                AdjGraph.toSeq graph
                                |> Seq.map (fun (s,s1,t,t1,w) -> ((getLabel s),(getLabel t),w))
                                |> Seq.groupBy(fun (s,t,w) ->
                                    if s<t then 
                                        (s,t)
                                    else
                                        (t,s)
                                )
                                |> Seq.map(fun ((s,t),toSum) -> 
                                    s,(s,s),
                                    t,(t,t),
                                    toSum
                                    |>Seq.sumBy (fun (s,t,w) -> w/2.))

                                

                            nbOfMoves,                                    
                            AdjGraph.ofSeq elementSeq,
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
            let rec louvainInPlace_ nbOfLoops (newG:AdjGraph<int,int*int,float>) (modularityIncreaseThreshold: float) (modulartiy:float) =
            
                let (nbOfMoves,newGraph,newModularity) = 
                
                    louvainCycleInPlace newG randomized modularityIncreaseThreshold nbOfLoops modulartiy           

                if nbOfMoves < 2 || ((nbOfLoops>0) && (newModularity<modulartiy)) then 
                    

                    printfn "new modularity= %A" modulartiy
                    copiedGraph

                else 

                    louvainInPlace_ (nbOfLoops+1) newGraph modularityIncreaseThreshold newModularity


            louvainInPlace_ 0 copiedGraph2 modularityIncreaseThreshold 0.
        louvainMethod graph weightF randomized modularityIncreaseThreshold resolution

    /// <summary> 
    /// Takes a FGraph and returns a new graph whose NodeData has been transformed into tupels, where the second part is the community according to modularity-optimization by the Louvain Algorithm (Blondel, Vincent D; Guillaume, Jean-Loup; Lambiotte, Renaud; Lefebvre, Etienne (9 October 2008). "Fast unfolding of communities in large networks". Journal of Statistical Mechanics: Theory and Experiment. 2008 ).
    /// </summary>
    /// <param name="modularityIncreaseThreshold">Threshold of modularity-difference that has to be exceeded in order to be recognized as a modularity-increase. The value has to be between 0. and 1. to get a meaningful result. The smaller the value, the longer the calculation takes.</param> 
    /// <param name="graph">FGraph, that is used as the template for the modularity optimization.</param> 
    /// <returns>A new FGraph whose NodeData has been transformed into tupels, where the second part is the community accorging to modularity-optimization.</returns>
    static member louvain (modularityIncreaseThreshold: float) (weightF:'Edge -> float) (graph:AdjGraph<'Node,'Label,'Edge>) : (AdjGraph<'Node,'Label*int,'Edge>) =
        Louvain.louvainResolution false weightF modularityIncreaseThreshold 1. graph 

    /// <summary> 
    /// Takes a FGraph and returns a new graph whose NodeData has been transformed into tupels, where the second part is the community according to modularity-optimization by the Louvain Algorithm (Blondel, Vincent D; Guillaume, Jean-Loup; Lambiotte, Renaud; Lefebvre, Etienne (9 October 2008). "Fast unfolding of communities in large networks". Journal of Statistical Mechanics: Theory and Experiment. 2008 ).
    /// </summary>
    /// <param name="modularityIncreaseThreshold">Threshold of modularity-difference that has to be exceeded in order to be recognized as a modularity-increase. The value has to be between 0. and 1. to get a meaningful result. The smaller the value, the longer the calculation takes.</param> 
    /// <param name="graph">FGraph, that is used as the template for the modularity optimization.</param> 
    /// <returns>A new FGraph whose NodeData has been transformed into tupels, where the second part is the community accorging to modularity-optimization.</returns>
    static member louvainRandom (modularityIncreaseThreshold: float) (weightF:'Edge -> float) (graph:AdjGraph<'Node,'Label,'Edge>) : (AdjGraph<'Node,'Label*int,'Edge>)=
        Louvain.louvainResolution true weightF modularityIncreaseThreshold 1. graph 

    /// <summary> 
    /// Returns all partitions found at each level of Louvain method.
    /// </summary>
    /// <param name="getWeight">Function to get the edge weight from 'EdgeData.
    /// Optional; defaults to each edge weight being equal to 1.0.
    /// </param>
    /// <param name="rng">
    /// The random number generator to be used in the initial ordering of the nodes in the <paramref name="graph"/>.
    /// Optional; defaults to creating a System.Random object and calling `.NextDouble()` method on it.
    /// </param>
    /// <param name="resolution">If resolution is less than 1, modularity favors
    /// larger communities. Greater than 1 favors smaller communities.
    /// Optional; default = 1.0</param>
    /// <param name="threshold">
    /// The desired minimum modularity gain for each level of the algorithm
    /// Optional; default = 0.0000001
    /// </param>
    /// <param name="graph">The graph to analyse</param> 
    static member louvainPartitionsUndirected (getWeight: 'EdgeData -> float) (rng: unit -> float) (resolution: float) (threshold: float) (graph: UndirectedGraph<'NodeKey, _, 'EdgeData>) =
        let m = graph |> UndirectedGraph.Edge.sumBy getWeight 

        let rec loop (g: UndirectedGraph<int, _, float>) (partitions: int Set [] []) (modularity: float ) =
            let partition = partitions |> Array.last
            let node2Community, innerPartition, newPartition, improvement = LouvainHelpers.UndirectedGraph.oneLevel id m resolution (Some partition) g rng

            let newModularity = Measures.Modularity.ofUndirectedGraph id resolution innerPartition g
            if improvement && newModularity - modularity > threshold then
                let newGraph: UndirectedGraph<int,_,float> = LouvainHelpers.UndirectedGraph.genSuperNodeGraph id node2Community g
                loop newGraph (Array.append partitions [|newPartition|]) newModularity
            else
                partitions
                |> Array.map(fun part ->
                    part
                    |> Array.map (fun ar -> ar|>Set.map(fun ix -> graph.NodeKeys[ix]))
                )
        
        // Initial
        let node2Community, innerPartition, partition, _ = LouvainHelpers.UndirectedGraph.oneLevel getWeight m resolution None graph rng
        let initialMod = Measures.Modularity.ofUndirectedGraph getWeight resolution innerPartition graph
        let newGraph = LouvainHelpers.UndirectedGraph.genSuperNodeGraph getWeight node2Community graph

        loop newGraph [|partition|] initialMod
        
    /// <summary> 
    /// Returns all partitions found at each level of Louvain method.
    /// </summary>
    /// <param name="getWeight">Function to get the edge weight from 'EdgeData.
    /// Optional; defaults to each edge weight being equal to 1.0.
    /// </param>
    /// <param name="rng">
    /// The random number generator to be used in the initial ordering of the nodes in the <paramref name="graph"/>.
    /// Optional; defaults to creating a System.Random object and calling `.NextDouble()` method on it.
    /// </param>
    /// <param name="resolution">If resolution is less than 1, modularity favors
    /// larger communities. Greater than 1 favors smaller communities.
    /// Optional; default = 1.0</param>
    /// <param name="threshold">
    /// The desired minimum modularity gain for each level of the algorithm
    /// Optional; default = 0.0000001
    /// </param>
    /// <param name="graph">The graph to analyse</param> 
    static member louvainPartitionsDiGraph (getWeight: 'EdgeData -> float) (rng: unit -> float) (resolution: float) (threshold: float) (graph: DiGraph<'NodeKey, _, 'EdgeData>) =
        let m = graph |> DiGraph.Edge.sumBy getWeight

        let rec loop (g: DiGraph<int, _, float>) (partitions: int Set [] []) (modularity: float ) =
            let partition = partitions |> Array.last
            let node2Community, innerPartition, newPartition, improvement =
                LouvainHelpers.DiGraph.oneLevel id m resolution (Some partition) g rng
            let newModularity = Measures.Modularity.ofDiGraph id resolution innerPartition g
            if improvement && newModularity - modularity > threshold then
                let newGraph: DiGraph<int,_,float> = LouvainHelpers.DiGraph.genSuperNodeGraph id node2Community g
                loop newGraph (Array.append partitions [|newPartition|]) newModularity 
            else
                partitions
                |> Array.map(fun part ->
                    part
                    |> Array.map (fun ar -> ar|>Set.map(fun ix -> graph.NodeKeys[ix]))
                )
        
        // Initial
        let node2Community, innerPartition, partition, _ = LouvainHelpers.DiGraph.oneLevel getWeight m resolution None graph rng
        let initialMod = Measures.Modularity.ofDiGraph getWeight resolution innerPartition graph
        let newGraph = LouvainHelpers.DiGraph.genSuperNodeGraph getWeight node2Community graph

        loop newGraph [|partition|] initialMod
        
    /// <summary> 
    /// Returns all partitions found at each level of Louvain method.
    /// </summary>
    /// <param name="graph">The graph to analyse</param> 
    /// <param name="getWeight">Function to get the edge weight from 'EdgeData.
    /// Optional; defaults to each edge weight being equal to 1.0.
    /// </param>
    /// <param name="resolution">If resolution is less than 1, modularity favors
    /// larger communities. Greater than 1 favors smaller communities.
    /// Optional; default = 1.0</param>
    /// <param name="threshold">
    /// The desired minimum modularity gain for each level of the algorithm
    /// Optional; default = 0.0000001
    /// </param>
    /// <param name="rng">
    /// The random number generator to be used in the initial ordering of the nodes in the <paramref name="graph"/>.
    /// Optional; defaults to creating a System.Random object and calling `.NextDouble()` method on it.
    /// </param>
    static member louvainPartitions (graph: DiGraph<'NodeKey, _, 'EdgeData>, ?getWeight: 'EdgeData -> float, ?resolution, ?threshold: float, ?rng :unit -> float) =
        let rng = defaultArg rng (System.Random()).NextDouble
        let threshold = defaultArg threshold 1e-7
        let resolution = defaultArg resolution 1.
        let getWeight = defaultArg getWeight (fun _ -> 1.)
        Louvain.louvainPartitionsDiGraph getWeight rng resolution threshold graph

    /// <summary> 
    /// Returns all partitions found at each level of Louvain method.
    /// </summary>
    /// <param name="graph">The graph to analyse</param> 
    /// <param name="getWeight">Function to get the edge weight from 'EdgeData.
    /// Optional; defaults to each edge weight being equal to 1.0.
    /// </param>
    /// <param name="resolution">If resolution is less than 1, modularity favors
    /// larger communities. Greater than 1 favors smaller communities.
    /// Optional; default = 1.0</param>
    /// <param name="threshold">
    /// The desired minimum modularity gain for each level of the algorithm
    /// Optional; default = 0.0000001
    /// </param>
    /// <param name="rng">
    /// The random number generator to be used in the initial ordering of the nodes in the <paramref name="graph"/>.
    /// Optional; defaults to creating a System.Random object and calling `.NextDouble()` method on it.
    /// </param>
    static member louvainPartitions (graph: UndirectedGraph<'NodeKey, _, 'EdgeData>, ?getWeight: 'EdgeData -> float, ?resolution, ?threshold: float, ?rng :unit -> float) =
        let rng = defaultArg rng (System.Random()).NextDouble
        let threshold = defaultArg threshold 1e-7
        let resolution = defaultArg resolution 1.
        let getWeight = defaultArg getWeight (fun _ -> 1.)
        Louvain.louvainPartitionsUndirected getWeight rng resolution threshold graph

    /// <summary> 
    /// Finds optimal partition of <paramref name="graph"/> nodes into communities using Louvain method.
    /// </summary>
    /// <param name="graph">The graph to analyse</param> 
    /// <param name="getWeight">Function to get the edge weight from 'EdgeData.
    /// Optional; defaults to each edge weight being equal to 1.0.
    /// </param>
    /// <param name="resolution">If resolution is less than 1, modularity favors
    /// larger communities. Greater than 1 favors smaller communities.
    /// Optional; default = 1.0</param>
    /// <param name="threshold">
    /// The desired minimum modularity gain for each level of the algorithm
    /// Optional; default = 0.0000001
    /// </param>
    /// <param name="rng">
    /// The random number generator to be used in the initial ordering of the nodes in the <paramref name="graph"/>.
    /// Optional; defaults to creating a System.Random object and calling `.NextDouble()` method on it.
    /// </param>
    static member louvainCommunities (graph: DiGraph<'NodeKey, _, 'EdgeData>, ?getWeight: 'EdgeData -> float, ?resolution, ?threshold: float, ?rng :unit -> float) =
        let rng = defaultArg rng (System.Random()).NextDouble
        let threshold = defaultArg threshold 1e-7
        let resolution = defaultArg resolution 1.
        let getWeight = defaultArg getWeight (fun _ -> 1.)

        Louvain.louvainPartitions(graph, getWeight, resolution, threshold, rng)
        |> Array.last

    /// <summary> 
    /// Finds optimal partition of <paramref name="graph"/> nodes into communities using Louvain method.
    /// </summary>
    /// <param name="graph">The graph to analyse</param> 
    /// <param name="getWeight">Function to get the edge weight from 'EdgeData.
    /// Optional; defaults to each edge weight being equal to 1.0.
    /// </param>
    /// <param name="resolution">If resolution is less than 1, modularity favors
    /// larger communities. Greater than 1 favors smaller communities.
    /// Optional; default = 1.0</param>
    /// <param name="threshold">
    /// The desired minimum modularity gain for each level of the algorithm
    /// Optional; default = 0.0000001
    /// </param>
    /// <param name="rng">
    /// The random number generator to be used in the initial ordering of the nodes in the <paramref name="graph"/>.
    /// Optional; defaults to creating a System.Random object and calling `.NextDouble()` method on it.
    /// </param>
    static member louvainCommunities (graph: UndirectedGraph<'NodeKey, _, 'EdgeData>, ?getWeight: 'EdgeData -> float, ?resolution, ?threshold: float, ?rng :unit -> float) =
        let rng = defaultArg rng (System.Random()).NextDouble
        let threshold = defaultArg threshold 1e-7
        let resolution = defaultArg resolution 1.
        let getWeight = defaultArg getWeight (fun _ -> 1.)
        
        Louvain.louvainPartitions(graph, getWeight, resolution, threshold, rng)
        |> Array.last
