namespace Graphoscope

open FSharpAux
open System.Collections.Generic



/// <summary> 
/// A adjacency matrix storing additional node information 
/// </summary>
type AdjMatrix<'NodeKey, 'NodeData, 'EdgeData when 'NodeKey : comparison>
    (edgeData : 'EdgeData [,], nodeData : array<'NodeData>, nodekeyIndex : Dictionary<'NodeKey, int>) =

    new (nodeCount : int) = 
        let n = nodeCount - 1
        let nodeData     = Array.zeroCreate n
        let nodekeyIndex = Dictionary<'NodeKey, int>(n)
        let adjMatrix    = Array2D.zeroCreate n n        
        AdjMatrix(adjMatrix, nodeData, nodekeyIndex)

    member this.Item(n:int, m:int) = 
         edgeData.[n,m]

    member this.Bykey(sourceKey:'NodeKey, targetKey:'NodeKey) =
        edgeData.[nodekeyIndex.[sourceKey], nodekeyIndex.[targetKey]]    
    
    member this.Nodes = nodeData
    
    member this.NodesByKey(key:'NodeKey) = nodeData.[nodekeyIndex.[key]]

    // member this.AddElement (sourceKey : 'NodeKey) (source : 'NodeData)  (targetKey : 'NodeKey) (target : 'NodeData) (data : 'EdgeData) =
    //     nodekeyIndex

/// <summary> 
/// Basic Adjacency Graph representation
/// </summary>
type AdjGraph<'NodeKey, 'NodeData, 'EdgeData> when 'NodeKey: comparison =
   Dictionary<'NodeKey, 'NodeData * Dictionary<'NodeKey, 'EdgeData>>

/// <summary> 
/// Functions to operate on the AdjGraph representation
/// </summary>
type AdjGraph() =

    /// <summary> 
    /// Creates an empty Adjacency Graph
    /// </summary>
    /// <returns>Empty AdjGraph</returns>
    static member create<'NodeKey, 'NodeData, 'EdgeData when 'NodeKey : comparison>()
        : AdjGraph<'NodeKey, 'NodeData, 'EdgeData> =
        AdjGraph<'NodeKey, 'NodeData, 'EdgeData>()

    /// <summary> 
    /// Adds an edge and the corresponding nodes with data to the graph
    /// </summary>
    static member addElement (nk1 : 'NodeKey) (nd1 : 'NodeData) (nk2 : 'NodeKey) (nd2 : 'NodeData) (ed : 'EdgeData) (g : AdjGraph<'NodeKey,'NodeData,'EdgeData>) : AdjGraph<'NodeKey,'NodeData,'EdgeData> =
        let mutable contextNk1 = (Unchecked.defaultof<'NodeData>,null)
        match g.TryGetValue(nk1,&contextNk1) with
        | true  ->
            if nk1 <> nk2 then
                let mutable contextNk2 = (Unchecked.defaultof<'NodeData>,null)
                match g.TryGetValue(nk2,&contextNk2) with            
                | true  ->
                    let nd1, s1 = contextNk1
                    //let mutable s1_ = Unchecked.defaultof<'EdgeData>
                    match s1.ContainsKey(nk2) with
                    | true  -> ()//failwithf "An Edge between Source Node %O Target Node %O does already exist" nk1 nk2
                    | false -> 
                        // Potentially update edge data
                        s1.Add(nk2,ed)                                        
                        let (nd2, s2) = contextNk2
                        s2.Add(nk1,ed)
                | false -> 
                    let nd1, s1 = contextNk1
                    s1.Add(nk2,ed)                                        
                    let s2 = Dictionary<_,_>()
                    s2.Add(nk1,ed)
                    g.Add(nk2,(nd2,s2))               
            else
                // inser self loop p
                let nd1, s1 = contextNk1
                match s1.ContainsKey(nk2) with
                | true -> ()
                | false ->
                    // Potentially update edge data
                    s1.Add(nk2,ed)
                
        | false -> 
            let mutable contextNk2 = (Unchecked.defaultof<'NodeData>,null)
            match g.TryGetValue(nk2,&contextNk2) with
            | true  ->   
                let s1 = Dictionary<_,_>()                
                s1.Add(nk2,ed)            
                g.Add(nk1,(nd1,s1))                
                let (nd2, s2) = contextNk2
                s2.Add(nk1,ed)                                            
            | false ->                 
                if nk1 <> nk2 then
                    let s1 = Dictionary<_,_>()                
                    s1.Add(nk2,ed)            
                    g.Add(nk1,(nd1,s1))     
                    
                    let s2 = Dictionary<_,_>()
                    s2.Add(nk1,ed)
                    g.Add(nk2,(nd2,s2))
                else
                    let s1 = Dictionary<_,_>()                
                    s1.Add(nk2,ed)  
                    g.Add(nk1,(nd1,s1))
        g    


    /// <summary> 
    /// Returns the Adjacency graph conetent as a sequence of edges 
    /// </summary>
    static member toSeq  (graph: AdjGraph<'NodeKey, 'NodeData, 'EdgeData>) = 
        // seq {
        //     for skv in graph do
        //         let source, adjComponent = skv.Value
        //         for tkv in adjComponent do
        //             let _,target = graph.[tkv.Key]                     
        //             yield (skv.Key,source,tkv.Key,target,tkv.Value)
        // }
        graph.Keys
        |> Seq.fold(fun foOuter node -> 
            let source = node
            let sourceData,_ = graph.Item source
            let targets = AdjGraph.getNeighbours source graph
            Seq.fold(fun fo (t,tv) ->
                // let t = t
                let td,_ = graph.Item t 
                // let tv = targetData.Value
                if Set.contains (source,sourceData,t,td,tv) fo || Set.contains (t,td,source,sourceData,tv) fo then
                    fo
                else
                    (Set.add(source,sourceData,t,td,tv) fo)
            ) foOuter targets
        ) Set.empty
        |> Set.toSeq

    
    /// <summary> 
    /// Creates an Adjacency graph of a sequence of edges
    /// </summary>
    static member ofSeq(edgelist : seq<'NodeKey * 'NodeData * 'NodeKey * 'NodeData * 'EdgeData>) 
        : AdjGraph<'NodeKey, 'NodeData, 'EdgeData> = 
        let graph = AdjGraph.create()//new AdjGraph<'NodeKey, 'Nodedata,'EdgeData>()
        edgelist
        |> Seq.iter (fun (sk,s ,tk,t,ed: 'EdgeData) -> AdjGraph.addElement sk s tk t ed graph |> ignore)
        graph

    
    /// <summary> 
    /// Converts Adjacency graph to its Adjacency matrix representation
    /// </summary>
    static member toAdjMatrix (graph: AdjGraph<'NodeKey, 'NodeData, 'EdgeData>)
        : AdjMatrix<'NodeKey, 'NodeData, 'EdgeData> =
        let nodeData     = Array.zeroCreate graph.Count
        let nodekeyIndex = Dictionary<'NodeKey, int>(graph.Count)
        let adjMatrix    = Array2D.zeroCreate graph.Count graph.Count
        let mutable index = 0
        for skv in graph do
            let source, _ = skv.Value
            nodeData.[index] <- source
            nodekeyIndex.Add(skv.Key, index)
            index <- index + 1
        index <- 0
        for skv in graph do
            let source, adjComponent = skv.Value
            for tkv in adjComponent do  
                adjMatrix.[index, nodekeyIndex.[tkv.Key]] <- tkv.Value
            index <- index + 1
        AdjMatrix(adjMatrix, nodeData, nodekeyIndex)


    ///Adds a labeled node to the graph.
    static member  addNode (nk:'NodeKey) (nd : 'NodeData)  (g : AdjGraph<'NodeKey, 'NodeData, 'EdgeData>) : AdjGraph<'NodeKey, 'NodeData, 'EdgeData> =
        let mutable context = (Unchecked.defaultof<'NodeData>,null)
        match g.TryGetValue(nk,&context) with
        | true  -> context <- (nd,Dictionary<_,_>())
        | false -> g.Add(nk,(nd,Dictionary<_,_>()))            
        g

    ///Adds labeled nodes to the graph.
    static member addNodes (nodeSeq:seq<(('NodeKey)*('NodeData))>) (g : AdjGraph<'NodeKey, 'NodeData, 'EdgeData>) : AdjGraph<'NodeKey, 'NodeData, 'EdgeData> =
        Seq.iter (fun (nk,nd) -> (AdjGraph.addNode nk nd g)|>ignore) nodeSeq |>ignore
        g

    ///Evaluates the number of nodes in the graph.
    static member countNodes (g : AdjGraph<'NodeKey, 'NodeData, 'EdgeData>) : int = 
        g.Count
 
    ///Returns true, if the node v is contained in the graph. Otherwise, it returns false.
    static member containsNode vk (g : AdjGraph<'NodeKey, 'NodeData, 'EdgeData>) : bool =
        Dictionary.containsKey vk g

    ///Lookup a labeled vertex in the graph. Raising KeyNotFoundException if no binding exists in the graph.
    static member findNode (n: 'NodeKey) (g : AdjGraph<'NodeKey, 'NodeData, 'EdgeData>) : ('NodeKey * 'NodeData) = 
        Dictionary.item n g
        |> fun (nd, _) -> n, nd

    ///Set the NodeData of a given NodeKey to the given NodeData
    static member setNodeData (n: 'NodeKey) (nd: 'NodeData) (g : AdjGraph<'NodeKey, 'NodeData, 'EdgeData>) : AdjGraph<'NodeKey, 'NodeData, 'EdgeData> = 
        let _,s = g.Item n
        g.Item n <- (nd,s)
        g

    ///Remove the Node and all edges connected to it
    static member removeNode (nk:'NodeKey) (g : AdjGraph<'NodeKey, 'NodeData, 'EdgeData>) : AdjGraph<'NodeKey, 'NodeData, 'EdgeData> = 
        match AdjGraph.containsNode nk g with
        |true   -> 
            let _,s = g.Item nk
            for k2 in s do
                g.Item k2.Key|> fun (nd,s) -> s.Remove nk |>ignore
            g.Remove nk |>ignore
            g 
        |_ -> g

    /// Applies the given function to each node of the graph
    static member iterNodes (action : 'NodeKey -> 'NodeData -> unit) (graph: AdjGraph<'NodeKey, 'NodeData, 'EdgeData>) =
        for kv in graph do
            let node, _ = kv.Value
            action kv.Key node
            

    /// Applies the given function to each node of the graph
    static member iteriNodes (action : int -> 'NodeKey -> 'NodeData -> unit) (graph: AdjGraph<'NodeKey, 'NodeData, 'EdgeData>) =
        let mutable counter = -1
        for kv in graph do
            let node, _ = kv.Value
            action (counter+1) kv.Key node


    /// Builds a graph whose elements are the results of applying the given function to each of the node.
    static member map (mapping : 'NodeKey -> 'NodeData -> ('NodeKey*'NodeData)) (graph: AdjGraph<'NodeKey, 'NodeData, 'EdgeData>) =
        let tmpGraph =  Dictionary<'NodeKey, 'NodeData * Dictionary<'NodeKey, 'EdgeData>>(graph.Count)
        for kv in graph do
            let node, adjComponent = kv.Value
            let tmpNodeKey, tmpNode = mapping kv.Key node
            tmpGraph.Add(tmpNodeKey, (tmpNode,adjComponent))
        tmpGraph

    ///Returns all the nodes as a seq of 'NodeKey * 'NodeData Tuple
    static member getNodes (g : AdjGraph<'NodeKey, 'NodeData, 'EdgeData>) : seq<'NodeKey* 'NodeData> = 
        g
        |> Seq.map(fun kvp ->
            let (nd,s) = kvp.Value
            kvp.Key,nd
        )

    /// Converts nodes to nodeKey * nodeData array 
    static member toNodeArray (graph: AdjGraph<'NodeKey, 'NodeData, 'EdgeData>) =
        let tmp = Array.zeroCreate graph.Count 
        AdjGraph.iteriNodes (fun i key node -> tmp.[i] <- key,node) graph
        tmp


    /// Counts all edges 
    static member countEdges (graph: AdjGraph<'NodeKey, 'NodeData, 'EdgeData>) =
        graph
        |> Seq.sumBy (fun kv -> 
            let _, adjComponent = kv.Value
            adjComponent.Count
            )
        

    ///Returns true, if the edge from vertex v1 to vertex v2 is contained in the graph. Otherwise, it returns false.
    static member containsEdge v1 v2 (g: AdjGraph<'NodeKey,'NodeData,'EdgeData>) : bool =
        let mutable context = (Unchecked.defaultof<'NodeData>,null)
        match g.TryGetValue(v1,&context) with
        | true  -> 
            let (_, s) = context
            s.ContainsKey(v2)
        | false -> false
            
    
    /// Add edge
    static member addEdge (sourceKey : 'NodeKey) (targetKey : 'NodeKey) (data : 'EdgeData) (graph: AdjGraph<'NodeKey, 'NodeData, 'EdgeData>) =                
        if not <| graph.ContainsKey targetKey then
            failwithf "The target node %O of the edge does not exist in this graph." targetKey        

        match graph.ContainsKey(sourceKey) with
        | true  -> 
            let source,adjComponent = graph.[sourceKey]
            Dictionary.addOrUpdateInPlace targetKey data adjComponent |> ignore
            Dictionary.addOrUpdateInPlace sourceKey (source,adjComponent) graph |> ignore
        | false -> 
            failwithf "The source node %O of the edge does not exist in this graph." sourceKey
        
        graph

    ///Adds a labeled edge to the graph.
    static member tryAddEdge (nk1 : 'NodeKey) (nk2 : 'NodeKey) (ed : 'EdgeData) (g : AdjGraph<'NodeKey,'NodeData,'EdgeData>) : AdjGraph<'NodeKey,'NodeData,'EdgeData> option =
        if (AdjGraph.containsNode nk1 g |> not) || (AdjGraph.containsNode nk2 g |> not) || AdjGraph.containsEdge nk1 nk2 g then
            None
        else 
            let (nd1, s1) = Dictionary.item nk1 g
            Dictionary.addOrUpdateInPlace nk2 ed s1 |> ignore
            let (nd2, s2) = Dictionary.item nk2 g
            Dictionary.addOrUpdateInPlace nk1 ed s2 |> ignore
            g |> Some

    ///Add labeled edges to the graph.
    static member addEdges (edgeSeq:seq<('NodeKey)*('NodeKey)*('EdgeData)>) (g : AdjGraph<'NodeKey,'NodeData,'EdgeData>) : AdjGraph<'NodeKey,'NodeData,'EdgeData> =
        Seq.iter (fun (nk1,nk2,ed) -> AdjGraph.addEdge nk1 nk2 ed g|>ignore) edgeSeq|>ignore
        g

    ///Remove an edge
    static member removeEdge (nkSource : 'NodeKey) (nkTarget : 'NodeKey) (g : AdjGraph<'NodeKey,'NodeData,'EdgeData>) : AdjGraph<'NodeKey,'NodeData,'EdgeData> =
        match AdjGraph.containsEdge nkSource nkTarget g with
        | true    -> 
            g.Item nkSource|> fun (nd,s) -> s.Remove nkTarget |>ignore
            g.Item nkTarget|> fun (nd,s) -> s.Remove nkSource |>ignore
            g
        | _         -> g 


    ///Removes all edges according to the given removeF
    static member removeMany (edgeSeq:seq<('NodeKey)*('NodeKey)>) (removeF: 'NodeKey->'NodeKey->AdjGraph<'NodeKey,'NodeData,'EdgeData> -> AdjGraph<'NodeKey,'NodeData,'EdgeData>) (g : AdjGraph<'NodeKey,'NodeData,'EdgeData>) : AdjGraph<'NodeKey,'NodeData,'EdgeData> =
        Seq.iter(fun (nk1,nk2) -> (removeF nk1 nk2 g )|>ignore) edgeSeq|>ignore
        g

    /// Applies the given function to each node of the graph
    static member iterEdge (action : 'NodeKey -> 'NodeKey -> 'EdgeData -> unit) (graph: AdjGraph<'NodeKey, 'NodeData, 'EdgeData>) =
        for skv in graph do
            let source, adjComponent = skv.Value
            for tkv in adjComponent do  
                action skv.Key tkv.Key tkv.Value
    
    /// <summary> 
    /// Tries to find an edge between the specified nodes. Raises Exception if no such edge exists in the graph.
    /// </summary>
    static member findEdge (sourceKey : 'NodeKey) (targetKey : 'NodeKey) (graph: AdjGraph<'NodeKey, 'NodeData, 'EdgeData>) =
        match graph.ContainsKey(sourceKey) with
        | true  -> 
            let source,adjComponent = graph.[sourceKey]
            match adjComponent.ContainsKey(targetKey) with
            | true -> sourceKey,targetKey,adjComponent.[targetKey]
            | false -> failwithf "Edge %O - %O does not exist in this graph." sourceKey targetKey 
        | false -> 
            failwithf "The source node %O of the edge does not exist in this graph." sourceKey

    ///Lookup a labeled edge in the graph, returning a Some value if a binding exists and None if not.
    static member tryFindEdge (nk1 : 'NodeKey) (nk2 : 'NodeKey) (g : AdjGraph<'NodeKey,'NodeData,'EdgeData>) : ('NodeKey * 'NodeKey * 'EdgeData) option =
        let mutable context = (Unchecked.defaultof<'NodeData>,null)
        match g.TryGetValue(nk1,&context) with
        | false -> None
        | true  -> 
            let (_, s) = context
            match s.TryGetValue(nk1) with
            | false,_  -> None
            | true, e  ->                     
                Some (nk1,nk2,e)


    ///Returns the neighours of a node nk with their EdgeData
    static member getNeighbours (nk:'NodeKey) (g : AdjGraph<'NodeKey, 'NodeData, 'EdgeData>) : seq<'NodeKey*'EdgeData> =
        let nd,s = g.Item nk    
        seq{
            for kvp in s do
                kvp.Key,
                kvp.Value
        }

    ///Returns the degree of a node nk 
    static member getDegree (g : AdjGraph<'NodeKey, 'NodeData, 'EdgeData>) (nk:'NodeKey) :int = 
        let nd,s = g.Item nk
        s|>Seq.length

    ///Maps contexts of the graph.
    static member mapContexts (mapping : 'NodeData*Dictionary<'NodeKey,'EdgeData> -> 'T) (g : AdjGraph<'NodeKey, 'NodeData, 'EdgeData>) : seq<'NodeKey * 'T> =
        g
        |> Seq.map (fun kv -> kv.Key,mapping kv.Value )

    /// <summary> 
    /// Converts the FGraph to an array2d 
    /// </summary>
    /// <param name="graph">The graph to be converted</param> 
    /// <returns>An array2d</returns>
    static member toArray2D (nodeIndexer : 'NodeKey -> int)  =
        (fun (g : AdjGraph<'NodeKey,'NodeData,'EdgeData>) ->
            let n = g.Count
            let matrix = Array2D.zeroCreate n n
            for skv in g do
                let (_, s) = skv.Value
                for tkv in s do  
                    matrix.[nodeIndexer skv.Key,nodeIndexer tkv.Key] <- tkv.Value
            
            matrix
            )

    /// <summary> 
    /// Returns the FGraph edges as a sequence of edges 
    /// </summary>
    static member toEdgeSeq (graph: AdjGraph<'NodeKey, 'NodeData, 'EdgeData>) = 
        seq {
             for skv in graph do
                let (source, s) = skv.Value
                for tkv in s do                      
                    let sk = skv.Key
                    
                    let tk,tv = tkv.Key,tkv.Value
                    if sk<=tk then
                        yield (sk,tk,tv)
                    else
                        yield (tk,sk,tv)
                    //yield (skv.Key,tkv.Key,tkv.Value)
        }
        |> Seq.distinct
    

    ///Returns the overlapping nodes of two graphs
    static member getNodeOverlap (graph1: AdjGraph<'NodeKey, 'NodeData, 'EdgeData>) (graph2: AdjGraph<'NodeKey, 'NodeData, 'EdgeData>)  = 
        let nodeKeySet1 = graph1.Keys|>Set.ofSeq 
        let nodeKeySet2 = graph2.Keys|>Set.ofSeq 
        
        Set.intersect nodeKeySet1 nodeKeySet2 

    ///Returns the overlapping edges of two graphs
    static member getEdgeOverlap (graph1: AdjGraph<'NodeKey, 'NodeData, 'EdgeData>) (graph2: AdjGraph<'NodeKey, 'NodeData, 'EdgeData>)  = 
        let edgeSet1 = graph1|>AdjGraph.toEdgeSeq|> Seq.map(fun (s,t,w) -> seq{s,t;t,s})|>Seq.concat|>Set.ofSeq
        let edgeSet2 = graph2|>AdjGraph.toEdgeSeq|> Seq.map(fun (s,t,w) -> seq{s,t})    |>Seq.concat|>Set.ofSeq
        
        Set.intersect edgeSet1 edgeSet2

    ///Returns the amount of overlapping nodes of two graphs
    static member getNodeOverlapCount (graph1: AdjGraph<'NodeKey, 'NodeData, 'EdgeData>) (graph2: AdjGraph<'NodeKey, 'NodeData, 'EdgeData>) :int = 
        AdjGraph.getNodeOverlap graph1 graph2
        |>Set.count

    ///Returns the amount of overlapping edges of two graphs
    static member getEdgeOverlapCount (graph1: AdjGraph<'NodeKey, 'NodeData, 'EdgeData>) (graph2: AdjGraph<'NodeKey, 'NodeData, 'EdgeData>) :int  = 
        AdjGraph.getNodeOverlap graph1 graph2
        |>Set.count



    static member getSubGraphOfNodeSeq (graph:AdjGraph<'NodeKey,'NodeData,'EdgeData>) (nodeSeq:seq<'NodeKey>) =
        let set = nodeSeq|> Set.ofSeq
        graph
        |> AdjGraph.toSeq
        |> Seq.choose(fun (nk1,nd1,nk2,nd2,w) ->
            if set.Contains nk1 && set.Contains nk2 then
                Some (nk1,nd1,nk2,nd2,w)
            else
                None
        ) 
        |> AdjGraph.ofSeq

    static member getNeighourhoodSubgraphByHops (startingNode:'NodeKey) (hopCount:int) (graph:AdjGraph<'NodeKey,'NodeData,'EdgeData>) =

        let rec loop (visited:Set<'NodeKey>) (toVisit:Set<'NodeKey>) (hops:int) =
            if hops = hopCount then
                visited
            else
                let newVisited,newToVisit = 
                    toVisit
                    |>Seq.fold(fun ((v:Set<'NodeKey>),(tV:Set<'NodeKey>)) x -> 
                        let nd,neighbours = graph.Item x
                        v.Add x,
                        neighbours
                        |>Seq.fold(fun a x -> a.Add(x.Key)) tV

                    ) (visited,Set.empty) 
                    
                loop newVisited newToVisit (hops+1)

        let subNodes = loop ((Set.empty).Add startingNode) (AdjGraph.getNeighbours startingNode graph|>Seq.map fst|>Set) 0
        AdjGraph.getSubGraphOfNodeSeq graph subNodes


module AdjGraph =
    
    /// <summary> 
    /// Creates an empty Adjacency Graph
    /// </summary>
    /// <returns>Empty AdjGraph</returns>
    let empty<'NodeKey, 'NodeData, 'EdgeData when 'NodeKey : comparison>
        : AdjGraph<'NodeKey, 'NodeData, 'EdgeData> =
        AdjGraph<'NodeKey, 'NodeData, 'EdgeData>()

    /// <summary> 
    /// Functions operating on nodes
    /// </summary>
    type Node() =

        /// Counts all nodes 
        static member count (graph: AdjGraph<'NodeKey, 'NodeData, 'EdgeData>) =
            graph.Count

        /// Adds node to graph [if node exists it is updated]
        static member addNode (graph: AdjGraph<'NodeKey, 'NodeData, 'EdgeData>) (key:'NodeKey) (data:'NodeData) :AdjGraph<'NodeKey, 'NodeData, 'EdgeData> =
            AdjGraph.addNode key data graph

        static member removeNode (g : AdjGraph<'NodeKey, 'NodeData, 'EdgeData>) (nk:'NodeKey)  : AdjGraph<'NodeKey, 'NodeData, 'EdgeData> =     
            AdjGraph.removeNode nk g
        /// 
        static member containsKey (key:'NodeKey) (graph: AdjGraph<'NodeKey, 'NodeData, 'EdgeData>) =
            graph.ContainsKey key

        /// Applies the given function to each node of the graph
        static member iter (action : 'NodeKey -> 'NodeData -> unit) (graph: AdjGraph<'NodeKey, 'NodeData, 'EdgeData>) =
           AdjGraph.iterNodes action graph
                

         /// Applies the given function to each node of the graph
        static member iteri (action : int -> 'NodeKey -> 'NodeData -> unit) (graph: AdjGraph<'NodeKey, 'NodeData, 'EdgeData>) =
           AdjGraph.iteriNodes action graph


        /// Builds a graph whose elements are the results of applying the given function to each of the node.
        static member map (mapping : 'NodeKey -> 'NodeData -> ('NodeKey*'NodeData)) (graph: AdjGraph<'NodeKey, 'NodeData, 'EdgeData>) =
            AdjGraph.map mapping graph

        /// Converts nodes to nodeKey * nodeData array 
        static member toArray (graph: AdjGraph<'NodeKey, 'NodeData, 'EdgeData>) =
            AdjGraph.toNodeArray

    /// <summary> 
    /// Functions operating on directed edges
    /// </summary>
    type Edge() =
        
        /// Counts all edges 
        static member count (graph: AdjGraph<'NodeKey, 'NodeData, 'EdgeData>) =
            AdjGraph.countEdges graph


        /// Add edge
        static member add (sourceKey : 'NodeKey) (targetKey : 'NodeKey) (data : 'EdgeData) (graph: AdjGraph<'NodeKey, 'NodeData, 'EdgeData>) =                
            AdjGraph.addEdge sourceKey targetKey data graph

        /// Applies the given function to each node of the graph
        static member iter (action : 'NodeKey -> 'NodeKey -> 'EdgeData -> unit) (graph: AdjGraph<'NodeKey, 'NodeData, 'EdgeData>) =
            AdjGraph.iterEdge action graph

        /// <summary> 
        /// Tries to find an edge between the specified nodes. Raises Exception if no such edge exists in the graph.
        /// </summary>
        static member find (sourceKey : 'NodeKey) (targetKey : 'NodeKey) (graph: AdjGraph<'NodeKey, 'NodeData, 'EdgeData>) =
           AdjGraph.findEdge sourceKey targetKey graph
    
        //static member map (graph: AdjGraph<'NodeKey, 'NodeData, 'EdgeData>) =
            

