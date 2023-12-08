namespace Graphoscope.Graphs.Undirected

open FSharpAux
open System.Collections.Generic
open System.Linq

type Adj<'NodeKey, 'EdgeData> = seq<'NodeKey * 'EdgeData>

type FContext<'NodeKey, 'NodeData, 'EdgeData> when 'NodeKey: comparison =
    Dictionary<'NodeKey,'EdgeData> * 'NodeData * Dictionary<'NodeKey,'EdgeData>

type FContextMapU<'NodeKey,'NodeData,'EdgeData> when 'NodeKey: comparison =
    Dictionary<'NodeKey, FContext<'NodeKey, 'NodeData, 'EdgeData>>

//All edge information is stored strictly in successors
module FContext =

    ///Lists the vertices which have edges pointing to/from the vertex.
    let neighbours (context:FContext<'NodeKey, 'NodeData, 'EdgeData>) : Adj<'NodeKey, 'EdgeData> = 
        let (_, _, s) = context
        seq {
            for kv in s do
                yield kv.Key,kv.Value 
        }
        

    // //Properties

    ///Evaluates the number of edges associated with the vertex.
    let degree (context:FContext<'NodeKey, 'NodeData, 'EdgeData>) : int =
        let (_, _, s) = context
        s.Count
    
    ///Evaluates the number of edges pointing to the vertex.
    let inwardDegree (context:FContext<'NodeKey, 'NodeData, 'EdgeData>) : int=
        degree context

    ///Evaluates the number of edges pointing away from the vertex.
    let outwardDegree (context:FContext<'NodeKey, 'NodeData, 'EdgeData>) : int = 
        degree context
    
    /// Clones context 
    let clone (context:FContext<'NodeKey, 'NodeData, 'EdgeData>) : FContext<'NodeKey, 'NodeData, 'EdgeData> =
        let d1, data, d2 = context
        let d1Cloned = new Dictionary<'NodeKey,'EdgeData>(d1)
        let d2Cloned = new Dictionary<'NodeKey,'EdgeData>(d2)
        (d1Cloned, data, d2Cloned)

type FContextMapU() = 

    /// <summary> 
    /// Creates a new graph with the given Data
    /// </summary>
    /// <returns>FContextMapU</returns>
    static member create<'NodeKey, 'NodeData, 'EdgeData when 'NodeKey: comparison>() : FContextMapU<'NodeKey, 'NodeData, 'EdgeData> =
        Dictionary<_,_>()

    /// <summary> 
    /// Clones an existing graph
    /// 
    /// Note: Current implementation subject to change 
    /// 
    /// [see here for further info](https://github.com/fslaborg/Graphoscope/issues/52#issuecomment-1741746696)
    /// </summary>
    /// <returns>FContextMapU</returns>
    static member clone (graph:FContextMapU<'NodeKey, 'NodeData, 'EdgeData>) : FContextMapU<'NodeKey, 'NodeData, 'EdgeData> =
        graph.ToDictionary( (fun f -> f.Key), 
                            (fun f -> FContext.clone f.Value))
    
    /// <summary> 
    /// Adds a labeled, directed edge to the graph.
    /// </summary>
    /// <returns>FContextMapU with new element</returns>
    static member addElement (nk1 : 'NodeKey) (nd1 : 'NodeData) (nk2 : 'NodeKey) (nd2 : 'NodeData) (ed : 'EdgeData) (g : FContextMapU<'NodeKey,'NodeData,'EdgeData>) : FContextMapU<'NodeKey,'NodeData,'EdgeData> =
        let mutable contextNk1 = (null,Unchecked.defaultof<'NodeData>,null)
        match g.TryGetValue(nk1,&contextNk1) with
        | true  ->
            if nk1 <> nk2 then
                let mutable contextNk2 = (null,Unchecked.defaultof<'NodeData>,null)
                match g.TryGetValue(nk2,&contextNk2) with            
                | true  ->
                    let p1, nd1, s1 = contextNk1
                    //let mutable s1_ = Unchecked.defaultof<'EdgeData>
                    match s1.ContainsKey(nk2) with
                    | true  -> ()//failwithf "An Edge between Source Node %O Target Node %O does already exist" nk1 nk2
                    | false -> 
                        // Potentially update edge data
                        s1.Add(nk2,ed)                                        
                        let (p2, nd2, s2) = contextNk2
                        s2.Add(nk1,ed)
                | false -> 
                    let p1, nd1, s1 = contextNk1
                    s1.Add(nk2,ed)                                        
                    let s2 = Dictionary<_,_>()
                    s2.Add(nk1,ed)
                    g.Add(nk2,(Dictionary<_,_>(),nd2,s2))               
            else
                // insert self loop p
                let p1, nd1, s1 = contextNk1
                match s1.ContainsKey(nk2) with
                | true -> ()
                | false ->
                    // Potentially update edge data
                    s1.Add(nk2,ed)                
                
        | false -> 
            let mutable contextNk2 = (null,Unchecked.defaultof<'NodeData>,null)
            match g.TryGetValue(nk2,&contextNk2) with
            | true  ->   
                let s1 = Dictionary<_,_>()                
                s1.Add(nk2,ed)            
                g.Add(nk1,(Dictionary<_,_>(),nd1,s1))                
                let (p2, nd2, s2) = contextNk2
                s2.Add(nk1,ed)                                                
            | false ->                 
                if nk1 <> nk2 then
                    let s1 = Dictionary<_,_>()                
                    s1.Add(nk2,ed)            
                    g.Add(nk1,(Dictionary<_,_>(),nd1,s1))     
                    
                    let s2 = Dictionary<_,_>()
                    s2.Add(nk1,ed)
                    g.Add(nk2,(Dictionary<_,_>(),nd2,s2))
                else
                    let s1 = Dictionary<_,_>()                
                    s1.Add(nk2,ed)        
                    g.Add(nk1,(Dictionary<_,_>(),nd1,s1))                       
        g    


    ///Adds a labeled node to the graph.
    static member  addNode (nk:'NodeKey) (nd : 'NodeData)  (g : FContextMapU<'NodeKey, 'NodeData, 'EdgeData>) : FContextMapU<'NodeKey, 'NodeData, 'EdgeData> =
        let mutable context = (null,Unchecked.defaultof<'NodeData>,null)
        match g.TryGetValue(nk,&context) with
        | true  -> context <- (Dictionary<_,_>(),nd,Dictionary<_,_>())
        | false -> g.Add(nk,(Dictionary<_,_>(),nd,Dictionary<_,_>()))            
        g

    ///Adds labeled nodes to the graph.
    static member addNodes (nodeSeq:seq<(('NodeKey)*('NodeData))>) (g : FContextMapU<'NodeKey, 'NodeData, 'EdgeData>) : FContextMapU<'NodeKey, 'NodeData, 'EdgeData> =
        Seq.iter (fun (nk,nd) -> (FContextMapU.addNode nk nd g)|>ignore) nodeSeq |>ignore
        g

    ///Evaluates the number of nodes in the graph.
    static member countNodes (g : FContextMapU<'NodeKey, 'NodeData, 'EdgeData>) : int = 
        g.Count
 
    ///Returns true, if the node v is contained in the graph. Otherwise, it returns false.
    static member containsNode vk (g : FContextMapU<'NodeKey, 'NodeData, 'EdgeData>) : bool =
        Dictionary.containsKey vk g

    ///Lookup a labeled vertex in the graph. Raising KeyNotFoundException if no binding exists in the graph.
    static member findNode (n: 'NodeKey) (g : FContextMapU<'NodeKey, 'NodeData, 'EdgeData>) : ('NodeKey * 'NodeData) = 
        Dictionary.item n g
        |> fun (_, nd, _) -> n, nd

    ///Set the NodeData of a given NodeKey to the given NodeData
    static member setNodeData (n: 'NodeKey) (nd: 'NodeData) (g : FContextMapU<'NodeKey, 'NodeData, 'EdgeData>) : FContextMapU<'NodeKey, 'NodeData, 'EdgeData> = 
        let p,_,s = g.Item n
        g.Item n <- (p,nd,s)
        g

    ///Returns all the nodes as a seq of 'NodeKey * 'NodeData Tuple
    static member getNodes (g : FContextMapU<'NodeKey, 'NodeData, 'EdgeData>) : seq<'NodeKey* 'NodeData> = 
        g
        |> Seq.map(fun kvp ->
            let (p,nd,s) = kvp.Value
            kvp.Key,nd
        )



    ///Maps contexts of the graph.
    static member mapContexts (mapping : FContext<'NodeKey, 'NodeData, 'EdgeData> -> 'T) (g : FContextMapU<'NodeKey,'NodeData,'EdgeData>) : seq<'NodeKey * 'T>= 
        g
        |> Seq.map (fun kv -> kv.Key,mapping kv.Value )

    /////Remove the Node and all edges connected to it
    static member removeNode (nk:'NodeKey) (g : FContextMapU<'NodeKey, 'NodeData, 'EdgeData>) : FContextMapU<'NodeKey, 'NodeData, 'EdgeData> = 
        match FContextMapU.containsNode nk g with
        |true   -> 
            let p,_,s = g.Item nk
            for k2 in s do
                g.Item k2.Key|> fun (p,nd,s) -> p.Remove nk |>ignore
            g.Remove nk |>ignore
            g 
        |_ -> g

     /// <summary> 
     /// Returns the FContextMapU content as a sequence of edges 
     /// </summary>
    static member toSeq (graph: FContextMapU<'NodeKey, 'NodeData, 'EdgeData>) = 
        seq {
             for skv in graph do
                let (_, source, s) = skv.Value
                for tkv in s do                      
                    let (_, target, _) = graph.[tkv.Key]
                    if skv.Key >=tkv.Key then
                        yield (skv.Key,source,tkv.Key,target,tkv.Value)
                    else
                         yield (tkv.Key,target,skv.Key,source,tkv.Value)
        }
        |> Seq.distinct   

    /// <summary> 
    /// Creates an Adjacency graph of a sequence of edges
    /// </summary>
    static member ofSeq (edgelist : seq<'NodeKey * 'NodeData * 'NodeKey * 'NodeData * 'EdgeData>) : FContextMapU<'NodeKey, 'NodeData, 'EdgeData> = 
        let graph = FContextMapU.create()
        edgelist
        |> Seq.iter (fun (sk,s,tk,t,ed) -> FContextMapU.addElement sk s tk t ed graph |> ignore)
        graph

    /// <summary> 
    /// Converts the FContextMapU to an array2d 
    /// </summary>
    /// <param name="graph">The graph to be converted</param> 
    /// <returns>An array2d</returns>
    static member toArray2D (nodeIndexer : 'NodeKey -> int)  =
        (fun (g : FContextMapU<'NodeKey,'NodeData,'EdgeData>) ->
            let n = g.Count
            let matrix = Array2D.zeroCreate n n
            for skv in g do
                let (_, _, s) = skv.Value
                for tkv in s do  
                    matrix.[nodeIndexer skv.Key,nodeIndexer tkv.Key] <- tkv.Value
            
            matrix
            )

    ///Evaluates the number of edges in the graph.
    static member countEdges (g: FContextMapU<'NodeKey,'NodeData,'EdgeData>) : int =
        g 
        |> Seq.sumBy (fun kv -> 
            let (_,_,s) = kv.Value
            ((float s.Count) / 2.)
            )
        |> int

    ///Returns true, if the edge from vertex v1 to vertex v2 is contained in the graph. Otherwise, it returns false.
    static member containsEdge v1 v2 (g: FContextMapU<'NodeKey,'NodeData,'EdgeData>) : bool =
        let mutable context = (null,Unchecked.defaultof<'NodeData>,null)
        match g.TryGetValue(v1,&context) with
        | true  -> 
            let (_, _, s) = context
            s.ContainsKey(v2)
        | false -> false
            
    
    ///Lookup a labeled edge in the graph. Raising KeyNotFoundException if no binding exists in the graph.
    static member findEdge (v1:'NodeKey) (v2:'NodeKey) (g : FContextMapU<'NodeKey,'NodeData,'EdgeData>) : 'NodeKey * 'NodeKey * 'EdgeData =
            Dictionary.item v1 g
            |> fun (_, _, s) -> Dictionary.item v2 s
            |> fun e -> (v1,v2,e)
    
    ///Lookup a labeled edge in the graph, returning a Some value if a binding exists and None if not.
    static member tryFindEdge (nk1 : 'NodeKey) (nk2 : 'NodeKey) (g : FContextMapU<'NodeKey,'NodeData,'EdgeData>) : ('NodeKey * 'NodeKey * 'EdgeData) option =
        let mutable context = (null,Unchecked.defaultof<'NodeData>,null)
        match g.TryGetValue(nk1,&context) with
        | false -> None
        | true  -> 
            let (_, _, s) = context
            match s.TryGetValue(nk1) with
            | false,_  -> None
            | true, e  ->                     
                Some (nk1,nk2,e)


    //Add and remove

    ///Adds a labeled, directed edge to the graph.
    static member tryAddEdge (nk1 : 'NodeKey) (nk2 : 'NodeKey) (ed : 'EdgeData) (g : FContextMapU<'NodeKey,'NodeData,'EdgeData>) : FContextMapU<'NodeKey,'NodeData,'EdgeData> option =
        if (FContextMapU.containsNode nk1 g |> not) || (FContextMapU.containsNode nk2 g |> not) || FContextMapU.containsEdge nk1 nk2 g then
            None
        else 
            let (p1, nd1, s1) = Dictionary.item nk1 g
            Dictionary.addOrUpdateInPlace nk2 ed s1 |> ignore
            let (p2, nd2, s2) = Dictionary.item nk2 g
            Dictionary.addOrUpdateInPlace nk1 ed s2 |> ignore
            g |> Some

    ///Adds a labeled, directed edge to the graph.
    static member addEdge (nk1 : 'NodeKey) (nk2 : 'NodeKey) (ed : 'EdgeData) (g : FContextMapU<'NodeKey,'NodeData,'EdgeData>) : FContextMapU<'NodeKey,'NodeData,'EdgeData> =
        let mutable contextNk1 = (null,Unchecked.defaultof<'NodeData>,null)
        match g.TryGetValue(nk1,&contextNk1) with
        | false -> () //failwithf "Source Node %O does not exist" nk1 
        | true  ->
            if nk1 <> nk2 then
                let mutable contextNk2 = (null,Unchecked.defaultof<'NodeData>,null)
                match g.TryGetValue(nk2,&contextNk2) with
                | false -> failwithf "Target Node %O does not exist" nk2
                | true  ->   
                    let p1, nd1, s1 = contextNk1
                    //let mutable s1_ = Unchecked.defaultof<'EdgeData>
                    match s1.ContainsKey(nk2) with                    
                    | true  -> failwithf "An Edge between Source Node %O Target Node %O does already exist" nk1 nk2
                    | false -> 
                        s1.Add(nk2,ed)                                        
                        let (p2, nd2, s2) = contextNk2
                        s2.Add(nk1,ed)
            else
                let p1, nd1, s1 = contextNk1
                match s1.ContainsKey(nk1) with 
                | true -> failwithf "An Edge between Source Node %O Target Node %O does already exist" nk1 nk2
                | false ->
                    s1.Add(nk1,ed)                                    
        g

    ///Add labeled, directed edges to the graph.
    static member addEdges (edgeSeq:seq<('NodeKey)*('NodeKey)*('EdgeData)>) (g : FContextMapU<'NodeKey,'NodeData,'EdgeData>) : FContextMapU<'NodeKey,'NodeData,'EdgeData> =
        Seq.iter (fun (nk1,nk2,ed) -> FContextMapU.addEdge nk1 nk2 ed g|>ignore) edgeSeq|>ignore
        g

    ///Remove a directed edge
    static member removeEdge (nkSource : 'NodeKey) (nkTarget : 'NodeKey) (g : FContextMapU<'NodeKey,'NodeData,'EdgeData>) : FContextMapU<'NodeKey,'NodeData,'EdgeData> =
        match FContextMapU.containsEdge nkSource nkTarget g with
        | true    -> 
            g.Item nkSource|> fun (p,nd,s) -> s.Remove nkTarget |>ignore
            g.Item nkTarget|> fun (p,nd,s) -> s.Remove nkSource |>ignore
            g
        | _         -> g 


    ///Removes all edges according to the given removeF
    static member removeMany (edgeSeq:seq<('NodeKey)*('NodeKey)>) (removeF: 'NodeKey->'NodeKey->FContextMapU<'NodeKey,'NodeData,'EdgeData> -> FContextMapU<'NodeKey,'NodeData,'EdgeData>) (g : FContextMapU<'NodeKey,'NodeData,'EdgeData>) : FContextMapU<'NodeKey,'NodeData,'EdgeData> =
        Seq.iter(fun (nk1,nk2) -> (removeF nk1 nk2 g )|>ignore) edgeSeq|>ignore
        g

    // /// Applies the given function on each egdge of the graph
    // static member iterEdges (action : 'NodeKey -> 'NodeKey -> 'EdgeData -> unit) (graph: FContextMapU<'NodeKey, 'NodeData, 'EdgeData>) =
    //     for skv in graph do
    //         let (_, _, s) = skv.Value
    //         for tkv in s do  
    //             action skv.Key tkv.Key tkv.Value

    // /// Maps the given function on each egdge of the graph
    // static member mapEdges (action : 'NodeKey -> 'NodeKey -> 'EdgeData -> 'U) (graph: FContextMapU<'NodeKey, 'NodeData, 'EdgeData>) =
    //     graph
    //     |>Seq.map(fun skv ->
    //         skv.Value
    //         |> fun (_,_,s) -> s
    //         |> Seq.map(fun tkv ->
    //             action skv.Key tkv.Key tkv.Value
    //         )
    //     )
    //     |> Seq.concat

    // /// Applies the given function on every edge of the graph, which also receives an ascending integer index.
    // static member iteriEdges (action : int -> 'NodeKey -> 'NodeKey -> 'EdgeData -> unit) (graph: FContextMapU<'NodeKey, 'NodeData, 'EdgeData>) =
    //     let mutable index = -1
    //     for skv in graph do
    //         let (_, _, s) = skv.Value
    //         for tkv in s do  
    //             index <- index + 1
    //             action index skv.Key tkv.Key tkv.Value

    // /// Maps the given function on each egdge of the graph
    // static member mapiEdges (action : int -> 'NodeKey -> 'NodeKey -> 'EdgeData -> 'U) (graph: FContextMapU<'NodeKey, 'NodeData, 'EdgeData>) =
    //     let mutable index = -1
    //     graph
    //     |>Seq.map(fun skv ->
    //         skv.Value
    //         |> fun (_,_,s) -> s
    //         |> Seq.map(fun tkv ->
    //             index <- index + 1
    //             action index skv.Key tkv.Key tkv.Value
    //         )
    //     )
    //     |> Seq.concat

    ///Set the EdgeData of a given Edge between nk1 and nk2 to the given EdgeData
    static member setEdgeData (nk1: 'NodeKey) (nk2: 'NodeKey)(ed: 'EdgeData) (g : FContextMapU<'NodeKey, 'NodeData, 'EdgeData>) : FContextMapU<'NodeKey, 'NodeData, 'EdgeData> = 
        let mutable contextNk1 = (null,Unchecked.defaultof<'NodeData>,null)
        match g.TryGetValue(nk1,&contextNk1) with
        | false -> failwithf "Source Node %O does not exist" nk1 
        | true  ->
            if nk1 <> nk2 then
                let mutable contextNk2 = (null,Unchecked.defaultof<'NodeData>,null)
                match g.TryGetValue(nk2,&contextNk2) with
                | false -> failwithf "Edge to Target Node %O does not exist" nk2
                | true  ->   
                    let p1, nd1, s1 = contextNk1
                    //let mutable s1_ = Unchecked.defaultof<'EdgeData>
                    match s1.ContainsKey(nk2) with                    
                    | true  -> 
                        s1.Item nk2 <- (ed)
                        let (p2, nd2, s2) = contextNk2
                        s2.Item nk1 <- (ed)

                    | false -> 
                        failwithf "An Edge between Source Node %O Target Node %O does not exist" nk1 nk2

            else
                let p1, nd1, s1 = contextNk1
                match s1.ContainsKey(nk1) with 
                | true -> 
                    s1.Item nk2 <- (ed)
                | false ->
                    failwithf "An Edge between Source Node %O Target Node %O does not exist" nk1 nk2
                    
        g


    /// <summary> 
    /// Creates a new graph with the given node Data
    /// </summary>
    /// <returns>FContextMapU</returns>
    static member createFromNodes (nodes:seq<(('NodeKey)*('NodeData))>) :FContextMapU<'NodeKey,'NodeData,'EdgeData> =
        FContextMapU.create()
        |> FContextMapU.addNodes nodes

    // static member createOfEdgeSeq (edges:seq<('NodeKey)*('NodeKey)*('EdgeData)>) :FContextMapU<'NodeKey,'NodeKey,'EdgeData> =
    //     let nodes: seq<'NodeKey * 'NodeKey> = 
    //         edges
    //         |> Seq.map(fun (s,t,w) -> [s,s;t,t])
    //         |> Seq.concat
    //         |> Seq.distinct
    //     FContextMapU.create()
    //     |> FContextMapU.addNodes nodes
    //     |> FContextMapU.addEdges edges

    /// <summary> 
    /// Creates a new graph with the given Data
    /// </summary>
    /// <returns>FContextMapU</returns>
    static member create ((nodes:seq<(('NodeKey)*('NodeData))>),(edges:seq<('NodeKey)*('NodeKey)*('EdgeData)>)) :FContextMapU<'NodeKey,'NodeData,'EdgeData> =
        FContextMapU.createFromNodes nodes
        |> FContextMapU.addEdges edges
        
        // ///Maps edgeData of the graph.
        // static member map 
        // /// Returns a new graph containing only the edges for which the given predicate returns true.
        // static member filter (predicate)
        // ///Removes an edge from the graph.
        // static member remove 
    
    ///Returns if the given graph features loops
    static member hasLoops (graph: FContextMapU<'NodeKey, 'NodeData, 'EdgeData>) =
        graph
        |> Seq.countIf (fun (kv: KeyValuePair<'NodeKey,FContext<'NodeKey,'NodeData,'EdgeData>>) -> 
            let p,d,s = kv.Value
            s|>Dictionary.containsKey kv.Key
        )
        |> fun x -> x<>0

    ///Returns if the given graph does not feature loops
    static member isSimple (graph: FContextMapU<'NodeKey, 'NodeData, 'EdgeData>) =
        graph
        |> Seq.countIf (fun (kv: KeyValuePair<'NodeKey,FContext<'NodeKey,'NodeData,'EdgeData>>) -> 
            let p,d,s = kv.Value
            s|>Dictionary.containsKey kv.Key
        )
        |> fun x -> x=0

    static member getNodeLabel (graph: FContextMapU<'NodeKey, 'NodeData, 'EdgeData>) (nk:'NodeKey) :'NodeData =
        graph.Item nk
        |> fun (p,d,s) -> d
    
  
    static member filterGraphByNodeKey (graph: FContextMapU<'NodeKey, 'NodeData, 'EdgeData>) (nodeFilter:'NodeKey ->  bool) : FContextMapU<'NodeKey, 'NodeData, 'EdgeData> =
        let newGraph :FContextMapU<'NodeKey, 'NodeData, 'EdgeData> = Dictionary<_,_>() 
        graph
        |> Seq.iter(fun kvp ->
            if nodeFilter (kvp.Key) then
                let p,d,s = kvp.Value
                
                let sNew:Dictionary<'NodeKey,'EdgeData> =  
                    let dicS :Dictionary<'NodeKey,'EdgeData> = new Dictionary<_,_>() 
                    s|>Seq.iter(fun tsv -> if nodeFilter tsv.Key then dicS.Add(tsv.Key,tsv.Value))
                    dicS

                newGraph.Add (kvp.Key,(Dictionary<_,_>(),d,sNew))
        )
        newGraph

    static member filterGraphByNodeKeyInplace (graph: FContextMapU<'NodeKey, 'NodeData, 'EdgeData>) (nodeFilter:'NodeKey ->  bool) : FContextMapU<'NodeKey, 'NodeData, 'EdgeData> =
        graph
        |> Seq.iter(fun (kvp:KeyValuePair<'NodeKey,FContext<'NodeKey, 'NodeData, 'EdgeData>>) -> 
            if nodeFilter (kvp.Key) |> not then
                graph.Remove kvp.Key |> ignore              
            else
                let p,d,s = kvp.Value
                s|>Seq.iter(fun tsv -> if nodeFilter tsv.Key |>not then s.Remove tsv.Key|>ignore)
        )
        graph

    static member filterGraphByEdge (graph: FContextMapU<'NodeKey, 'NodeData, 'EdgeData>) (edgeFilter:'NodeKey -> 'NodeKey -> bool) : FContextMapU<'NodeKey, 'NodeData, 'EdgeData> =
        let newGraph :FContextMapU<'NodeKey, 'NodeData, 'EdgeData> = Dictionary<_,_>() 

        graph
        |> Seq.iter(fun (kvp) -> 
            let (p,d,s) = kvp.Value
            let sNew:Dictionary<'NodeKey,'EdgeData> =  
                let dicS :Dictionary<'NodeKey,'EdgeData> = new Dictionary<_,_>() 
                s|>Seq.iter(fun tvp -> if edgeFilter kvp.Key tvp.Key then dicS.Add(tvp.Key,tvp.Value))
                dicS
            newGraph.Add(kvp.Key,(Dictionary<_,_>(),d,sNew))
        )

        newGraph

    static member filterGraphByEdgeInplace (graph: FContextMapU<'NodeKey, 'NodeData, 'EdgeData>) (edgeFilter:'NodeKey -> 'NodeKey -> bool) : FContextMapU<'NodeKey, 'NodeData, 'EdgeData> =
        graph
        |> Seq.iter(fun (kvp) -> 
            let (p,d,s) = kvp.Value        
            (s|>Seq.iter(fun tvp -> if edgeFilter kvp.Key tvp.Key |>not then s.Remove tvp.Key|>ignore))     
        )

        graph
    
    static member filterGraphByContext (graph: FContextMapU<'NodeKey, 'NodeData, 'EdgeData>) (contextFilter:FContext<'NodeKey, 'NodeData, 'EdgeData> -> bool) : FContextMapU<'NodeKey, 'NodeData, 'EdgeData> =
        let newGraph :FContextMapU<'NodeKey, 'NodeData, 'EdgeData> = Dictionary<_,_>() 
        graph
        |> Seq.iter(fun kvp ->
            if contextFilter (kvp.Value) then
                let p,d,s = kvp.Value
                
                let sNew:Dictionary<'NodeKey,'EdgeData> =  
                    let dicS :Dictionary<'NodeKey,'EdgeData> = new Dictionary<_,_>() 
                    s|>Seq.iter(fun tsv -> if contextFilter graph.[tsv.Key] then dicS.Add(tsv.Key,tsv.Value))
                    dicS

                newGraph.Add (kvp.Key,(Dictionary<_,_>(),d,sNew))
        )
        newGraph

    static member filterGraphByContextInplace (graph: FContextMapU<'NodeKey, 'NodeData, 'EdgeData>) (contextFilter:FContext<'NodeKey, 'NodeData, 'EdgeData> ->  bool) : FContextMapU<'NodeKey, 'NodeData, 'EdgeData> =
        graph
        |> Seq.iter(fun (kvp:KeyValuePair<'NodeKey,FContext<'NodeKey, 'NodeData, 'EdgeData>>) -> 
            if contextFilter (kvp.Value) |> not then
                graph.Remove kvp.Key |> ignore              
            else
                let p,d,s = kvp.Value
                s|>Seq.iter(fun tsv -> if contextFilter graph.[tsv.Key] |>not then s.Remove tsv.Key|>ignore)
        )
        graph

    static member mapNodeData (graph: FContextMapU<'NodeKey, 'NodeData, 'EdgeData>) (mapping:'NodeData -> 'NodeData_) : FContextMapU<'NodeKey, 'NodeData_, 'EdgeData> =
        let newGraph :FContextMapU<'NodeKey, 'NodeData_, 'EdgeData> = Dictionary<_,_>() 
        graph
        |> Seq.iter(fun kvp ->
            
            let p,d,s = kvp.Value
            let mappedNodeData = mapping d   
            let sCloned = new Dictionary<'NodeKey,'EdgeData>(s)

            newGraph.Add (kvp.Key,(Dictionary<_,_>(),mappedNodeData,sCloned))
        )
        newGraph

    static member mapNodeDataInplace (graph: FContextMapU<'NodeKey, 'NodeData, 'EdgeData>) (mapping:'NodeData -> 'NodeData) : FContextMapU<'NodeKey, 'NodeData, 'EdgeData> =
        graph
        |> Seq.iter(fun (kvp:KeyValuePair<'NodeKey,FContext<'NodeKey, 'NodeData, 'EdgeData>>) -> 
            let p,d,s = kvp.Value
            let mappedNodeData = mapping d   
            graph.Item kvp.Key <- (p,mappedNodeData,s)
            )
        graph

    static member mapNodeKey (graph: FContextMapU<'NodeKey, 'NodeData, 'EdgeData>) (mapping:'NodeKey -> 'NodeKey_) : FContextMapU<'NodeKey_, 'NodeData, 'EdgeData> =
        let newGraph :FContextMapU<'NodeKey_, 'NodeData, 'EdgeData> = Dictionary<_,_>() 
        graph
        |> Seq.iter(fun kvp ->
            let newKey = mapping kvp.Key
            let p,d,s = kvp.Value

            let sNew:Dictionary<'NodeKey_,'EdgeData> =  
                let dicS :Dictionary<'NodeKey_,'EdgeData> = new Dictionary<_,_>() 
                s
                |>Seq.iter(fun tvp -> 
                    let mappedNode = mapping tvp.Key
                    dicS.Add(mappedNode,tvp.Value)
                )
                dicS

            newGraph.Add (newKey,(Dictionary<_,_>(),d,sNew))
        )
        newGraph

    static member mapNodeKeyInplace (graph: FContextMapU<'NodeKey, 'NodeData, 'EdgeData>) (mapping:'NodeKey -> 'NodeKey) : FContextMapU<'NodeKey, 'NodeData, 'EdgeData> =
        graph
        |> Seq.iter(fun (kvp:KeyValuePair<'NodeKey,FContext<'NodeKey, 'NodeData, 'EdgeData>>) -> 
            let newKey = mapping kvp.Key
            let p,d,s = kvp.Value

            let sNew:Dictionary<'NodeKey,'EdgeData> =  
                let dicS :Dictionary<'NodeKey,'EdgeData> = new Dictionary<_,_>() 
                s
                |>Seq.iter(fun tvp -> 
                    let mappedNode = mapping tvp.Key
                    dicS.Add(mappedNode,tvp.Value)
                )
                dicS

            graph.Add (newKey,(Dictionary<_,_>(),d,sNew))
            graph.Remove kvp.Key |> ignore
        )
        graph

    static member mapEdgeData (graph: FContextMapU<'NodeKey, 'NodeData, 'EdgeData>) (mapping:'NodeKey -> 'NodeKey -> 'EdgeData -> 'EdgeData_) : FContextMapU<'NodeKey, 'NodeData, 'EdgeData_> =
        let newGraph :FContextMapU<'NodeKey, 'NodeData, 'EdgeData_> = Dictionary<_,_>() 
        graph
        |> Seq.iter(fun kvp ->
            let (p,d,s) = kvp.Value

            let sNew:Dictionary<'NodeKey,'EdgeData_> =  
                let dicS :Dictionary<'NodeKey,'EdgeData_> = new Dictionary<_,_>() 
                s
                |>Seq.iter(fun tvp -> 
                    let mappedEdgeData = mapping kvp.Key tvp.Key tvp.Value 
                    dicS.Add(tvp.Key,mappedEdgeData)
                )
                dicS

            newGraph.Add(kvp.Key,(Dictionary<_,_>(),d,sNew))
        )

        newGraph

    static member mapEdgeDataInplace (graph: FContextMapU<'NodeKey, 'NodeData, 'EdgeData>)  (mapping:'NodeKey -> 'NodeKey -> 'EdgeData -> 'EdgeData) : FContextMapU<'NodeKey, 'NodeData, 'EdgeData> =
        graph
        |> Seq.iter(fun kvp ->
            let (p,d,s) = kvp.Value

            s
            |>Seq.iter(fun (tvp:KeyValuePair<'NodeKey,'EdgeData>) ->
                let newEdgeData :'EdgeData = mapping (kvp.Key) (tvp.Key) (tvp.Value)
                s.Item tvp.Key <- (newEdgeData)
            )
        )
        graph

    static member mapContext (graph: FContextMapU<'NodeKey, 'NodeData, 'EdgeData>) (contextMapping:FContext<'NodeKey, 'NodeData, 'EdgeData> ->  FContext<'NodeKey, 'NodeData_, 'EdgeData_>) : FContextMapU<'NodeKey, 'NodeData_, 'EdgeData_> =
        let newGraph :FContextMapU<'NodeKey, 'NodeData_, 'EdgeData_> = Dictionary<_,_>() 
        graph
        |> Seq.iter(fun kvp ->
            let mappedContext = contextMapping kvp.Value

            newGraph.Add(kvp.Key,(mappedContext))
        )

        newGraph

    static member mapContextInplace (graph: FContextMapU<'NodeKey, 'NodeData, 'EdgeData>) (contextMapping:FContext<'NodeKey, 'NodeData, 'EdgeData> ->  FContext<'NodeKey, 'NodeData, 'EdgeData>) : FContextMapU<'NodeKey, 'NodeData, 'EdgeData> =
        graph
        |> Seq.iter(fun kvp ->
            let (p,d,s) = kvp.Value
            let mappedContext = contextMapping kvp.Value
            graph.Item kvp.Key <- mappedContext
        )
        graph


module FContextMapU =

    /// <summary> 
    /// Returns a new, empty graph
    /// </summary>
    /// <returns>Empty FContextMapU</returns>
    let empty<'NodeKey, 'NodeData, 'EdgeData when 'NodeKey: comparison> : FContextMapU<'NodeKey, 'NodeData, 'EdgeData> = 
        Dictionary<_,_>()
    
    type Node() =
        
        static member addNode (g : FContextMapU<'NodeKey, 'NodeData, 'EdgeData>) (nk:'NodeKey) (nd : 'NodeData) : FContextMapU<'NodeKey, 'NodeData, 'EdgeData> =
            FContextMapU.addNode nk nd g
        
        static member removeNode (g : FContextMapU<'NodeKey, 'NodeData, 'EdgeData>) (nk:'NodeKey)  : FContextMapU<'NodeKey, 'NodeData, 'EdgeData> =     
            FContextMapU.removeNode nk g
    
    type Edge() =
        static member addEdge (g : FContextMapU<'NodeKey,'NodeData,'EdgeData>) (nk1 : 'NodeKey) (nk2 : 'NodeKey) (ed : 'EdgeData) : FContextMapU<'NodeKey,'NodeData,'EdgeData> =
            FContextMapU.addEdge nk1 nk2 ed g

        static member removeEdge (g : FContextMapU<'NodeKey,'NodeData,'EdgeData>) (nkSource : 'NodeKey) (nkTarget : 'NodeKey) : FContextMapU<'NodeKey,'NodeData,'EdgeData> =
            FContextMapU.removeEdge nkSource nkTarget g