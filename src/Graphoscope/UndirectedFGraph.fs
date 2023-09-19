// namespace Graphoscope

// open FSharpAux
// open System.Collections.Generic

// type Adj<'NodeKey, 'EdgeData> = Adj<'NodeKey * 'EdgeData>

// type FContext<'NodeKey, 'NodeData, 'EdgeData> when 'NodeKey: comparison =
//     FContext<'NodeKey, 'NodeData, 'EdgeData>

// type UndirectedFGraph<'NodeKey,'NodeData,'EdgeData> when 'NodeKey: comparison =
//     UndirectedFGraph<'NodeKey,'NodeData,'EdgeData>
namespace Graphoscope

open FSharpAux
open System.Collections.Generic

type UndirectedFGraph<'NodeKey,'NodeData,'EdgeData> when 'NodeKey: comparison =
    Dictionary<'NodeKey, FContext<'NodeKey, 'NodeData, 'EdgeData>>

// (* Transition functions *)

// let internal fromAdj<'NodeKey,'EdgeData when 'NodeKey: comparison> : Adj<'NodeKey, 'EdgeData> -> Dictionary<'NodeKey,'EdgeData> =
//     fun x -> Dictionary.ofSeq x |> upcast 

// let internal fromContext<'Vertex,'Label,'Edge when 'Vertex: comparison> : Context<'Vertex,'Label,'Edge> -> MContext<'Vertex,'Label,'Edge> =
//     fun (p, _, l, s) -> fromAdj p, l, fromAdj s

// let internal toAdj<'NodeKey,'EdgeData when 'NodeKey: comparison> : Dictionary<'NodeKey,'EdgeData> -> Adj<'NodeKey,'EdgeData> =
//     Dictionary.toSeq

// let internal toContext (v:'Vertex) (mc : MContext<'Vertex,'Label,'Edge>) : Context<'Vertex,'Label,'Edge> =
//     mc
//     |> fun (p, l, s) -> toAdj p, v, l, toAdj s
 

type UndirectedFGraph() = 

    /// <summary> 
    /// Creates a new graph with the given Data
    /// </summary>
    /// <returns>UndirectedFGraph</returns>
    static member create<'NodeKey, 'NodeData, 'EdgeData when 'NodeKey: comparison>() : UndirectedFGraph<'NodeKey, 'NodeData, 'EdgeData> =
        Dictionary<_,_>()
    
    /// <summary> 
    /// Adds a labeled, directed edge to the graph.
    /// </summary>

    /// <returns>UndirectedFGraph with new element</returns>
    static member addElement (nk1 : 'NodeKey) (nd1 : 'NodeData) (nk2 : 'NodeKey) (nd2 : 'NodeData) (ed : 'EdgeData) (g : UndirectedFGraph<'NodeKey,'NodeData,'EdgeData>) : UndirectedFGraph<'NodeKey,'NodeData,'EdgeData> =
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
                        p2.Add(nk1,ed)
                | false -> 
                    let p1, nd1, s1 = contextNk1
                    s1.Add(nk2,ed)                                        
                    let p2 = Dictionary<_,_>()
                    p2.Add(nk1,ed)
                    g.Add(nk2,(p2,nd2,Dictionary<_,_>()))               
            else
                // inser self loop p
                let p1, nd1, s1 = contextNk1
                match s1.ContainsKey(nk2) with
                | true -> ()
                | false ->
                    // Potentially update edge data
                    s1.Add(nk2,ed)                
                    p1.Add(nk1,ed)
                
        | false -> 
            let mutable contextNk2 = (null,Unchecked.defaultof<'NodeData>,null)
            match g.TryGetValue(nk2,&contextNk2) with
            | true  ->   
                let s1 = Dictionary<_,_>()                
                s1.Add(nk2,ed)            
                g.Add(nk1,(Dictionary<_,_>(),nd1,s1))                
                let (p2, nd2, s2) = contextNk2
                p2.Add(nk1,ed)                                                
            | false ->                 
                if nk1 <> nk2 then
                    let s1 = Dictionary<_,_>()                
                    s1.Add(nk2,ed)            
                    g.Add(nk1,(Dictionary<_,_>(),nd1,s1))     
                    
                    let p2 = Dictionary<_,_>()
                    p2.Add(nk1,ed)
                    g.Add(nk2,(p2,nd2,Dictionary<_,_>()))
                else
                    let s1 = Dictionary<_,_>()                
                    s1.Add(nk2,ed)
                    let p1 = Dictionary<_,_>()
                    p1.Add(nk1,ed)            
                    g.Add(nk1,(p1,nd1,s1))                       
        g    


    ///Adds a labeled node to the graph.
    static member  addNode (nk:'NodeKey) (nd : 'NodeData)  (g : UndirectedFGraph<'NodeKey, 'NodeData, 'EdgeData>) : UndirectedFGraph<'NodeKey, 'NodeData, 'EdgeData> =
        let mutable context = (null,Unchecked.defaultof<'NodeData>,null)
        match g.TryGetValue(nk,&context) with
        | true  -> context <- (Dictionary<_,_>(),nd,Dictionary<_,_>())
        | false -> g.Add(nk,(Dictionary<_,_>(),nd,Dictionary<_,_>()))            
        g

    ///Adds labeled nodes to the graph.
    static member addNodes (nodeSeq:seq<(('NodeKey)*('NodeData))>) (g : UndirectedFGraph<'NodeKey, 'NodeData, 'EdgeData>) : UndirectedFGraph<'NodeKey, 'NodeData, 'EdgeData> =
        Seq.iter (fun (nk,nd) -> (UndirectedFGraph.addNode nk nd g)|>ignore) nodeSeq |>ignore
        g

    ///Evaluates the number of nodes in the graph.
    static member countNodes (g : UndirectedFGraph<'NodeKey, 'NodeData, 'EdgeData>) : int = 
        g.Count
 
    ///Returns true, if the node v is contained in the graph. Otherwise, it returns false.
    static member containsNode vk (g : UndirectedFGraph<'NodeKey, 'NodeData, 'EdgeData>) : bool =
        Dictionary.containsKey vk g

    ///Lookup a labeled vertex in the graph. Raising KeyNotFoundException if no binding exists in the graph.
    static member findNode (n: 'NodeKey) (g : UndirectedFGraph<'NodeKey, 'NodeData, 'EdgeData>) : ('NodeKey * 'NodeData) = 
        Dictionary.item n g
        |> fun (_, nd, _) -> n, nd

    ///Set the NodeData of a given NodeKey to the given NodeData
    static member setNodeData (n: 'NodeKey) (nd: 'NodeData) (g : UndirectedFGraph<'NodeKey, 'NodeData, 'EdgeData>) : UndirectedFGraph<'NodeKey, 'NodeData, 'EdgeData> = 
        let p,_,s = g.Item n
        g.Item n <- (p,nd,s)
        g

    ///Maps contexts of the graph.
    static member mapContexts (mapping : FContext<'NodeKey, 'NodeData, 'EdgeData> -> 'T) (g : UndirectedFGraph<'NodeKey,'NodeData,'EdgeData>) : seq<'NodeKey * 'T>= 
        g
        |> Seq.map (fun kv -> kv.Key,mapping kv.Value )

    /////Remove the Node and all edges connected to it
    static member removeNode (nk:'NodeKey) (g : UndirectedFGraph<'NodeKey, 'NodeData, 'EdgeData>) : UndirectedFGraph<'NodeKey, 'NodeData, 'EdgeData> = 
        match UndirectedFGraph.containsNode nk g with
        |true   -> 
            let p,_,s = g.Item nk
            for k1 in p do
                g.Item k1.Key|> fun (p,nd,s) -> s.Remove nk |>ignore
            for k2 in s do
                g.Item k2.Key|> fun (p,nd,s) -> p.Remove nk |>ignore
            g.Remove nk |>ignore
            g 
        |_ -> g

     /// <summary> 
     /// Returns the UndirectedFGraph content as a sequence of edges 
     /// </summary>
    static member toSeq (graph: UndirectedFGraph<'NodeKey, 'NodeData, 'EdgeData>) = 
        seq {
             for skv in graph do
                let (_, source, s) = skv.Value
                for tkv in s do                      
                    let (_, target, _) = graph.[tkv.Key]
                    yield (skv.Key,source,tkv.Key,target,tkv.Value)
         }

    /// <summary> 
    /// Creates an Adjacency graph of a sequence of edges
    /// </summary>
    static member ofSeq (edgelist : seq<'NodeKey * 'NodeData * 'NodeKey * 'NodeData * 'EdgeData>) 
        : UndirectedFGraph<'NodeKey, 'NodeData, 'EdgeData> = 
        let graph = UndirectedFGraph.create()
        edgelist
        |> Seq.iter (fun (sk,s,tk,t,ed) -> UndirectedFGraph.addElement sk s tk t ed graph |> ignore)
        graph

    /// <summary>
    /// Creates an UndirectedFGraph consisting of the Nodes of a given UndirectedFGraph but with its directed Edges reversed.
    /// </summary>
    static member reverseEdges (graph : UndirectedFGraph<'NodeKey, 'NodeData, 'EdgeData>) = 
        let newGraph = Dictionary<'NodeKey, FContext<'NodeKey, 'NodeData, 'EdgeData>>()
        (graph.Keys, graph.Values)
        ||> Seq.iter2 (
            fun n e -> 
                let edgeSource, edgeLabel, edgeSink = e
                let newEdgeSource = Dictionary<'NodeKey,'EdgeData>()
                (edgeSink.Keys, edgeSink.Values)
                ||> Seq.iter2 (fun k d -> newEdgeSource.Add(k, d))
                let newEdgeSink = Dictionary<'NodeKey,'EdgeData>()
                (edgeSource.Keys, edgeSource.Values)
                ||> Seq.iter2 (fun k d -> newEdgeSink.Add(k, d))
                newGraph.Add(n, (newEdgeSource, edgeLabel, newEdgeSink))
        )
        newGraph

    /// <summary> 
    /// Converts the UndirectedFGraph to an array2d 
    /// </summary>
    /// <param name="graph">The graph to be converted</param> 
    /// <returns>An array2d</returns>
    static member toArray2D(nodeIndexer : 'NodeKey -> int)  =       
        (fun (g : UndirectedFGraph<'NodeKey,'NodeData,'EdgeData>) ->
                let n = g.Count
                let matrix = Array2D.zeroCreate n n
                for skv in g do
                    let (_, _, s) = skv.Value
                    for tkv in s do  
                        matrix.[nodeIndexer skv.Key,nodeIndexer tkv.Key] <- tkv.Value
                        matrix.[nodeIndexer tkv.Key,nodeIndexer skv.Key] <- tkv.Value
                matrix
                )

    ///Evaluates the number of edges in the graph.
    static member countEdges (g: UndirectedFGraph<'NodeKey,'NodeData,'EdgeData>) : int =
        g 
        |> Seq.sumBy (fun kv -> 
            let (_,_,s) = kv.Value
            s.Count
            )

    ///Returns true, if the edge from vertex v1 to vertex v2 is contained in the graph. Otherwise, it returns false.
    static member containsEdge v1 v2 (g: UndirectedFGraph<'NodeKey,'NodeData,'EdgeData>) : bool =
        let mutable context = (null,Unchecked.defaultof<'NodeData>,null)
        match g.TryGetValue(v1,&context) with
        | true  -> 
            let (_, _, s) = context
            s.ContainsKey(v2)
        | false -> false
            
    
    ///Lookup a labeled edge in the graph. Raising KeyNotFoundException if no binding exists in the graph.
    static member findEdge (v1:'NodeKey) (v2:'NodeKey) (g : UndirectedFGraph<'NodeKey,'NodeData,'EdgeData>) : 'NodeKey * 'NodeKey * 'EdgeData =
            Dictionary.item v1 g
            |> fun (_, _, s) -> Dictionary.item v2 s
            |> fun e -> (v1,v2,e)
    
    ///Lookup a labeled edge in the graph, returning a Some value if a binding exists and None if not.
    static member tryFindEdge (nk1 : 'NodeKey) (nk2 : 'NodeKey) (g : UndirectedFGraph<'NodeKey,'NodeData,'EdgeData>) : ('NodeKey * 'NodeKey * 'EdgeData) option =
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
    static member tryAddEdge (nk1 : 'NodeKey) (nk2 : 'NodeKey) (ed : 'EdgeData) (g : UndirectedFGraph<'NodeKey,'NodeData,'EdgeData>) : UndirectedFGraph<'NodeKey,'NodeData,'EdgeData> option =
        if (UndirectedFGraph.containsNode nk1 g |> not) || (UndirectedFGraph.containsNode nk2 g |> not) || UndirectedFGraph.containsEdge nk1 nk2 g then
            None
        else 
            let (p1, nd1, s1) = Dictionary.item nk1 g
            Dictionary.addOrUpdateInPlace nk2 ed s1 |> ignore
            let (p2, nd2, s2) = Dictionary.item nk2 g
            Dictionary.addOrUpdateInPlace nk1 ed p2 |> ignore
            g |> Some

    ///Adds a labeled, directed edge to the graph.
    static member addEdge (nk1 : 'NodeKey) (nk2 : 'NodeKey) (ed : 'EdgeData) (g : UndirectedFGraph<'NodeKey,'NodeData,'EdgeData>) : UndirectedFGraph<'NodeKey,'NodeData,'EdgeData> =
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
                        p2.Add(nk1,ed)
            else
                let p1, nd1, s1 = contextNk1
                match s1.ContainsKey(nk1) with 
                | true -> failwithf "An Edge between Source Node %O Target Node %O does already exist" nk1 nk2
                | false ->
                    s1.Add(nk1,ed)                
                    p1.Add(nk1,ed)
                    
        g
        
    ///Add labeled, directed edges to the graph.
    static member addEdges (edgeSeq:seq<('NodeKey)*('NodeKey)*('EdgeData)>) (g : UndirectedFGraph<'NodeKey,'NodeData,'EdgeData>) : UndirectedFGraph<'NodeKey,'NodeData,'EdgeData> =
        Seq.iter (fun (nk1,nk2,ed) -> UndirectedFGraph.addEdge nk1 nk2 ed g|>ignore) edgeSeq|>ignore
        g

    ///Remove a directed edge
    static member removeEdge (nkSource : 'NodeKey) (nkTarget : 'NodeKey) (g : UndirectedFGraph<'NodeKey,'NodeData,'EdgeData>) : UndirectedFGraph<'NodeKey,'NodeData,'EdgeData> =
        match UndirectedFGraph.containsEdge nkSource nkTarget g with
        | true    -> 
            g.Item nkSource|> fun (p,nd,s) -> s.Remove nkTarget |>ignore
            g.Item nkTarget|> fun (p,nd,s) -> p.Remove nkSource |>ignore
            g
        | _         -> g 


    ///Removes all edges according to the given removeF
    static member removeMany (edgeSeq:seq<('NodeKey)*('NodeKey)>) (removeF: 'NodeKey->'NodeKey->UndirectedFGraph<'NodeKey,'NodeData,'EdgeData> -> UndirectedFGraph<'NodeKey,'NodeData,'EdgeData>) (g : UndirectedFGraph<'NodeKey,'NodeData,'EdgeData>) : UndirectedFGraph<'NodeKey,'NodeData,'EdgeData> =
        Seq.iter(fun (nk1,nk2) -> (removeF nk1 nk2 g )|>ignore) edgeSeq|>ignore
        g

    /// Applies the given function on each egdge of the graph
    static member iterEdges (action : 'NodeKey -> 'NodeKey -> 'EdgeData -> unit) (graph: UndirectedFGraph<'NodeKey, 'NodeData, 'EdgeData>) =
        for skv in graph do
            let (_, _, s) = skv.Value
            for tkv in s do  
                action skv.Key tkv.Key tkv.Value
        
    /// Applies the given function on every edge of the graph, which also receives an ascending integer index.
    static member iteriEdges (action : int -> 'NodeKey -> 'NodeKey -> 'EdgeData -> unit) (graph: UndirectedFGraph<'NodeKey, 'NodeData, 'EdgeData>) =
        let mutable index = -1
        for skv in graph do
            let (_, _, s) = skv.Value
            for tkv in s do  
                index <- index + 1
                action index skv.Key tkv.Key tkv.Value

    /// <summary> 
    /// Returns the UndirectedFGraph edges as a sequence of edges 
    /// </summary>
    static member toEdgeSeq (graph: UndirectedFGraph<'NodeKey, 'NodeData, 'EdgeData>) = 
        seq {
             for skv in graph do
                let (_, source, s) = skv.Value
                for tkv in s do                      
                    yield (skv.Key,tkv.Key,tkv.Value)
         }

    /// <summary> 
    /// Creates a new graph with the given node Data
    /// </summary>
    /// <returns>UndirectedFGraph</returns>
    static member createFromNodes (nodes:seq<(('NodeKey)*('NodeData))>) :UndirectedFGraph<'NodeKey,'NodeData,'EdgeData> =
        UndirectedFGraph.create()
        |> UndirectedFGraph.addNodes nodes

    // static member createOfEdgeSeq (edges:seq<('NodeKey)*('NodeKey)*('EdgeData)>) :UndirectedFGraph<'NodeKey,'NodeKey,'EdgeData> =
    //     let nodes: seq<'NodeKey * 'NodeKey> = 
    //         edges
    //         |> Seq.map(fun (s,t,w) -> [s,s;t,t])
    //         |> Seq.concat
    //         |> Seq.distinct
    //     UndirectedFGraph.create()
    //     |> UndirectedFGraph.addNodes nodes
    //     |> UndirectedFGraph.addEdges edges

    /// <summary> 
    /// Creates a new graph with the given Data
    /// </summary>
    /// <returns>UndirectedFGraph</returns>
    static member create ((nodes:seq<(('NodeKey)*('NodeData))>),(edges:seq<('NodeKey)*('NodeKey)*('EdgeData)>)) :UndirectedFGraph<'NodeKey,'NodeData,'EdgeData> =
        UndirectedFGraph.createFromNodes nodes
        |> UndirectedFGraph.addEdges edges
        
        // ///Maps edgeData of the graph.
        // static member map 
        // /// Returns a new graph containing only the edges for which the given predicate returns true.
        // static member filter (predicate)
        // ///Removes an edge from the graph.
        // static member remove 
        

module UndirectedFGraph =

    /// <summary> 
    /// Returns a new, empty graph
    /// </summary>
    /// <returns>Empty UndirectedFGraph</returns>
    let empty<'NodeKey, 'NodeData, 'EdgeData when 'NodeKey: comparison> : UndirectedFGraph<'NodeKey, 'NodeData, 'EdgeData> = 
        Dictionary<_,_>()
    
    type Node() =
        
        static member addNode (g : UndirectedFGraph<'NodeKey, 'NodeData, 'EdgeData>) (nk:'NodeKey) (nd : 'NodeData) : UndirectedFGraph<'NodeKey, 'NodeData, 'EdgeData> =
            UndirectedFGraph.addNode nk nd g
        
        static member removeNode (g : UndirectedFGraph<'NodeKey, 'NodeData, 'EdgeData>) (nk:'NodeKey)  : UndirectedFGraph<'NodeKey, 'NodeData, 'EdgeData> =     
            UndirectedFGraph.removeNode nk g
    
    type Edge() =
        static member addEdge (g : UndirectedFGraph<'NodeKey,'NodeData,'EdgeData>) (nk1 : 'NodeKey) (nk2 : 'NodeKey) (ed : 'EdgeData) : UndirectedFGraph<'NodeKey,'NodeData,'EdgeData> =
            UndirectedFGraph.addEdge nk1 nk2 ed g

        static member removeEdge (g : UndirectedFGraph<'NodeKey,'NodeData,'EdgeData>) (nkSource : 'NodeKey) (nkTarget : 'NodeKey) : UndirectedFGraph<'NodeKey,'NodeData,'EdgeData> =
            UndirectedFGraph.removeEdge nkSource nkTarget g