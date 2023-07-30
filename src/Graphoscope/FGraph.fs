namespace Graphoscope

open FSharpAux
open System.Collections.Generic

type Adj<'NodeKey, 'EdgeData> = seq<'NodeKey * 'EdgeData>

type FContext<'NodeKey, 'NodeData, 'EdgeData> when 'NodeKey: comparison =
     Dictionary<'NodeKey,'EdgeData> * 'NodeData * Dictionary<'NodeKey,'EdgeData>

type FGraph<'NodeKey,'NodeData,'EdgeData> when 'NodeKey: comparison =
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



module FContext =
    ///Lists the vertices which have edges pointing to the vertex.
    let predecessors (context:FContext<'NodeKey, 'NodeData, 'EdgeData>) : Adj<'NodeKey, 'EdgeData> = 
        let (p, _, _) = context
        seq {
            for kv in p do
                yield kv.Key,kv.Value 
        }

    ///Lists the vertices which have edges pointing away from the vertex.
    let successors (context:FContext<'NodeKey, 'NodeData, 'EdgeData>) : Adj<'NodeKey, 'EdgeData> = 
        let (_, _, s) = context
        seq {
            for kv in s do
                yield kv.Key,kv.Value 
        }
        
    ///Lists the vertices which are connected to the vertex.
    let neighbours (context:FContext<'NodeKey, 'NodeData, 'EdgeData>) : Adj<'NodeKey, 'EdgeData> =
        let (p, _, s) = context
        seq {
            for kv in p do
                yield kv.Key,kv.Value
            for kv in s do
                yield kv.Key,kv.Value 
        }

    // //Properties

    ///Evaluates the number of edges pointing to the vertex.
    let inwardDegree (context:FContext<'NodeKey, 'NodeData, 'EdgeData>) : int=
        let (p, _, _) = context
        p.Count

    ///Evaluates the number of edges pointing away from the vertex.
    let outwardDegree (context:FContext<'NodeKey, 'NodeData, 'EdgeData>) : int = 
        let (_, _, s) = context
        s.Count
    
    ///Evaluates the number of edges associated with the vertex.
    let degree (context:FContext<'NodeKey, 'NodeData, 'EdgeData>) : int =
        let (p, _, s) = context
        p.Count + s.Count




module FGraph =


    /// <summary> 
    /// Returns a new, empty graph
    /// </summary>
    /// <returns>Empty FGraph</returns>
    let empty<'NodeKey, 'NodeData, 'EdgeData when 'NodeKey: comparison> : FGraph<'NodeKey, 'NodeData, 'EdgeData> = 
        Dictionary<_,_>()
    
    type Node() =
    
        ///Adds a labeled node to the graph.
        static member  add (nk:'NodeKey) (nd : 'NodeData)  (g : FGraph<'NodeKey, 'NodeData, 'EdgeData>) : FGraph<'NodeKey, 'NodeData, 'EdgeData> =
            let mutable context = (null,Unchecked.defaultof<'NodeData>,null)
            match g.TryGetValue(nk,&context) with
            | true  -> context <- (Dictionary<_,_>(),nd,Dictionary<_,_>())
            | false -> g.Add(nk,(Dictionary<_,_>(),nd,Dictionary<_,_>()))            
            g

        ///Adds labeled nodes to the graph.
        static member addMany (nodeSeq:seq<(('NodeKey)*('NodeData))>) (g : FGraph<'NodeKey, 'NodeData, 'EdgeData>) : FGraph<'NodeKey, 'NodeData, 'EdgeData> =
            Seq.fold (fun graph (nk,nd) -> Node.add nk nd graph) g nodeSeq

        ///Evaluates the number of nodes in the graph.
        static member count (g : FGraph<'NodeKey, 'NodeData, 'EdgeData>) : int = 
            g.Count
 
        ///Returns true, if the node v is contained in the graph. Otherwise, it returns false.
        static member contains vk (g : FGraph<'NodeKey, 'NodeData, 'EdgeData>) : bool =
            Dictionary.containsKey vk g

        ///Lookup a labeled vertex in the graph. Raising KeyNotFoundException if no binding exists in the graph.
        static member find (n: 'NodeKey) (g : FGraph<'NodeKey, 'NodeData, 'EdgeData>) : ('NodeKey * 'NodeData) = 
            Dictionary.item n g
            |> fun (_, nd, _) -> n, nd
    


    ///Functions for edges of directed Graphs
    type Edge() = 

        ///Evaluates the number of edges in the graph.
        static member count (g: FGraph<'NodeKey,'NodeData,'EdgeData>) : int =
            g 
            |> Seq.sumBy (fun kv -> 
                let (_,_,s) = kv.Value
                s.Count
                )

        ///Returns true, if the edge from vertex v1 to vertex v2 is contained in the graph. Otherwise, it returns false.
        static member contains v1 v2 (g: FGraph<'NodeKey,'NodeData,'EdgeData>) : bool =
            let mutable context = (null,Unchecked.defaultof<'NodeData>,null)
            match g.TryGetValue(v1,&context) with
            | true  -> 
                let (_, _, s) = context
                s.ContainsKey(v2)
            | false -> false
            
    
        ///Lookup a labeled edge in the graph. Raising KeyNotFoundException if no binding exists in the graph.
        static member find (v1:'NodeKey) (v2:'NodeKey) (g : FGraph<'NodeKey,'NodeData,'EdgeData>) : 'NodeKey * 'NodeKey * 'EdgeData =
                Dictionary.item v1 g
                |> fun (_, _, s) -> Dictionary.item v2 s
                |> fun e -> (v1,v2,e)
    
        ///Lookup a labeled edge in the graph, returning a Some value if a binding exists and None if not.
        static member tryFind (nk1 : 'NodeKey) (nk2 : 'NodeKey) (g : FGraph<'NodeKey,'NodeData,'EdgeData>) : ('NodeKey * 'NodeKey * 'EdgeData) option =
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
        static member tryAdd (nk1 : 'NodeKey) (nk2 : 'NodeKey) (ed : 'EdgeData) (g : FGraph<'NodeKey,'NodeData,'EdgeData>) : FGraph<'NodeKey,'NodeData,'EdgeData> option =
            if (Node.contains nk1 g |> not) || (Node.contains nk2 g |> not) || Edge.contains nk1 nk2 g then
                None
            else 
                let (p1, nd1, s1) = Dictionary.item nk1 g
                Dictionary.addOrUpdateInPlace nk2 ed s1 |> ignore
                let (p2, nd2, s2) = Dictionary.item nk2 g
                Dictionary.addOrUpdateInPlace nk1 ed p2 |> ignore
                g |> Some

        ///Adds a labeled, directed edge to the graph.
        static member add (nk1 : 'NodeKey) (nk2 : 'NodeKey) (ed : 'EdgeData) (g : FGraph<'NodeKey,'NodeData,'EdgeData>) : FGraph<'NodeKey,'NodeData,'EdgeData> =
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
        static member addMany (edgeSeq:seq<('NodeKey)*('NodeKey)*('EdgeData)>) (g : FGraph<'NodeKey,'NodeData,'EdgeData>) : FGraph<'NodeKey,'NodeData,'EdgeData> =
            Seq.fold (fun graph (nk1,nk2,ed) -> Edge.add nk1 nk2 ed graph) g edgeSeq

        /// Applies the given function on each egdge of the graph
        static member iter (action : 'NodeKey -> 'NodeKey -> 'EdgeData -> unit) (graph: FGraph<'NodeKey, 'NodeData, 'EdgeData>) =
            for skv in graph do
                let (_, _, s) = skv.Value
                for tkv in s do  
                    action skv.Key tkv.Key tkv.Value
        
        /// Applies the given function on every edge of the graph, which also receives an ascending integer index.
        static member iteri (action : int -> 'NodeKey -> 'NodeKey -> 'EdgeData -> unit) (graph: FGraph<'NodeKey, 'NodeData, 'EdgeData>) =
            let mutable index = -1
            for skv in graph do
                let (_, _, s) = skv.Value
                for tkv in s do  
                    index <- index + 1
                    action index skv.Key tkv.Key tkv.Value
        
type FGraph() = 
    
    /// <summary> 
    /// Creates a new, empty graph
    /// </summary>
    /// <returns>Empty FGraph</returns>
    static member create<'NodeKey, 'NodeData, 'EdgeData when 'NodeKey: comparison>() : FGraph<'NodeKey, 'NodeData, 'EdgeData> =
        Dictionary<_,_>()
 
 
    /// <summary> 
    /// Converts the FGraph to an array2d 
    /// </summary>
    /// <param name="graph">The graph to be converted</param> 
    /// <returns>An array2d</returns>
    static member toArray2D (nodeIndexer : 'NodeKey -> int)  =
        (fun (g : FGraph<'NodeKey,'NodeData,'EdgeData>) ->
            //let nodeIndex =
            //    // TODO: better without sorting 
            //    g
            //    |> Seq.sortBy (fun kv -> kv.Key)
            //    |> Seq.mapi (fun i kv -> kv.Key,i)
            //    |> Dict.ofSeq 
            let n = g.Count
            let matrix = Array2D.zeroCreate n n
            for skv in g do
                let (_, _, s) = skv.Value
                for tkv in s do  
                    matrix.[nodeIndexer skv.Key,nodeIndexer tkv.Key] <- tkv.Value
            
            matrix
            )

    /// <summary> 
    /// Adds a labeled, directed edge to the graph.
    /// </summary>
    
    /// <returns>FGraph with new element</returns>
    static member addElement (nk1 : 'NodeKey) (nd1 : 'NodeData) (nk2 : 'NodeKey) (nd2 : 'NodeData) (ed : 'EdgeData) (g : FGraph<'NodeKey,'NodeData,'EdgeData>) : FGraph<'NodeKey,'NodeData,'EdgeData> =
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

    ///Maps contexts of the graph.
    static member mapContexts (mapping : FContext<'NodeKey, 'NodeData, 'EdgeData> -> 'T) (g : FGraph<'NodeKey,'NodeData,'EdgeData>) : seq<'NodeKey * 'T>= 
        g
        |> Seq.map (fun kv -> kv.Key,mapping kv.Value )

     /// <summary> 
     /// Returns the FGraph content as a sequence of edges 
     /// </summary>
    static member toSeq  (graph: FGraph<'NodeKey, 'NodeData, 'EdgeData>) = 
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
    static member ofSeq(edgelist : seq<'NodeKey * 'NodeData * 'NodeKey * 'NodeData * 'EdgeData>) 
        : FGraph<'NodeKey, 'NodeData, 'EdgeData> = 
        let graph = FGraph.empty
        edgelist
        |> Seq.iter (fun (sk,s,tk,t,ed) -> FGraph.addElement sk s tk t ed graph |> ignore)
        graph
        

        // ///Maps edgeData of the graph.
        // static member map 
        // /// Returns a new graph containing only the edges for which the given predicate returns true.
        // static member filter (predicate)
        // ///Removes an edge from the graph.
        // static member remove 
         