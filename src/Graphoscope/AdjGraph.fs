namespace Graphoscope

open FSharpAux
open System.Collections.Generic


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
    static member addElement (sourceKey : 'NodeKey) (source : 'NodeData)  (targetKey : 'NodeKey) (target : 'NodeData) (data : 'EdgeData) 
        (graph: AdjGraph<'NodeKey, 'NodeData, 'EdgeData>) =            
        
        match graph.ContainsKey(sourceKey) with
        | true  -> 
                match graph.ContainsKey(targetKey) with
                | true  ->
                    let _,adjComponent = graph.[sourceKey]
                    Dictionary.addOrUpdateInPlace targetKey data adjComponent |> ignore
                    graph.[sourceKey] <- (source,adjComponent)
                    // potentially updata tagetData
                    let _,adjComponentTarget = graph.[targetKey]
                    graph.[targetKey] <- (target,adjComponentTarget)
                | false ->
                    let _,adjComponent = graph.[sourceKey]
                    Dictionary.addOrUpdateInPlace targetKey data adjComponent |> ignore
                    graph.[sourceKey] <- (source,adjComponent)
                    // insert target node
                    Dictionary.addOrUpdateInPlace targetKey (target,Dictionary<'NodeKey,_>()) graph |> ignore

        | false -> 
            let adjComponent = Dictionary<'NodeKey, 'EdgeData>()
            adjComponent.Add(targetKey, data)
            graph.Add(sourceKey,(source,adjComponent))
            match graph.ContainsKey(targetKey) with
            | true ->
                // update target node
                let _,adjComponent = graph.[targetKey]
                Dictionary.addOrUpdateInPlace targetKey (target,adjComponent) graph |> ignore
            | false ->
                // insert target node
                Dictionary.addOrUpdateInPlace targetKey (target,Dictionary<'NodeKey,_>()) graph |> ignore
                
        graph


    /// <summary> 
    /// Returns the Adjacency graph conetent as a sequence of edges 
    /// </summary>
    static member toSeq  (graph: AdjGraph<'NodeKey, 'NodeData, 'EdgeData>) = 
        seq {
            for skv in graph do
                let source, adjComponent = skv.Value
                for tkv in adjComponent do
                    let _,target = graph.[tkv.Key]                     
                    yield (skv.Key,source,tkv.Key,target,tkv.Value)
        }

    
    /// <summary> 
    /// Creates an Adjacency graph of a sequence of edges
    /// </summary>
    static member ofSeq(edgelist : seq<'NodeKey * 'NodeData * 'NodeKey * 'NodeData * 'EdgeData>) 
        : AdjGraph<'NodeKey, 'NodeData, 'EdgeData> = 
        let graph = AdjGraph.create()//new AdjGraph<'NodeKey, 'Nodedata,'EdgeData>()
        edgelist
        |> Seq.iter (fun e -> AdjGraph.addElement e graph |> ignore)
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
        static member add (key:'NodeKey) (data:'NodeData) (graph: AdjGraph<'NodeKey, 'NodeData, 'EdgeData>)
            : AdjGraph<'NodeKey, 'NodeData, 'EdgeData> =
            Dictionary.addOrUpdateInPlace key (data,Dictionary<'NodeKey,_>()) graph |> ignore
            graph

        /// 
        static member containsKey (key:'NodeKey) (graph: AdjGraph<'NodeKey, 'NodeData, 'EdgeData>) =
            graph.ContainsKey key

         /// Applies the given function to each node of the graph
        static member iter (action : 'NodeKey -> 'NodeData -> unit) (graph: AdjGraph<'NodeKey, 'NodeData, 'EdgeData>) =
            for kv in graph do
                let node, _ = kv.Value
                action kv.Key node
                

         /// Applies the given function to each node of the graph
        static member iteri (action : int -> 'NodeKey -> 'NodeData -> unit) (graph: AdjGraph<'NodeKey, 'NodeData, 'EdgeData>) =
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

        /// Converts nodes to nodeKey * nodeData array 
        static member toArray (graph: AdjGraph<'NodeKey, 'NodeData, 'EdgeData>) =
            let tmp = Array.zeroCreate graph.Count 
            Node.iteri (fun i key node -> tmp.[i] <- key,node) graph
            tmp

    /// <summary> 
    /// Functions operating on directed edges
    /// </summary>
    type Edge() =
        
        /// Counts all edges 
        static member count (graph: AdjGraph<'NodeKey, 'NodeData, 'EdgeData>) =
            graph
            |> Seq.sumBy (fun kv -> 
                let _, adjComponent = kv.Value
                adjComponent.Count
                )


        /// Add edge
        static member add (sourceKey : 'NodeKey) (targetKey : 'NodeKey) (data : 'EdgeData) (graph: AdjGraph<'NodeKey, 'NodeData, 'EdgeData>) =                
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



        /// Applies the given function to each node of the graph
        static member iter (action : 'NodeKey -> 'NodeKey -> 'EdgeData -> unit) (graph: AdjGraph<'NodeKey, 'NodeData, 'EdgeData>) =
            for skv in graph do
                let source, adjComponent = skv.Value
                for tkv in adjComponent do  
                    action skv.Key tkv.Key tkv.Value
        
        /// <summary> 
        /// Tries to find an edge between the specified nodes. Raises Exception if no such edge exists in the graph.
        /// </summary>
        static member find (sourceKey : 'NodeKey) (targetKey : 'NodeKey) (graph: AdjGraph<'NodeKey, 'NodeData, 'EdgeData>) =
            match graph.ContainsKey(sourceKey) with
            | true  -> 
                let source,adjComponent = graph.[sourceKey]
                match adjComponent.ContainsKey(targetKey) with
                | true -> sourceKey,targetKey,adjComponent.[targetKey]
                | false -> failwithf "Edge %O - %O does not exist in this graph." sourceKey targetKey 
            | false -> 
                failwithf "The source node %O of the edge does not exist in this graph." sourceKey
    

        //static member map (graph: AdjGraph<'NodeKey, 'NodeData, 'EdgeData>) =
            

