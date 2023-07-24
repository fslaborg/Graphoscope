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
    static member empty<'NodeKey, 'NodeData, 'EdgeData when 'NodeKey : comparison>() 
        : AdjGraph<'NodeKey, 'NodeData, 'EdgeData> =
        AdjGraph<'NodeKey, 'NodeData, 'EdgeData>()


    // Adds a labeled, edge to the graph.
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

    /// 
    /// <summary> 
    /// Returns an Adjacency graph as a sequence of edges
    /// </summary>
    /// <returns>Empty AdjGraph</returns>
    static member toSeq  (graph: AdjGraph<'NodeKey, 'NodeData, 'EdgeData>) = 
        seq {
            for skv in graph do
                let source, adjComponent = skv.Value
                for tkv in adjComponent do
                    let _,target = graph.[tkv.Key]                     
                    yield (skv.Key,source,tkv.Key,target,tkv.Value)
        }

    /// Creates a graph of a sequence of edges
    
    /// <summary> 
    /// Returns an Adjacency graph as a sequence of edges
    /// </summary>
    /// <returns>Empty AdjGraph</returns>
    static member ofSeq(edgelist : seq<'NodeKey * 'NodeData * 'NodeKey * 'NodeData * 'EdgeData>) 
        : AdjGraph<'NodeKey, 'NodeData, 'EdgeData> = 
        let graph = AdjGraph.create ()//new AdjGraph<'NodeKey, 'Nodedata,'EdgeData>()
        edgelist
        |> Seq.iter (fun e -> AdjGraph.addElement e graph |> ignore)
        graph



module AdjGraph =
    
    type Node() =

        /// Counts all nodes 
        static member count (graph: AdjGraph<'NodeKey, 'NodeData, 'EdgeData>) =
            graph.Count

        /// Adds node to graph [if node exists it is updated]
        static member add (key:'NodeKey) (data:'NodeData) (graph: AdjGraph<'NodeKey, 'NodeData, 'EdgeData>)
            : AdjGraph<'NodeKey, 'NodeData, 'EdgeData> =
            Dictionary.addOrUpdateInPlace key (data,Dictionary<'NodeKey,_>()) graph |> ignore
            graph

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
        static member map (mapping : 'NodeKey -> 'NodeData -> Node<'NodeKey, 'NodeData>) (graph: AdjGraph<'NodeKey, 'NodeData, 'EdgeData>) =
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

    type Edge() =
        
        /// Counts all edges 
        static member count (graph: AdjGraph<'NodeKey, 'NodeData, 'EdgeData>) =
            graph
            |> Seq.sumBy (fun kv -> 
                let _, adjComponent = kv.Value
                adjComponent.Count
                )

        /// Applies the given function to each node of the graph
        static member iter (action : 'NodeKey -> 'NodeKey -> 'EdgeData -> unit) (graph: AdjGraph<'NodeKey, 'NodeData, 'EdgeData>) =
            for skv in graph do
                let source, adjComponent = skv.Value
                for tkv in adjComponent do  
                    action skv.Key tkv.Key tkv.Value
    
        //static member map (graph: AdjGraph<'NodeKey, 'NodeData, 'EdgeData>) =
            

