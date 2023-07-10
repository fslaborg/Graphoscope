namespace Graphoscope

open FSharpAux
open System.Collections.Generic

type AdjGraph<'NodeKey, 'NodeData, 'EdgeData when 'NodeKey : comparison> 
    internal(nodes:Dictionary<'NodeKey, 'NodeData>,adjComponents : Dictionary<'NodeKey, Dictionary<'NodeKey, 'EdgeData>>) =
    
    new() = AdjGraph<'NodeKey, 'NodeData, 'EdgeData>(
        new Dictionary<'NodeKey, 'NodeData>(),
        new Dictionary<'NodeKey, Dictionary<'NodeKey, 'EdgeData>>()
        )
        
    /// Adds node to graph [if node exists it is updated]
    member this.AddNode (key:'NodeKey) (data:'NodeData)  =
        Dictionary.addOrUpdateInPlace key data nodes |> ignore
        this

    //Adds a labeled, edge to the graph.
    member this.AddEdgeWithNodes (sourceKey : 'NodeKey) (source : 'NodeData)  (targetKey : 'NodeKey) (target : 'NodeData) (data : 'EdgeData) =
        Dictionary.addOrUpdateInPlace sourceKey source nodes |> ignore
        Dictionary.addOrUpdateInPlace targetKey target nodes |> ignore
        match adjComponents.ContainsKey(sourceKey) with
        | true  -> 
            let adjComponent = adjComponents.[sourceKey]
            Dictionary.addOrUpdateInPlace targetKey data adjComponent |> ignore
            adjComponents.[sourceKey] <- adjComponent
        | false -> 
            let adjComponent = Dictionary<'NodeKey, 'EdgeData>()
            Dictionary.addOrUpdateInPlace targetKey data adjComponent |> ignore
            adjComponents.Add(sourceKey,adjComponent)
        this    
        


    //Adds a labeled, edge to the graph.
    member this.AddEdge (source : 'NodeKey) (target : 'NodeKey) (data : 'EdgeData) =
        if not <| nodes.ContainsKey source then
            failwithf "The source node %O of the edge does not exist in this graph." source
        if not <| nodes.ContainsKey target then
            failwithf "The target node %O of the edge does not exist in this graph." target        

        match adjComponents.ContainsKey(source) with
        | true  -> 
            let adjComponent = adjComponents.[source]
            Dictionary.addOrUpdateInPlace target data adjComponent |> ignore
            Dictionary.addOrUpdateInPlace source adjComponent adjComponents |> ignore
        | false -> 
            let adjComponent = Dictionary<'NodeKey, 'EdgeData>()
            Dictionary.addOrUpdateInPlace target data adjComponent |> ignore
            Dictionary.addOrUpdateInPlace source adjComponent adjComponents |> ignore
        this    
        

    //Get edge
    member this.GetEdgeByKeys (source : 'NodeKey) (target : 'NodeKey) =
        match adjComponents.ContainsKey source with
        | true  -> 
            match adjComponents.[source].ContainsKey target with 
            | true  -> 
                source, target, adjComponents.[source].[target]
            | false ->
                failwithf "The target node %O of the edge does not exist in this graph." target 
        | false -> 
            failwithf "The source node %O of the edge does not exist in this graph." source


    //##################
    // static members

    // Creates an empty AdjacencyGraph
    static member create<'NodeKey, 'NodeData, 'EdgeData when 'NodeKey : comparison>() : AdjGraph<'NodeKey, 'NodeData, 'EdgeData>  =
        AdjGraph()

    /// Adds node to graph [if node exists it is updated]
    static member addNode (key:'NodeKey) (data:'NodeData) (g : AdjGraph<'NodeKey, 'NodeData, 'EdgeData>) =
        g.AddNode key data

    //Adds a labeled, edge to the graph.
    static member addEdgeWithNodes (sourceKey : 'NodeKey) (source : 'NodeData)  
        (targetKey : 'NodeKey) (target : 'NodeData) (data : 'EdgeData) (g : AdjGraph<'NodeKey, 'NodeData, 'EdgeData>) =       
        g.AddEdgeWithNodes sourceKey source targetKey target data
        

    //Adds a labeled, edge to the graph.
    static member addEdge (source : 'NodeKey) (target : 'NodeKey) (data : 'EdgeData) (g : AdjGraph<'NodeKey, 'NodeData, 'EdgeData>) =
        g.AddEdge source target data

    //Get edge
    static member getEdgeByKeys (source : 'NodeKey) (target : 'NodeKey) (g : AdjGraph<'NodeKey, 'NodeData, 'EdgeData>) =
        g.GetEdgeByKeys source target
