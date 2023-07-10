namespace Graphoscope

open FSharpAux
open System.Collections.Generic

type AdjCompGraph<'NodeKey, 'NodeData, 'EdgeData when 'NodeKey : comparison> internal(adjComponents:Dictionary<'NodeKey, 'NodeData * Dictionary<'NodeKey, 'EdgeData>>) = 
        let adjComponents = adjComponents
        new () = AdjCompGraph(Dictionary<'NodeKey, 'NodeData * Dictionary<'NodeKey, 'EdgeData>>())                          

        /// Adds node to graph [if node exists it is updated]
        member this.AddNode (key:'NodeKey) (data:'NodeData)  =
            Dictionary.addOrUpdateInPlace key (data,Dictionary<'NodeKey,_>()) adjComponents |> ignore
            this

        //Adds a labeled, edge to the graph.
        member this.AddEdge (source : 'NodeKey) (target : 'NodeKey) (data : 'EdgeData) =
            match adjComponents.ContainsKey(source) with
            | true  -> 
                match adjComponents.ContainsKey(target) with
                | true  ->
                    let nodeData,adjComponent = adjComponents.[source]
                    Dictionary.addOrUpdateInPlace target data adjComponent |> ignore
                    adjComponents.[source] <- (nodeData,adjComponent)                
                | false -> 
                    failwithf "The target node %O of the edge does not exist in this graph." target                 
            | false -> 
                failwithf "The source node %O of the edge does not exist in this graph." source        

        //Adds a labeled, edge to the graph.
        member this.AddEdgeWithNodes (sourceKey : 'NodeKey) (source : 'NodeData)  (targetKey : 'NodeKey) (target : 'NodeData) (data : 'EdgeData) =            
            match adjComponents.ContainsKey(sourceKey) with
            | true  -> 
                    match adjComponents.ContainsKey(targetKey) with
                    | true  ->
                        let _,adjComponent = adjComponents.[sourceKey]
                        Dictionary.addOrUpdateInPlace targetKey data adjComponent |> ignore
                        adjComponents.[sourceKey] <- (source,adjComponent)
                        // potentially updata tagetData
                        let _,adjComponentTarget = adjComponents.[targetKey]
                        adjComponents.[targetKey] <- (target,adjComponentTarget)
                    | false ->
                        let _,adjComponent = adjComponents.[sourceKey]
                        Dictionary.addOrUpdateInPlace targetKey data adjComponent |> ignore
                        adjComponents.[sourceKey] <- (source,adjComponent)
                        // insert target node
                        Dictionary.addOrUpdateInPlace targetKey (target,Dictionary<'NodeKey,_>()) adjComponents |> ignore

            | false -> 
                let adjComponent = Dictionary<'NodeKey, 'EdgeData>()
                adjComponent.Add(targetKey, data)
                adjComponents.Add(sourceKey,(source,adjComponent))
                match adjComponents.ContainsKey(targetKey) with
                | true ->
                    // update target node
                    let _,adjComponent = adjComponents.[targetKey]
                    Dictionary.addOrUpdateInPlace targetKey (target,adjComponent) adjComponents |> ignore
                | false ->
                    // insert target node
                    Dictionary.addOrUpdateInPlace targetKey (target,Dictionary<'NodeKey,_>()) adjComponents |> ignore
                    
            this


        //Get edge
        member this.GetEdgeByKeys (source : 'NodeKey) (target : 'NodeKey) =
            match adjComponents.ContainsKey source with
            | true  -> 
                let _,sourceAdj = adjComponents.[source]
                match sourceAdj.ContainsKey target with 
                | true  -> 
                    source, target, sourceAdj.[target]
                | false ->
                    failwithf "The target node %O of the edge does not exist in this graph." target 
            | false -> 
                failwithf "The source node %O of the edge does not exist in this graph." source


        // Creates an empty AdjacencyGraph
        static member create<'NodeKey, 'NodeData, 'EdgeData when 'NodeKey : comparison>() =
            AdjCompGraph<'NodeKey, 'NodeData, 'EdgeData>()

        /// Adds node to graph [if node exists it is updated]
        static member addNode (key:'NodeKey) (data:'NodeData) (g : AdjCompGraph<'NodeKey, 'NodeData, 'EdgeData>) =
            g.AddNode key data


        //Adds a labeled, edge to the graph.
        static member addEdge(source : 'NodeKey) (target : 'NodeKey) (data : 'EdgeData) (g : AdjCompGraph<'NodeKey, 'NodeData, 'EdgeData>) =
            g.AddEdge source target data

           // Creates an empty AdjacencyGraph
        static member addEdgeWithNodes (sourceKey : 'NodeKey) (source : 'NodeData)  (targetKey : 'NodeKey) (target : 'NodeData) (data : 'EdgeData) (g : AdjCompGraph<'NodeKey, 'NodeData, 'EdgeData>) =
            g.AddEdgeWithNodes sourceKey  source  targetKey target data

           // Creates an empty AdjacencyGraph
        static member getEdgeByKeys (source : 'NodeKey) (target : 'NodeKey) (g : AdjCompGraph<'NodeKey, 'NodeData, 'EdgeData>) =
            g.GetEdgeByKeys source target
