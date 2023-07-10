namespace Graphoscope

open FSharpAux
open System.Collections.Generic

type AdjGraph<'NodeKey, 'NodeData, 'EdgeData when 'NodeKey : comparison>  = {
    Nodes         : Dictionary<'NodeKey, 'NodeData>
    AdjComponents : Dictionary<'NodeKey, Dictionary<'NodeKey, 'EdgeData>>
}

    
    with 
        // Creates an empty AdjacencyGraph
        static member create<'NodeKey, 'NodeData, 'EdgeData when 'NodeKey : comparison>() =
            {
                Nodes         = Dictionary<'NodeKey, 'NodeData>()
                AdjComponents = Dictionary<'NodeKey, Dictionary<'NodeKey, 'EdgeData>>()
            }
        
        /// Adds node to graph [if node exists it is updated]
        static member AddNode (key:'NodeKey) (data:'NodeData) (g : AdjGraph<'NodeKey, 'NodeData, 'EdgeData>  ) =
            Dictionary.addOrUpdateInPlace key data g.Nodes |> ignore
            g

        //Adds a labeled, edge to the graph.
        static member AddEdgeWithNodes (sourceKey : 'NodeKey) (source : 'NodeData)  (targetKey : 'NodeKey) (target : 'NodeData) (data : 'EdgeData) (g : AdjGraph<'NodeKey, 'NodeData, 'EdgeData>  ) =
            Dictionary.addOrUpdateInPlace sourceKey source g.Nodes |> ignore
            Dictionary.addOrUpdateInPlace targetKey target g.Nodes |> ignore

            match g.AdjComponents.ContainsKey(sourceKey) with
            | true  -> 
                let adjComponent = g.AdjComponents.[sourceKey]
                Dictionary.addOrUpdateInPlace targetKey data adjComponent |> ignore
                Dictionary.addOrUpdateInPlace sourceKey adjComponent g.AdjComponents |> ignore
            | false -> 
                let adjComponent = Dictionary<'NodeKey, 'EdgeData>()
                Dictionary.addOrUpdateInPlace targetKey data adjComponent |> ignore
                Dictionary.addOrUpdateInPlace sourceKey adjComponent g.AdjComponents |> ignore
            
            g


        //Adds a labeled, edge to the graph.
        static member AddEdge (source : 'NodeKey) (target : 'NodeKey) (data : 'EdgeData) (g : AdjGraph<'NodeKey, 'NodeData, 'EdgeData>  ) =
            if not <| g.Nodes.ContainsKey source then
                failwithf "The source node %O of the edge does not exist in this graph." source
            if not <| g.Nodes.ContainsKey target then
                failwithf "The target node %O of the edge does not exist in this graph." target        

            match g.AdjComponents.ContainsKey(source) with
            | true  -> 
                let adjComponent = g.AdjComponents.[source]
                Dictionary.addOrUpdateInPlace target data adjComponent |> ignore
                Dictionary.addOrUpdateInPlace source adjComponent g.AdjComponents |> ignore
            | false -> 
                let adjComponent = Dictionary<'NodeKey, 'EdgeData>()
                Dictionary.addOrUpdateInPlace target data adjComponent |> ignore
                Dictionary.addOrUpdateInPlace source adjComponent g.AdjComponents |> ignore
            
            g

        //Get edge
        static member GetEdgeByKeys (source : 'NodeKey) (target : 'NodeKey) (g : AdjGraph<'NodeKey, 'NodeData, 'EdgeData>  ) =
            match g.AdjComponents.ContainsKey source with
            | true  -> 
                match g.AdjComponents.[source].ContainsKey target with 
                | true  -> 
                    source, target, g.AdjComponents.[source].[target]
                | false ->
                    failwithf "The target node %O of the edge does not exist in this graph." target 
            | false -> 
                failwithf "The source node %O of the edge does not exist in this graph." source

