namespace Graphoscope

open FSharpAux
open System.Collections.Generic


type MContext<'NodeKey, 'NodeData, 'EdgeData> when 'NodeKey: comparison =
     Dictionary<'NodeKey,'EdgeData> * 'NodeData * Dictionary<'NodeKey,'EdgeData>

type FGraph<'NodeKey,'NodeData,'EdgeData> when 'NodeKey: comparison =
    Dictionary<'NodeKey, MContext<'NodeKey, 'NodeData, 'EdgeData>>


module FGraph =
    ///Creates a new, empty graph.
    let empty<'NodeKey, 'NodeData, 'EdgeData when 'NodeKey: comparison> : FGraph<'NodeKey, 'NodeData, 'EdgeData> = 
        Dictionary<_,_>()

    let create<'NodeKey, 'NodeData, 'EdgeData when 'NodeKey: comparison>() : FGraph<'NodeKey, 'NodeData, 'EdgeData> =
        Dictionary<_,_>()

    module Nodes =
    
        ///Adds a labeled node to the graph.
        let add (nk:'NodeKey) (nd : 'NodeData)  (g : FGraph<'NodeKey, 'NodeData, 'EdgeData>) : FGraph<'NodeKey, 'NodeData, 'EdgeData> =
            Dictionary.addOrUpdateInPlace nk (Dictionary<_,_>(),nd,Dictionary<_,_>()) g |> ignore
            g


        ///Evaluates the number of nodes in the graph.
        let count (g : FGraph<'NodeKey, 'NodeData, 'EdgeData>) : int = 
            g.Count
 
        ///Returns true, if the node v is contained in the graph. Otherwise, it returns false.
        let contains vk (g : FGraph<'NodeKey, 'NodeData, 'EdgeData>) : bool =
            Dictionary.containsKey vk g

        ///Lookup a labeled vertex in the graph. Raising KeyNotFoundException if no binding exists in the graph.
        let find (n: 'NodeKey) (g : FGraph<'NodeKey, 'NodeData, 'EdgeData>) : ('NodeKey * 'NodeData) = 
            Dictionary.item n g
            |> fun (_, nd, _) -> n, nd
    


    ///Functions for edges of directed Graphs
    module Edges = 

        ///Returns true, if the edge from vertex v1 to vertex v2 is contained in the graph. Otherwise, it returns false.
        let contains v1 v2 (g: FGraph<'NodeKey,'NodeData,'EdgeData>) : bool =
            Dictionary.tryFind v1 g
            |> Option.bind (fun (_, _, s) -> Dictionary.tryFind v2 s)
            |> Option.isSome
    
        ///Lookup a labeled edge in the graph. Raising KeyNotFoundException if no binding exists in the graph.
        let find (v1:'NodeKey) (v2:'NodeKey) (g : FGraph<'NodeKey,'NodeData,'EdgeData>) : 'NodeKey * 'NodeKey * 'EdgeData =
                Dictionary.item v1 g
                |> fun (_, _, s) -> Dictionary.item v2 s
                |> fun e -> (v1,v2,e)
    
        ///Lookup a labeled edge in the graph, returning a Some value if a binding exists and None if not.
        let tryFind (nk1 : 'NodeKey) (nk2 : 'NodeKey) (g : FGraph<'NodeKey,'NodeData,'EdgeData>) : ('NodeKey * 'NodeKey * 'EdgeData) option =
                Dictionary.tryFind nk1 g
                |> Option.bind (fun (_, _, s) -> Dictionary.tryFind nk2 s)
                |> Option.map (fun e -> (nk1,nk2,e))

        //Add and remove

        ///Adds a labeled, directed edge to the graph.
        let tryAdd (nk1 : 'NodeKey) (nk2 : 'NodeKey) (ed : 'EdgeData) (g : FGraph<'NodeKey,'NodeData,'EdgeData>) : FGraph<'NodeKey,'NodeData,'EdgeData> option =
            if (Nodes.contains nk1 g |> not) || (Nodes.contains nk2 g |> not) || contains nk1 nk2 g then
                None
            else 
                let (p1, nd1, s1) = Dictionary.item nk1 g
                Dictionary.addOrUpdateInPlace nk2 ed s1 |> ignore
                let (p2, nd2, s2) = Dictionary.item nk2 g
                Dictionary.addOrUpdateInPlace nk1 ed p2 |> ignore
                g |> Some

        ///Adds a labeled, directed edge to the graph.
        let add (nk1 : 'NodeKey) (nk2 : 'NodeKey) (ed : 'EdgeData) (g : FGraph<'NodeKey,'NodeData,'EdgeData>) : FGraph<'NodeKey,'NodeData,'EdgeData> =
            //if Nodes.contains nk1 g |> not then failwithf "Source Node %O does not exist" nk1 
            //if Nodes.contains nk2 g |> not then failwithf "Target Node %O does not exist" nk2
            //if contains nk1 nk2 g then failwithf "An Edge between Source Node %O Target Node %O does already exist; FSharp.FGL does not allow for duplicate edges" nk1 nk2
            let (p1, nd1, s1) = Dictionary.item nk1 g
            Dictionary.addOrUpdateInPlace nk2 ed s1 |> ignore
            let (p2, nd2, s2) = Dictionary.item nk2 g
            Dictionary.addOrUpdateInPlace nk1 ed p2 |> ignore
            g

    module Converters =
        let toAdjacencyMatrix (g : FGraph<'NodeKey,'NodeData,float>): float [] [] =
            let nodeKeys = g.Keys |> Seq.toArray
            Array.init nodeKeys.Length (fun ri ->
                Array.init nodeKeys.Length (fun ci ->
                    if g.ContainsKey nodeKeys[ri] then
                        let (dict, _, _) = g[nodeKeys[ri]]
                        if dict.ContainsKey nodeKeys[ci] then
                            dict[nodeKeys[ci]]
                        else 0.
                    else
                        0.
                )
            )
