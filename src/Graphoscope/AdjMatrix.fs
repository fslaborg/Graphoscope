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