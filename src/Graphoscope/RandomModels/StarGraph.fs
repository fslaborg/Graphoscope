namespace Graphoscope.RandomModels


open Graphoscope
open Graphoscope.Graphs

type StarGraph() =


            
    /// <summary> 
    /// Returns a StarGraph
    /// </summary>
    /// <param name="n">Specifies the number of nodes</param>
    /// <param name="nodeIDFunction">Function to create a NodeKey of an integer</param>
    /// <param name="nodeDataFunction">Function to create NodeData of NodeKeys</param>
    /// <param name="edgeDataFunction">Function to create EdgeData of NodeKeys</param>
    /// <returns>A Sequence to create a StarGraph</returns>
    /// 
    static member initStarGraphSeq (n: int) (nodeIDFunction:int -> 'NodeKey) (nodeDataFunction:'NodeKey -> 'NodeData) (edgeDataFunction:'NodeKey ->'NodeKey ->'EdgeData) :seq<'NodeKey * 'NodeData * 'NodeKey * 'NodeData * 'EdgeData> =

        let allNodes =
            Seq.init n (fun x -> nodeIDFunction x)
        
        let keyNodeID = 
            Seq.head allNodes
        
        let keyNodeData =
            nodeDataFunction keyNodeID

        allNodes
        |> Seq.tail
        |> Seq.map(fun nk2 -> 
            keyNodeID,
            keyNodeData,
            nk2,
            nodeDataFunction nk2,
            edgeDataFunction keyNodeID nk2
            )

    /// <summary> 
    /// Returns a StarGraph
    /// </summary>
    /// <param name="n">Specifies the number of nodes</param>
    /// <param name="nodeIDFunction">Function to create a NodeKey of an integer</param>
    /// <param name="nodeDataFunction">Function to create NodeData of NodeKeys</param>
    /// <param name="edgeDataFunction">Function to create EdgeData of NodeKeys</param>
    /// <returns>A StarFContextMap</returns>
    static member initStarFContextMap  (n: int) (nodeIDFunction:int -> 'NodeKey) (nodeDataFunction:'NodeKey -> 'NodeData) (edgeDataFunction:'NodeKey ->'NodeKey ->'EdgeData) :Directed.FContextMap<'NodeKey,'NodeData,'EdgeData> =
        let graphSeq = StarGraph.initStarGraphSeq n nodeIDFunction nodeDataFunction edgeDataFunction 
        Directed.FContextMap.ofSeq graphSeq
    
    /// <summary> 
    /// Returns a StarGraph
    /// </summary>
    /// <param name="n">Specifies the number of nodes</param>
    /// <param name="nodeIDFunction">Function to create a NodeKey of an integer</param>
    /// <param name="nodeDataFunction">Function to create NodeData of NodeKeys</param>
    /// <param name="edgeDataFunction">Function to create EdgeData of NodeKeys</param>
    /// <returns>A StarDirected.LilMatrix</returns>
    static member initStarLilMatrix  (n: int) (nodeIDFunction:int -> 'NodeKey) (nodeDataFunction:'NodeKey -> 'NodeData) (edgeDataFunction:'NodeKey ->'NodeKey ->'EdgeData) :Directed.LilMatrix<'NodeKey,'NodeData,'EdgeData> =
        let graphSeq = StarGraph.initStarGraphSeq n nodeIDFunction nodeDataFunction edgeDataFunction 
        Directed.LilMatrix.ofSeq graphSeq
    

