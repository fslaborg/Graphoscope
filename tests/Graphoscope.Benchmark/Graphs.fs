//namespace GraphBenchmarks
module Graphs

open System
open BenchmarkDotNet
open BenchmarkDotNet.Attributes

open Graphoscope

let rnd = new System.Random()

[<CustomComparison; CustomEquality>]
type DiNode =
    { NodeId : int; Data : int }
    interface IEquatable<DiNode> with
        member this.Equals other = other.NodeId.Equals this.NodeId

    override this.Equals other =
        match other with
        | :? DiNode as p -> (this :> IEquatable<_>).Equals p
        | _ -> false
    
    override this.GetHashCode () = this.NodeId.GetHashCode()

    interface IComparable with
        member this.CompareTo other =
            match other with
            | :? DiNode as n -> (this :> IComparable<_>).CompareTo n
            | _ -> -1

    interface IComparable<DiNode> with
        member this.CompareTo other = other.NodeId.CompareTo this.NodeId


[<MemoryDiagnoser>]
type Graphs () =
    let mutable edgesArr : (int*int*float) [] = [||]

    [<Params (10)>] 
    member val public NumberNodes = 0 with get, set

    [<Params (200)>] 
    member val public NumberEdges = 0 with get, set

    [<GlobalSetup>]
    member this.GlobalSetupData() =
        let edges = 
            [|
            for i=0 to this.NumberEdges-1 do
                let node1 = rnd.Next(0,this.NumberNodes-1)
                let node2 = rnd.Next(0,this.NumberNodes-1)
                yield (node1,node2,float i)
            |]
        edgesArr <- edges

 //   [<Benchmark>]
 //   member this.ArrayAdjacencyGraph () = 
 //       let g = ArrayAdjacencyGraph<int,int,float>()
 //       // Add nodes
 //       for i=0 to this.NumberNodes-1 do
 //           g.AddVertex(i, i) |> ignore
 //       // Add edges
 //       for i=0 to this.NumberEdges-1 do
 ////         let node1 = rnd.Next(0,this.NumberNodes-1)
 ////         let node2 = rnd.Next(0,this.NumberNodes-1)
 //           let (node1,node2,data) = edgesArr.[i]
 //           g.AddEdge(node1,node2,data) |> ignore
 //       [|
 //       for i=0 to this.NumberEdges-1 do
 //           let (node1,node2,_) = edgesArr.[i]
 //           let _,_,d = g.GetEdge(node1, node2)
 //           //printfn "%f" d
 //           yield d
 //       |] 

    //[<Benchmark>]
    //member this.DiGraph () =
    //    let g = DiGraph.create<int,float>()
    //     // Add nodes
    //    for i=0 to this.NumberNodes-1 do
    //        DiGraph.addNode (i) g
    //    // Add edges
    //    for i=0 to this.NumberEdges-1 do
    //        let node1 = rnd.Next(0,this.NumberNodes-1)
    //        let node2 = rnd.Next(0,this.NumberNodes-1)
    //        DiGraph.addEdge ((node1), (node2), float i) g 

    [<Benchmark>]
    member this.DiNodeGraph () =
        let g = DiGraph.create<DiNode>()
         // Add nodes
        for i=0 to this.NumberNodes-1 do
            DiGraph.addNode ({NodeId=i;Data=i}) g
        // Add edges
        for i=0 to this.NumberEdges-1 do
            let (node1,node2,data) = edgesArr.[i]
            DiGraph.addEdge ({NodeId=node1;Data=node1}, {NodeId=node2;Data=node2}, data) g 
        //[|
        //for i=0 to this.NumberEdges-1 do
        //    let (node1,node2,_) = edgesArr.[i]
        //    let _,_,d = DiGraph.find {NodeId=node1;Data=node1} {NodeId=node2;Data=node2} g
        //    //printfn "%f" d
        //    yield d
        //|] 

    //[<Benchmark>]
    //member this.FGraph () =
    //    let g = FGraph.create<int,int,float>()
    //     // Add nodes
    //    for i=0 to this.NumberNodes-1 do
    //        FGraph.Nodes.add i i g |> ignore
    //    // Add edges
    //    for i=0 to this.NumberEdges-1 do
    //        //let node1 = rnd.Next(0,this.NumberNodes-1)
    //        //let node2 = rnd.Next(0,this.NumberNodes-1)
    //    for i=0 to this.NumberEdges-1 do
    //        let (node1,node2,data) = edgesArr.[i]
    //        FGraph.Edges.add node1 node2 data g |> ignore
        
    //    [|
    //    for i=0 to this.NumberEdges-1 do
    //        let (node1,node2,_) = edgesArr.[i]
    //        let _,_,d = FGraph.Edges.find node1 node2 g
    //        yield d
    //    |] |> ignore
                   
