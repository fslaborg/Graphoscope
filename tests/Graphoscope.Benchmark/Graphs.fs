//namespace GraphBenchmarks
module Graphs

open System
open BenchmarkDotNet
open BenchmarkDotNet.Attributes

open Graphoscope

let rnd = new System.Random()



// [<MemoryDiagnoser>]
// type Graphs () =
//     let mutable edgesArr : (int*int*float) [] = [||]
//     let mutable UndirectedFContext     = Undirected.FContextMap.empty<int,int,float>
//     let mutable LilMatrix      = Directed.LilMatrix.empty<int,float>
//     // let mutable diNodeGraph  = Directed.LilMatrix.empty<DiNode<int>,float>
//     let mutable FContextMap       = FContextMap.empty<int,int,float>

//     [<Params (100, 10000)>] 
//     member val public NumberNodes = 0 with get, set

//     [<Params (500, 50000)>] 
//     member val public NumberEdges = 0 with get, set

//     [<GlobalSetup>]
//     member this.GlobalSetupData() =
//         // prepare edges
//         let edges = 
//             [|
//             for i=0 to this.NumberEdges-1 do
//                 let node1 = rnd.Next(0,this.NumberNodes-1)
//                 let node2 = rnd.Next(0,this.NumberNodes-1)
//                 yield (node1,node2,float i)
//             |]
//         edgesArr <- edges
//         //prepare UndirectedFContext
//         let gAdj= Undirected.FContextMap.create<int,int,float>()
//         for i=0 to this.NumberEdges-1 do
//             let (node1,node2,data) = edgesArr.[i]
//             Undirected.FContextMap.addElement node1 node1 node2 node2 data gAdj |> ignore
//         UndirectedFContext <- gAdj
//         //prepare LilMatrix
//         let gDi = Directed.LilMatrix.empty<int,float>
//         for i=0 to this.NumberNodes-1 do
//             Directed.LilMatrix.addNode i gDi
//         for i=0 to this.NumberEdges-1 do
//             let (node1,node2,data) = edgesArr.[i]
//             Directed.LilMatrix.addEdge (node1, node2, data) gDi
//         LilMatrix <- gDi
//         //prepare DiNodeGraph
//         let gDiNo = Directed.LilMatrix.empty<DiNode<int>,float>
//         for i=0 to this.NumberNodes-1 do
//             Directed.LilMatrix.addNode ({Id=i;Data=i}) gDiNo
//         for i=0 to this.NumberEdges-1 do
//             let (node1,node2,data) = edgesArr.[i]
//             Directed.LilMatrix.addEdge ({Id=node1;Data=node1}, {Id=node2;Data=node2}, data) gDiNo
//         diNodeGraph <- gDiNo
//         //prepare FContextMap
//         let gF = FContextMap.create<int,int,float>()
//         for i=0 to this.NumberNodes-1 do
//             FContextMap.addNode i i gF |> ignore
//         for i=0 to this.NumberEdges-1 do
//             let (node1,node2,data) = edgesArr.[i]
//             FContextMap.addEdge node1 node2 data gF |> ignore
//         FContextMap <- gF


//     [<Benchmark>]
//     member this.UndirectedFContext () = 
//         let g = Undirected.FContextMap.create<int,int,float>()
//         // Add nodes
//         for i=0 to this.NumberNodes-1 do
//             Undirected.FContextMap.Node.add i i g |> ignore
//         // Add edges
//         for i=0 to this.NumberEdges-1 do
//             let (node1,node2,data) = edgesArr.[i]
//             Undirected.FContextMap.Edge.add node1 node2 data g |> ignore
    
//     [<Benchmark>]
//     member this.LilMatrix () =
//         let g = Directed.LilMatrix.empty<int,float>
//          // Add nodes
//         for i=0 to this.NumberNodes-1 do
//             Directed.LilMatrix.addNode (i) g
//         // Add edges
//         for i=0 to this.NumberEdges-1 do
//             let (node1,node2,data) = edgesArr.[i]
//             Directed.LilMatrix.addEdge ((node1), (node2), float i) g 


//     [<Benchmark>]
//     member this.DiNodeGraph () =
//         let g = Directed.LilMatrix.empty<DiNode<int>,float>
//          // Add nodes
//         for i=0 to this.NumberNodes-1 do
//             Directed.LilMatrix.addNode ({Id=i;Data=i}) g
//         // Add edges
//         for i=0 to this.NumberEdges-1 do
//             let (node1,node2,data) = edgesArr.[i]
//             Directed.LilMatrix.addEdge ({Id=node1;Data=node1}, {Id=node2;Data=node2}, data) g 


//     [<Benchmark>]
//     member this.FContextMap () =
//         let g = FContextMap.create<int,int,float>()
//         for i=0 to this.NumberEdges-1 do
//             let (node1,node2,data) = edgesArr.[i]
//             FContextMap.addElement node1 node1 node2 node2 data g |> ignore
//         //// Add nodes
//         //for i=0 to this.NumberNodes-1 do
//         //    FContextMap.Node.add i i g |> ignore
//         //// Add edges
//         //for i=0 to this.NumberEdges-1 do
//         //    let (node1,node2,data) = edgesArr.[i]
//         //    FContextMap.Edge.add node1 node2 data g |> ignore
        


//     // ##############################################
//     // Access 

//     //[<Benchmark>]
//     //member this.Access_Adj () =     
//     //    [|
//     //    for i=0 to this.NumberEdges-1 do
//     //        let (node1,node2,_) = edgesArr.[i]
//     //        let _,_,d = Undirected.FContextMap.Edge.find node1 node2 UndirectedFContext            
//     //        yield d
//     //    |] 

//     //[<Benchmark>]
//     //member this.Access_Di () =  
//     //    [|
//     //    for i=0 to this.NumberEdges-1 do
//     //        let (node1,node2,_) = edgesArr.[i]
//     //        let _,_,d = Directed.LilMatrix.Edge.find node1 node2 LilMatrix
//     //        yield d
//     //    |] 

//     //[<Benchmark>]
//     //member this.Access_DiNode () =  
//     //     [|
//     //    for i=0 to this.NumberEdges-1 do
//     //        let (node1,node2,_) = edgesArr.[i]
//     //        let _,_,d = Directed.LilMatrix.Edge.find {Id=node1;Data=node1} {Id=node2;Data=node2} diNodeGraph
//     //        yield d
//     //    |] 

//     //[<Benchmark>]
//     //member this.Access_FContextMap () =  
//     //    [|
//     //    for i=0 to this.NumberEdges-1 do
//     //        let (node1,node2,_) = edgesArr.[i]
//     //        let _,_,d = FContextMap.Edge.find node1 node2 FContextMap
//     //        yield d
//     //    |] |> ignore




                   
// //// * Summary *

// //BenchmarkDotNet=v0.13.5, OS=Windows 11 (10.0.22621.1992/22H2/2022Update/SunValley2)
// //Intel Core i7-1065G7 CPU 1.30GHz, 1 CPU, 8 logical and 4 physical cores
// //.NET SDK=6.0.411
// //  [Host]     : .NET 6.0.19 (6.0.1923.31806), X64 RyuJIT AVX2 DEBUG
// //  DefaultJob : .NET 6.0.19 (6.0.1923.31806), X64 RyuJIT AVX2


// //|        Method | NumberNodes | NumberEdges |         Mean |      Error |       StdDev |       Median |      Gen0 |     Gen1 |     Gen2 |   Allocated |
// //|-------------- |------------ |------------ |-------------:|-----------:|-------------:|-------------:|----------:|---------:|---------:|------------:|
// //|      UndirectedFContext |         100 |         500 |     36.52 us |   0.727 us |     0.995 us |     36.42 us |   17.5171 |        - |        - |    71.64 KB |
// //|       AdjComp |         100 |         500 |     38.04 us |   0.638 us |     0.655 us |     38.31 us |   14.8926 |   0.2441 |        - |    60.89 KB |
// //|       LilMatrix |         100 |         500 |     15.80 us |   0.217 us |     0.193 us |     15.84 us |    9.9792 |   0.0305 |        - |    40.76 KB |
// //|   DiNodeGraph |         100 |         500 |     30.55 us |   0.592 us |     0.582 us |     30.44 us |   17.0898 |        - |        - |    70.01 KB |
// //|        FContextMap |         100 |         500 |     40.27 us |   0.804 us |     1.428 us |     39.95 us |   24.7803 |   0.1221 |        - |   101.35 KB |
// //|    Access_Adj |         100 |         500 |     23.86 us |   0.402 us |     0.356 us |     23.94 us |    6.8054 |        - |        - |    27.82 KB |
// //|   Access_Comp |         100 |         500 |     21.33 us |   0.421 us |     0.968 us |     21.48 us |    6.8054 |        - |        - |    27.82 KB |
// //|     Access_Di |         100 |         500 |     21.88 us |   0.429 us |     0.573 us |     21.96 us |    9.6741 |        - |        - |    39.54 KB |
// //| Access_DiNode |         100 |         500 |     39.21 us |   0.753 us |     1.433 us |     38.79 us |   16.3574 |        - |        - |    66.88 KB |
// //| Access_FContextMap |         100 |         500 |     15.16 us |   0.302 us |     0.790 us |     15.01 us |    6.8054 |        - |        - |    27.82 KB |
// //|      UndirectedFContext |         100 |       50000 |  3,013.35 us |  46.135 us |    43.155 us |  3,008.92 us |  500.0000 | 246.0938 |        - |  2561.04 KB |
// //|       AdjComp |         100 |       50000 |  3,162.62 us |  61.554 us |    82.172 us |  3,141.63 us |  195.3125 |  70.3125 |        - |  1002.56 KB |
// //|       LilMatrix |         100 |       50000 |  1,826.41 us |  29.391 us |    26.054 us |  1,829.23 us |  445.3125 | 222.6563 |        - |  2720.63 KB |
// //|   DiNodeGraph |         100 |       50000 |  5,599.60 us | 110.431 us |   118.160 us |  5,650.62 us |  914.0625 | 453.1250 |        - |  5070.45 KB |
// //|        FContextMap |         100 |       50000 |  3,312.77 us |  60.671 us |    53.783 us |  3,311.50 us |  359.3750 | 148.4375 |        - |  1984.74 KB |
// //|    Access_Adj |         100 |       50000 |  6,027.10 us | 166.613 us |   491.261 us |  6,032.00 us |  578.1250 | 187.5000 | 164.0625 |  2977.72 KB |
// //|   Access_Comp |         100 |       50000 |  5,605.19 us | 216.585 us |   638.606 us |  5,763.08 us |  578.1250 | 195.3125 | 164.0625 |  2977.73 KB |
// //|     Access_Di |         100 |       50000 | 11,669.94 us | 186.577 us |   222.107 us | 11,598.24 us |  859.3750 | 187.5000 | 171.8750 |  4149.85 KB |
// //| Access_DiNode |         100 |       50000 | 13,634.85 us | 262.426 us |   570.493 us | 13,488.59 us | 1531.2500 | 234.3750 | 171.8750 |  6884.08 KB |
// //| Access_FContextMap |         100 |       50000 |  4,651.53 us | 213.642 us |   629.927 us |  4,907.75 us |  578.1250 | 210.9375 | 164.0625 |  2977.67 KB |
// //|      UndirectedFContext |       10000 |         500 |  3,732.55 us | 124.537 us |   349.216 us |  3,781.20 us |  386.7188 | 324.2188 | 164.0625 |  2093.79 KB |
// //|       AdjComp |       10000 |         500 |    903.90 us |  19.537 us |    57.605 us |    906.83 us |  164.0625 | 106.4453 |  74.2188 |    807.8 KB |
// //|       LilMatrix |       10000 |         500 |  2,695.42 us |  88.026 us |   259.546 us |  2,682.65 us |  253.9063 | 222.6563 | 113.2813 |  1396.86 KB |
// //|   DiNodeGraph |       10000 |         500 |  4,177.83 us | 190.091 us |   560.488 us |  4,236.07 us |  367.1875 | 347.6563 | 191.4063 |  2045.24 KB |
// //|        FContextMap |       10000 |         500 |  5,451.31 us | 146.396 us |   431.653 us |  5,497.14 us |  546.8750 | 375.0000 | 187.5000 |  3003.15 KB |
// //|    Access_Adj |       10000 |         500 |     22.55 us |   0.433 us |     0.444 us |     22.47 us |    6.8054 |        - |        - |    27.82 KB |
// //|   Access_Comp |       10000 |         500 |     20.42 us |   0.401 us |     0.521 us |     20.37 us |    6.8054 |        - |        - |    27.82 KB |
// //|     Access_Di |       10000 |         500 |     19.38 us |   0.344 us |     0.305 us |     19.42 us |    9.6741 |        - |        - |    39.54 KB |
// //| Access_DiNode |       10000 |         500 |     35.03 us |   0.841 us |     2.412 us |     34.03 us |   16.3574 |        - |        - |    66.88 KB |
// //| Access_FContextMap |       10000 |         500 |     15.11 us |   0.252 us |     0.236 us |     15.14 us |    6.8054 |        - |        - |    27.82 KB |
// //|      UndirectedFContext |       10000 |       50000 | 21,790.54 us | 455.362 us | 1,342.645 us | 21,721.45 us | 1250.0000 | 531.2500 | 156.2500 |  7369.87 KB |
// //|       AdjComp |       10000 |       50000 | 14,915.89 us | 293.346 us |   605.811 us | 14,903.43 us |  906.2500 | 421.8750 | 125.0000 |   6134.5 KB |
// //|       LilMatrix |       10000 |       50000 | 11,529.53 us | 230.098 us |   660.194 us | 11,545.51 us |  656.2500 | 328.1250 |  93.7500 |  4040.71 KB |
// //|   DiNodeGraph |       10000 |       50000 | 23,243.74 us | 454.927 us | 1,026.847 us | 22,997.41 us | 1125.0000 | 468.7500 | 156.2500 |  7015.79 KB |
// //|        FContextMap |       10000 |       50000 | 29,492.95 us | 587.841 us | 1,290.326 us | 29,455.82 us | 1937.5000 | 937.5000 | 343.7500 | 10451.67 KB |
// //|    Access_Adj |       10000 |       50000 |  4,962.23 us | 128.893 us |   380.044 us |  5,033.85 us |  468.7500 |  93.7500 |  62.5000 |  2977.65 KB |
// //|   Access_Comp |       10000 |       50000 |  5,289.03 us | 138.180 us |   391.995 us |  5,315.84 us |  468.7500 |  93.7500 |  62.5000 |  2977.65 KB |
// //|     Access_Di |       10000 |       50000 |  6,017.33 us | 119.178 us |   305.498 us |  5,965.08 us |  765.6250 |  93.7500 |  70.3125 |  4149.48 KB |
// //| Access_DiNode |       10000 |       50000 |  7,690.06 us | 175.736 us |   515.404 us |  7,745.51 us | 1421.8750 | 125.0000 |  62.5000 |  6883.86 KB |
// //| Access_FContextMap |       10000 |       50000 |  5,599.11 us | 132.537 us |   384.515 us |  5,477.96 us |  468.7500 |  93.7500 |  62.5000 |  2977.65 KB |

// //// * Warnings *
// //MultimodalDistribution
// //  Graphs.Access_Adj: Default    -> It seems that the distribution is bimodal (mValue = 3.84)
// //  Graphs.FContextMap: Default        -> It seems that the distribution is bimodal (mValue = 4)
// //  Graphs.Access_Adj: Default    -> It seems that the distribution is bimodal (mValue = 3.3)
// //  Graphs.Access_DiNode: Default -> It seems that the distribution can have several modes (mValue = 3.13)
// //  Graphs.Access_FContextMap: Default -> It seems that the distribution is bimodal (mValue = 3.22)

// //// * Hints *
// //Outliers
// //  Graphs.UndirectedFContext: Default      -> 1 outlier  was  removed (39.24 us)
// //  Graphs.AdjComp: Default       -> 2 outliers were removed (40.00 us, 41.82 us)
// //  Graphs.LilMatrix: Default       -> 1 outlier  was  removed (16.39 us)
// //  Graphs.FContextMap: Default        -> 3 outliers were removed (44.57 us..47.00 us)
// //  Graphs.Access_Adj: Default    -> 1 outlier  was  removed (25.79 us)
// //  Graphs.Access_Comp: Default   -> 5 outliers were removed (24.32 us..41.47 us)
// //  Graphs.Access_DiNode: Default -> 1 outlier  was  removed (43.12 us)
// //  Graphs.Access_FContextMap: Default -> 2 outliers were removed (19.29 us, 23.24 us)
// //  Graphs.UndirectedFContext: Default      -> 2 outliers were detected (2.92 ms, 2.94 ms)
// //  Graphs.AdjComp: Default       -> 1 outlier  was  removed (3.98 ms)
// //  Graphs.LilMatrix: Default       -> 1 outlier  was  removed, 2 outliers were detected (1.77 ms, 1.91 ms)
// //  Graphs.FContextMap: Default        -> 1 outlier  was  removed (3.50 ms)
// //  Graphs.Access_Comp: Default   -> 4 outliers were detected (3.57 ms..4.02 ms)
// //  Graphs.Access_Di: Default     -> 3 outliers were removed (12.48 ms..12.75 ms)
// //  Graphs.Access_DiNode: Default -> 4 outliers were removed (15.24 ms..16.11 ms)
// //  Graphs.Access_FContextMap: Default -> 4 outliers were detected (2.59 ms..3.07 ms)
// //  Graphs.UndirectedFContext: Default      -> 9 outliers were removed, 12 outliers were detected (2.60 ms..2.83 ms, 4.65 ms..5.07 ms)
// //  Graphs.AdjComp: Default       -> 1 outlier  was  detected (712.87 us)
// //  Graphs.LilMatrix: Default       -> 2 outliers were detected (1.79 ms, 2.01 ms)
// //  Graphs.FContextMap: Default        -> 1 outlier  was  detected (4.17 ms)
// //  Graphs.Access_Comp: Default   -> 1 outlier  was  removed (22.26 us)
// //  Graphs.Access_Di: Default     -> 1 outlier  was  removed (20.12 us)
// //  Graphs.Access_DiNode: Default -> 5 outliers were removed (43.48 us..49.15 us)
// //  Graphs.Access_FContextMap: Default -> 1 outlier  was  removed (15.78 us)
// //  Graphs.LilMatrix: Default       -> 1 outlier  was  removed, 2 outliers were detected (9.71 ms, 13.58 ms)
// //  Graphs.DiNodeGraph: Default   -> 2 outliers were removed (26.09 ms, 27.42 ms)
// //  Graphs.Access_Comp: Default   -> 7 outliers were removed, 13 outliers were detected (4.30 ms..4.54 ms, 6.41 ms..9.17 ms)
// //  Graphs.Access_Di: Default     -> 2 outliers were removed (7.14 ms, 8.47 ms)
// //  Graphs.Access_DiNode: Default -> 1 outlier  was  removed (9.32 ms)
// //  Graphs.Access_FContextMap: Default -> 3 outliers were removed (6.66 ms..6.94 ms)

// //// * Legends *
// //  NumberNodes : Value of the 'NumberNodes' parameter
// //  NumberEdges : Value of the 'NumberEdges' parameter
// //  Mean        : Arithmetic mean of all measurements
// //  Error       : Half of 99.9% confidence interval
// //  StdDev      : Standard deviation of all measurements
// //  Median      : Value separating the higher half of all measurements (50th percentile)
// //  Gen0        : GC Generation 0 collects per 1000 operations
// //  Gen1        : GC Generation 1 collects per 1000 operations
// //  Gen2        : GC Generation 2 collects per 1000 operations
// //  Allocated   : Allocated memory per single operation (managed only, inclusive, 1KB = 1024B)
// //  1 us        : 1 Microsecond (0.000001 sec)