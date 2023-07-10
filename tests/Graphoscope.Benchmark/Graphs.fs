//namespace GraphBenchmarks
module Graphs

open System
open BenchmarkDotNet
open BenchmarkDotNet.Attributes

open Graphoscope

let rnd = new System.Random()



[<MemoryDiagnoser>]
type Graphs () =
    let mutable edgesArr : (int*int*float) [] = [||]
    let mutable adjGraph     = AdjGraph.create<int,int,float>()
    let mutable adjComp      = AdjCompGraph.create<int,int,float>()
    let mutable diGraph      = DiGraph.create<int,float>()
    let mutable diNodeGraph  = DiGraph.create<DiNode<int>,float>()

    [<Params (100)>] 
    member val public NumberNodes = 0 with get, set

    [<Params (500)>] 
    member val public NumberEdges = 0 with get, set

    [<GlobalSetup>]
    member this.GlobalSetupData() =
        // prepare edges
        let edges = 
            [|
            for i=0 to this.NumberEdges-1 do
                let node1 = rnd.Next(0,this.NumberNodes-1)
                let node2 = rnd.Next(0,this.NumberNodes-1)
                yield (node1,node2,float i)
            |]
        edgesArr <- edges
        //prepare AdjGraph
        let gAdj= AdjGraph.create<int,int,float>()
        for i=0 to this.NumberEdges-1 do
            let (node1,node2,data) = edgesArr.[i]
            AdjGraph.AddEdgeWithNodes node1 node1 node2 node2 data gAdj |> ignore
        adjGraph <- gAdj
        //prepare DiGraph
        let gComp= AdjCompGraph.create<int,int,float>()
        for i=0 to this.NumberEdges-1 do
            let (node1,node2,data) = edgesArr.[i]
            AdjCompGraph.addEdgeWithNodes node1 node1 node2 node2 data gComp |> ignore    
        adjComp <- gComp
        //prepare DiGraph
        let gDi = DiGraph.create<int,float>()
        for i=0 to this.NumberNodes-1 do
            DiGraph.addNode i gDi
        for i=0 to this.NumberEdges-1 do
            let (node1,node2,data) = edgesArr.[i]
            DiGraph.addEdge (node1, node2, data) gDi
        diGraph <- gDi
        //prepare DiNodeGraph
        let gDiNo = DiGraph.create<DiNode<int>,float>()
        for i=0 to this.NumberNodes-1 do
            DiGraph.addNode ({Id=i;Data=i}) gDiNo
        for i=0 to this.NumberEdges-1 do
            let (node1,node2,data) = edgesArr.[i]
            DiGraph.addEdge ({Id=node1;Data=node1}, {Id=node2;Data=node2}, data) gDiNo
        diNodeGraph <- gDiNo

    [<Benchmark>]
    member this.AdjGraph () = 
        let g = AdjCompGraph.create<int,int,float>()
        // Add nodes
        for i=0 to this.NumberNodes-1 do
            AdjCompGraph.addNode i i g |> ignore
        // Add edges
        for i=0 to this.NumberEdges-1 do
            let (node1,node2,data) = edgesArr.[i]
            AdjCompGraph.addEdge node1 node2 data g |> ignore
    
    [<Benchmark>]
    member this.AdjComp () = 
        let g = AdjGraph.create<int,int,float>()
        // Add nodes
        for i=0 to this.NumberNodes-1 do
            AdjGraph.AddNode i i g |> ignore
        // Add edges
        for i=0 to this.NumberEdges-1 do
            let (node1,node2,data) = edgesArr.[i]
            AdjGraph.AddEdge node1 node2 data g |> ignore

    [<Benchmark>]
    member this.DiGraph () =
        let g = DiGraph.create<int,float>()
         // Add nodes
        for i=0 to this.NumberNodes-1 do
            DiGraph.addNode (i) g
        // Add edges
        for i=0 to this.NumberEdges-1 do
            let (node1,node2,data) = edgesArr.[i]
            DiGraph.addEdge ((node1), (node2), float i) g 


    [<Benchmark>]
    member this.DiNodeGraph () =
        let g = DiGraph.create<DiNode<int>,float>()
         // Add nodes
        for i=0 to this.NumberNodes-1 do
            DiGraph.addNode ({Id=i;Data=i}) g
        // Add edges
        for i=0 to this.NumberEdges-1 do
            let (node1,node2,data) = edgesArr.[i]
            DiGraph.addEdge ({Id=node1;Data=node1}, {Id=node2;Data=node2}, data) g 


    // ##############################################
    // Access 

    [<Benchmark>]
    member this.Access_Adj () =     
        [|
        for i=0 to this.NumberEdges-1 do
            let (node1,node2,_) = edgesArr.[i]
            let _,_,d = AdjGraph.GetEdgeByKeys node1 node2 adjGraph            
            yield d
        |] 

    [<Benchmark>]
    member this.Access_Comp () =     
        [|
        for i=0 to this.NumberEdges-1 do
            let (node1,node2,_) = edgesArr.[i]
            let _,_,d = AdjCompGraph.getEdgeByKeys node1 node2 adjComp            
            yield d
        |] 

    [<Benchmark>]
    member this.Access_Di () =  
        [|
        for i=0 to this.NumberEdges-1 do
            let (node1,node2,_) = edgesArr.[i]
            let _,_,d = DiGraph.find node1 node2 diGraph
            yield d
        |] 

    [<Benchmark>]
    member this.Access_DiNode () =  
         [|
        for i=0 to this.NumberEdges-1 do
            let (node1,node2,_) = edgesArr.[i]
            let _,_,d = DiGraph.find {Id=node1;Data=node1} {Id=node2;Data=node2} diNodeGraph
            yield d
        |] 



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





                   
//// * Summary *

//BenchmarkDotNet=v0.13.5, OS=Windows 11 (10.0.22621.1848/22H2/2022Update/SunValley2)
//Intel Core i7-1065G7 CPU 1.30GHz, 1 CPU, 8 logical and 4 physical cores
//.NET SDK=6.0.411
//  [Host]     : .NET 6.0.19 (6.0.1923.31806), X64 RyuJIT AVX2 DEBUG
//  DefaultJob : .NET 6.0.19 (6.0.1923.31806), X64 RyuJIT AVX2


//|        Method | NumberNodes | NumberEdges |         Mean |      Error |       StdDev |       Median |      Gen0 |      Gen1 |     Gen2 |   Allocated |
//|-------------- |------------ |------------ |-------------:|-----------:|-------------:|-------------:|----------:|----------:|---------:|------------:|
//|      AdjGraph |         100 |         500 |     39.87 us |   0.758 us |     0.745 us |     39.89 us |   15.0757 |         - |        - |    61.71 KB |
//|       DiGraph |         100 |         500 |     15.77 us |   0.220 us |     0.205 us |     15.85 us |    9.9792 |    0.0305 |        - |    40.76 KB |
//|   DiNodeGraph |         100 |         500 |     69.47 us |   1.063 us |     1.917 us |     69.38 us |   33.3252 |         - |        - |   136.49 KB |
//|    Access_Adj |         100 |         500 |     21.99 us |   0.151 us |     0.141 us |     22.02 us |    6.8054 |         - |        - |    27.82 KB |
//|     Access_Di |         100 |         500 |     21.04 us |   0.242 us |     0.227 us |     21.11 us |    9.6741 |         - |        - |    39.54 KB |
//| Access_DiNode |         100 |         500 |     34.79 us |   0.560 us |     0.524 us |     34.76 us |   16.3574 |         - |        - |    66.88 KB |
//|      AdjGraph |         100 |       15000 |  1,005.77 us |  15.538 us |    12.975 us |  1,005.44 us |  101.5625 |   33.2031 |        - |   464.25 KB |
//|       DiGraph |         100 |       15000 |    438.53 us |   8.240 us |     8.093 us |    440.22 us |  155.2734 |   73.7305 |        - |   883.48 KB |
//|   DiNodeGraph |         100 |       15000 |  4,805.10 us |  89.313 us |    83.544 us |  4,817.04 us |  578.1250 |  179.6875 |  70.3125 |  3613.01 KB |
//|    Access_Adj |         100 |       15000 |    935.71 us |  12.347 us |    11.550 us |    939.94 us |  153.3203 |   76.1719 |  76.1719 |   842.37 KB |
//|     Access_Di |         100 |       15000 |  2,551.61 us |  42.851 us |    37.986 us |  2,557.68 us |  242.1875 |  144.5313 |  74.2188 |  1193.95 KB |
//| Access_DiNode |         100 |       15000 |  2,964.08 us |  54.085 us |    50.591 us |  2,972.48 us |  457.0313 |  144.5313 |  74.2188 |  2014.27 KB |
//|      AdjGraph |         100 |       50000 |  3,109.47 us |  42.855 us |    40.087 us |  3,105.79 us |  195.3125 |   74.2188 |        - |  1002.57 KB |
//|       DiGraph |         100 |       50000 |  1,832.58 us |  33.454 us |    29.656 us |  1,832.86 us |  439.4531 |  218.7500 |        - |   2640.4 KB |
//|   DiNodeGraph |         100 |       50000 | 24,780.19 us | 461.483 us |   409.093 us | 24,685.74 us | 2031.2500 |  875.0000 | 218.7500 | 11906.59 KB |
//|    Access_Adj |         100 |       50000 |  5,862.83 us | 222.008 us |   654.596 us |  5,998.50 us |  609.3750 |  210.9375 | 195.3125 |  2977.69 KB |
//|     Access_Di |         100 |       50000 | 11,373.01 us |  64.965 us |    60.768 us | 11,383.54 us |  906.2500 |  250.0000 | 218.7500 |  4149.86 KB |
//| Access_DiNode |         100 |       50000 | 13,294.45 us | 252.749 us |   224.055 us | 13,323.98 us | 1578.1250 |  312.5000 | 218.7500 |  6884.23 KB |
//|      AdjGraph |        5000 |         500 |    212.58 us |   2.747 us |     2.569 us |    212.75 us |   83.2520 |   41.5039 |  41.5039 |   463.54 KB |
//|       DiGraph |        5000 |         500 |    288.26 us |   4.009 us |     3.750 us |    288.81 us |  124.5117 |   82.5195 |  41.5039 |   705.35 KB |
//|   DiNodeGraph |        5000 |         500 |    729.30 us |  11.918 us |    11.148 us |    732.29 us |  181.6406 |   90.8203 |  90.8203 |  1102.67 KB |
//|    Access_Adj |        5000 |         500 |     21.14 us |   0.160 us |     0.142 us |     21.13 us |    6.8054 |         - |        - |    27.82 KB |
//|     Access_Di |        5000 |         500 |     18.59 us |   0.203 us |     0.190 us |     18.55 us |    9.6741 |         - |        - |    39.54 KB |
//| Access_DiNode |        5000 |         500 |     31.94 us |   0.443 us |     0.414 us |     31.89 us |   16.3574 |         - |        - |    66.88 KB |
//|      AdjGraph |        5000 |       15000 |  3,931.44 us |  95.786 us |   280.924 us |  3,938.51 us |  367.1875 |  242.1875 | 117.1875 |  2215.92 KB |
//|       DiGraph |        5000 |       15000 |  1,566.07 us |  58.992 us |   170.205 us |  1,570.43 us |  271.4844 |  142.5781 |  35.1563 |  1474.92 KB |
//|   DiNodeGraph |        5000 |       15000 |  7,452.01 us | 147.108 us |   272.675 us |  7,475.64 us |  796.8750 |  507.8125 | 140.6250 |  4500.09 KB |
//|    Access_Adj |        5000 |       15000 |  1,196.56 us |  30.315 us |    87.950 us |  1,183.96 us |  205.0781 |   83.9844 |  60.5469 |   842.42 KB |
//|     Access_Di |        5000 |       15000 |  2,393.13 us |  60.462 us |   178.273 us |  2,380.34 us |  292.9688 |  101.5625 |  62.5000 |  1193.96 KB |
//| Access_DiNode |        5000 |       15000 |  2,501.99 us |  66.515 us |   196.120 us |  2,519.60 us |  496.0938 |  128.9063 |  66.4063 |  2014.24 KB |
//|      AdjGraph |        5000 |       50000 | 11,344.72 us | 226.624 us |   442.012 us | 11,262.83 us |  843.7500 |  421.8750 | 125.0000 |  5099.84 KB |
//|       DiGraph |        5000 |       50000 |  4,183.82 us |  78.697 us |   124.821 us |  4,173.46 us |  570.3125 |  289.0625 |  15.6250 |  3453.96 KB |
//|   DiNodeGraph |        5000 |       50000 | 25,719.56 us | 556.211 us | 1,640.001 us | 25,730.39 us | 2250.0000 | 1187.5000 | 343.7500 | 12986.73 KB |
//|    Access_Adj |        5000 |       50000 |  6,390.41 us | 280.909 us |   828.267 us |  6,534.14 us |  554.6875 |  156.2500 | 140.6250 |  2977.72 KB |
//|     Access_Di |        5000 |       50000 |  7,383.01 us | 147.213 us |   385.230 us |  7,395.52 us |  835.9375 |  187.5000 | 140.6250 |  4149.54 KB |
//| Access_DiNode |        5000 |       50000 |  7,778.97 us | 171.301 us |   499.693 us |  7,697.34 us | 1500.0000 |  218.7500 | 140.6250 |  6883.88 KB |
//|      AdjGraph |       10000 |         500 |    265.99 us |   4.656 us |     3.635 us |    265.12 us |  185.5469 |  127.4414 |  92.7734 |   807.56 KB |
//|       DiGraph |       10000 |         500 |  2,692.38 us | 131.785 us |   388.571 us |  2,800.23 us |  253.9063 |  226.5625 | 113.2813 |  1397.02 KB |
//|   DiNodeGraph |       10000 |         500 |  4,625.85 us | 199.456 us |   588.101 us |  4,772.57 us |  382.8125 |  367.1875 | 195.3125 |  2112.55 KB |
//|    Access_Adj |       10000 |         500 |     21.47 us |   0.367 us |     0.325 us |     21.33 us |    6.8054 |         - |        - |    27.82 KB |
//|     Access_Di |       10000 |         500 |     18.59 us |   0.214 us |     0.190 us |     18.62 us |    9.6741 |         - |        - |    39.54 KB |
//| Access_DiNode |       10000 |         500 |     32.77 us |   0.455 us |     0.403 us |     32.88 us |   16.3574 |         - |        - |    66.88 KB |
//|      AdjGraph |       10000 |       15000 |  5,858.71 us | 167.186 us |   490.329 us |  5,798.91 us |  507.8125 |  351.5625 | 164.0625 |  2891.35 KB |
//|       DiGraph |       10000 |       15000 |  4,782.71 us | 172.287 us |   507.993 us |  4,779.38 us |  414.0625 |  273.4375 | 132.8125 |  2264.35 KB |
//|   DiNodeGraph |       10000 |       15000 | 12,436.24 us | 247.216 us |   697.278 us | 12,463.15 us |  984.3750 |  703.1250 | 265.6250 |   5605.4 KB |
//|    Access_Adj |       10000 |       15000 |  1,874.33 us |  47.673 us |   140.565 us |  1,870.74 us |  191.4063 |   66.4063 |  46.8750 |    842.4 KB |
//|     Access_Di |       10000 |       15000 |  1,952.37 us |  39.855 us |   117.512 us |  1,953.10 us |  281.2500 |   85.9375 |  50.7813 |  1193.94 KB |
//| Access_DiNode |       10000 |       15000 |  2,156.49 us |  85.107 us |   250.941 us |  2,104.53 us |  480.4688 |   97.6563 |  50.7813 |  2014.24 KB |
//|      AdjGraph |       10000 |       50000 | 14,463.37 us | 286.971 us |   730.432 us | 14,333.20 us | 1000.0000 |  578.1250 | 218.7500 |  6131.47 KB |
//|       DiGraph |       10000 |       50000 | 10,240.19 us | 203.322 us |   366.631 us | 10,247.01 us |  640.6250 |  296.8750 |  93.7500 |  4038.14 KB |
//|   DiNodeGraph |       10000 |       50000 | 33,768.81 us | 701.078 us | 2,056.140 us | 33,599.65 us | 2333.3333 | 1333.3333 | 400.0000 | 13896.12 KB |
//|    Access_Adj |       10000 |       50000 |  6,389.62 us | 127.008 us |   286.678 us |  6,374.67 us |  531.2500 |  156.2500 | 125.0000 |  2977.73 KB |
//|     Access_Di |       10000 |       50000 |  6,746.24 us | 134.389 us |   292.150 us |  6,758.33 us |  828.1250 |  164.0625 | 132.8125 |   4149.5 KB |
//| Access_DiNode |       10000 |       50000 |  7,719.06 us | 229.402 us |   669.176 us |  7,528.84 us | 1500.0000 |  203.1250 | 140.6250 |  6883.88 KB |

//// * Warnings *
//MultimodalDistribution
//  Graphs.DiGraph: Default       -> It seems that the distribution is bimodal (mValue = 3.74)
//  Graphs.Access_DiNode: Default -> It seems that the distribution is bimodal (mValue = 3.38)
//  Graphs.Access_Adj: Default    -> It seems that the distribution is bimodal (mValue = 4.15)
//  Graphs.Access_DiNode: Default -> It seems that the distribution is bimodal (mValue = 3.5)
//  Graphs.DiGraph: Default       -> It seems that the distribution can have several modes (mValue = 2.85)
//  Graphs.AdjGraph: Default      -> It seems that the distribution is bimodal (mValue = 3.31)
//  Graphs.DiGraph: Default       -> It seems that the distribution is bimodal (mValue = 3.57)
//  Graphs.Access_DiNode: Default -> It seems that the distribution can have several modes (mValue = 2.97)

//// * Hints *
//Outliers
//  Graphs.DiNodeGraph: Default   -> 8 outliers were removed (75.44 us..83.80 us)
//  Graphs.Access_Adj: Default    -> 1 outlier  was  detected (21.64 us)
//  Graphs.AdjGraph: Default      -> 2 outliers were removed (1.05 ms, 1.05 ms)
//  Graphs.Access_Di: Default     -> 1 outlier  was  removed (2.69 ms)
//  Graphs.DiGraph: Default       -> 1 outlier  was  removed (1.98 ms)
//  Graphs.DiNodeGraph: Default   -> 1 outlier  was  removed (26.55 ms)
//  Graphs.Access_Adj: Default    -> 4 outliers were detected (3.67 ms..3.90 ms)
//  Graphs.Access_DiNode: Default -> 2 outliers were removed (14.09 ms, 14.17 ms)
//  Graphs.AdjGraph: Default      -> 1 outlier  was  detected (206.92 us)
//  Graphs.Access_Adj: Default    -> 1 outlier  was  removed (21.63 us)
//  Graphs.AdjGraph: Default      -> 1 outlier  was  removed, 3 outliers were detected (3.23 ms, 3.23 ms, 4.65 ms)
//  Graphs.DiGraph: Default       -> 4 outliers were removed, 5 outliers were detected (1.15 ms, 2.03 ms..519.98 ms)
//  Graphs.DiNodeGraph: Default   -> 3 outliers were removed (8.27 ms..9.33 ms)
//  Graphs.Access_Adj: Default    -> 3 outliers were removed (1.49 ms..2.13 ms)
//  Graphs.AdjGraph: Default      -> 2 outliers were removed (12.75 ms, 12.90 ms)
//  Graphs.DiGraph: Default       -> 3 outliers were removed, 5 outliers were detected (3.81 ms, 3.90 ms, 4.48 ms..4.55 ms)
//  Graphs.Access_DiNode: Default -> 2 outliers were removed (9.44 ms, 9.79 ms)
//  Graphs.AdjGraph: Default      -> 3 outliers were removed (288.16 us..290.62 us)
//  Graphs.DiNodeGraph: Default   -> 7 outliers were detected (3.10 ms..3.31 ms)
//  Graphs.Access_Adj: Default    -> 1 outlier  was  removed (22.99 us)
//  Graphs.Access_Di: Default     -> 1 outlier  was  removed (19.12 us)
//  Graphs.Access_DiNode: Default -> 1 outlier  was  removed (34.06 us)
//  Graphs.AdjGraph: Default      -> 1 outlier  was  removed (7.53 ms)
//  Graphs.DiNodeGraph: Default   -> 2 outliers were removed (14.46 ms, 14.65 ms)
//  Graphs.DiGraph: Default       -> 1 outlier  was  removed (11.80 ms)
//  Graphs.DiNodeGraph: Default   -> 1 outlier  was  removed (42.78 ms)
//  Graphs.Access_Adj: Default    -> 6 outliers were removed (7.44 ms..7.75 ms)
//  Graphs.Access_DiNode: Default -> 2 outliers were removed (9.85 ms, 11.18 ms)

//// * Legends *
//  NumberNodes : Value of the 'NumberNodes' parameter
//  NumberEdges : Value of the 'NumberEdges' parameter
//  Mean        : Arithmetic mean of all measurements
//  Error       : Half of 99.9% confidence interval
//  StdDev      : Standard deviation of all measurements
//  Median      : Value separating the higher half of all measurements (50th percentile)
//  Gen0        : GC Generation 0 collects per 1000 operations
//  Gen1        : GC Generation 1 collects per 1000 operations
//  Gen2        : GC Generation 2 collects per 1000 operations
//  Allocated   : Allocated memory per single operation (managed only, inclusive, 1KB = 1024B)
//  1 us        : 1 Microsecond (0.000001 sec)