//namespace GraphBenchmarks
module Graphs

open System
open BenchmarkDotNet
open BenchmarkDotNet.Attributes

open Graphoscope

let rnd = new System.Random()


/////// NODEKEYS AS STRINGS


[<MemoryDiagnoser>]
type Graphs () =
    let mutable edgesArr : (string*string*float) [] = [||]
    let mutable adjGraph     = AdjGraph.empty<string,string,float>
    let mutable diGraph      = DiGraph.empty<string,string,float>
    let mutable diNodeGraph  = UndirectedGraph.empty<string,string,float>
    let mutable fGraph       = FGraph.empty<string,string,float>

    [<Params (100, 10000, 50000)>] 
    member val public NumberNodes = 0 with get, set

    [<Params (500, 50000, 250000)>] 
    member val public NumberEdges = 0 with get, set

    [<GlobalSetup>]
    member this.GlobalSetupData() =
        // prepare edges
        let edges = 
            [|
            for i=0 to this.NumberEdges-1 do
                let node1 = rnd.Next(0,this.NumberNodes-1)|>string
                let node2 = rnd.Next(0,this.NumberNodes-1)|>string
                yield (node1,node2,float i)
            |]
        edgesArr <- edges
        //prepare AdjGraph
        let gAdj= AdjGraph.create<string,string,float>()
        for i=0 to this.NumberNodes-1 do
            AdjGraph.addNode (i|>string) (i|>string) gAdj |> ignore
        for i=0 to this.NumberEdges-1 do
            let (node1,node2,data) = edgesArr.[i]
            AdjGraph.addElement node1 node1 node2 node2 data gAdj |> ignore
        adjGraph <- gAdj
        //prepare DiGraph
        let gDi = DiGraph.empty<string,string,float>
        for i=0 to this.NumberNodes-1 do
            DiGraph.addNode (i|>string) (i|>string) gDi|> ignore
        for i=0 to this.NumberEdges-1 do
            let (node1,node2,data) = edgesArr.[i]
            DiGraph.addElement node1 node1 node2 node2 data gDi|> ignore
        diGraph <- gDi
        //prepare DiNodeGraph
        let gDiNo = UndirectedGraph.empty<string,string,float>
        for i=0 to this.NumberNodes-1 do
            UndirectedGraph.addNode (i|>string) (i|>string) gDiNo|> ignore
        for i=0 to this.NumberEdges-1 do
            let (node1,node2,data) = edgesArr.[i]
            UndirectedGraph.addElement node1 node1 node2 node2 data gDiNo|> ignore
        diNodeGraph <- gDiNo
        //prepare FGraph
        let gF = FGraph.create<string,string,float>()
        for i=0 to this.NumberNodes-1 do
            FGraph.addNode (i|>string) (i|>string) gF |> ignore
        for i=0 to this.NumberEdges-1 do
            let (node1,node2,data) = edgesArr.[i]
            FGraph.addElement node1 node1 node2 node2 data gF |> ignore
        fGraph <- gF

    [<Benchmark>]
    member this.DiGraph () =
        let g = DiGraph.empty<string,string,float>
         // Add nodes
        for i=0 to this.NumberNodes-1 do
            DiGraph.addNode (i|>string) (i|>string)g|> ignore
        // Add edges
        for i=0 to this.NumberEdges-1 do
            let (node1,node2,data) = edgesArr.[i]
            //DiGraph.addElement ((node1),(node1), (node2),(node2), data) g |> ignore
            DiGraph.addElement node1 node1 node2 node2 data g|> ignore
        g

    [<Benchmark>]
    member this.UndirectedGraph () =
        let g = UndirectedGraph.empty<string,string,float>
         // Add nodes
        for i=0 to this.NumberNodes-1 do
            UndirectedGraph.addNode (i|>string) (i|>string) g |> ignore
        // Add edges
        for i=0 to this.NumberEdges-1 do
            let (node1,node2,data) = edgesArr.[i]
            //UndirectedGraph.addElement((node1),(node1), (node2),(node2), data) g|> ignore
            UndirectedGraph.addElement node1 node1 node2 node2 data g|> ignore
        g

    [<Benchmark>]
    member this.FGraph () =
        let g = FGraph.create<string,string,float>()
        // Add nodes
        for i=0 to this.NumberNodes-1 do
           FGraph.addNode (i|>string) (i|>string) g |> ignore
        // Add edges
        for i=0 to this.NumberEdges-1 do
            let (node1,node2,data) = edgesArr.[i]
            FGraph.addElement node1 node1 node2 node2 data g|> ignore
        g

    [<Benchmark>]
    member this.AdjGraph () = 
        let g = AdjGraph.create<string,string,float>()
        // Add nodes
        for i=0 to this.NumberNodes-1 do
            AdjGraph.addNode (i|>string) (i|>string) g |> ignore
        // Add edges
        for i=0 to this.NumberEdges-1 do
            let (node1,node2,data) = edgesArr.[i]
            AdjGraph.addElement node1 node1 node2 node2 data g |> ignore
        g


    // ##############################################
    // Access 

    [<Benchmark>]
    member this.Access_Di () =  
       [|
       for i=0 to this.NumberEdges-1 do
           let (node1,node2,_) = edgesArr.[i]
           //prstringfn "%i %i" node1 node2
           let _,_,d = 
                //try 
                DiGraph.find node1 node2 diGraph
                //with 
                //   | :? System.Collections.Generic.KeyNotFoundException ->  DiGraph.find node2 node1 diGraph
           yield d
       |] 

    [<Benchmark>]
    member this.Access_UndirectedGraph () =  
        [|
       for i=0 to this.NumberEdges-1 do
           let (node1,node2,_) = edgesArr.[i]
           //prstringfn "%i %i" node1 node2
           let _,_,d = 
                //try 
                    UndirectedGraph.find node1 node2 diNodeGraph
                //with 
                //   | :? System.Collections.Generic.KeyNotFoundException ->  UndirectedGraph.find node2 node1 diNodeGraph

           yield d
       |] 

    [<Benchmark>]
    member this.Access_FGraph () =  
       [|
       for i=0 to this.NumberEdges-1 do
           let (node1,node2,_) = edgesArr.[i]
           let _,_,d = FGraph.findEdge node1 node2 fGraph
           yield d
       |] 

    [<Benchmark>]
    member this.Access_Adj () =     
       [|
       for i=0 to this.NumberEdges-1 do
           let (node1,node2,_) = edgesArr.[i]
           let _,_,d = AdjGraph.Edge.find node1 node2 adjGraph            
           yield d
       |] 

    [<Benchmark>]
    member this.Djikstra_Di() =     
       Algorithms.Dijkstra.ofDiGraph "0" id diGraph

    [<Benchmark>]
    member this.Djikstra_UndirectedGraph() =     
       Algorithms.Dijkstra.ofUndirected "0" id diNodeGraph

    [<Benchmark>]
    member this.Djikstra_FGraph() =     
       Algorithms.Dijkstra.ofFGraph "0" id fGraph

    [<Benchmark>]
    member this.Djikstra_Adj() =     
       Algorithms.Dijkstra.ofAdjGraph "0" id adjGraph


//// * Summary *

//BenchmarkDotNet=v0.13.5, OS=Windows 10 (10.0.20348.2031)
//Intel Xeon Gold 6336Y CPU 2.40GHz, 2 CPU, 96 logical and 48 physical cores
//.NET SDK=6.0.407
//  [Host]     : .NET 6.0.18 (6.0.1823.26907), X64 RyuJIT AVX2 DEBUG
//  DefaultJob : .NET 6.0.18 (6.0.1823.26907), X64 RyuJIT AVX2


//|                   Method | NumberNodes | NumberEdges |          Mean |        Error |       StdDev |        Median |      Gen0 |      Gen1 |     Gen2 |   Allocated |
//|------------------------- |------------ |------------ |--------------:|-------------:|-------------:|--------------:|----------:|----------:|---------:|------------:|
//|                  DiGraph |         100 |         500 |     103.79 us |     1.625 us |     1.520 us |     103.06 us |    6.2256 |    1.0986 |        - |   114.59 KB |
//|          UndirectedGraph |         100 |         500 |     134.38 us |     2.661 us |     3.642 us |     134.23 us |    5.8594 |    0.9766 |        - |   109.31 KB |
//|                   FGraph |         100 |         500 |     111.20 us |     1.361 us |     1.273 us |     111.88 us |    8.0566 |    1.7090 |        - |   149.97 KB |
//|                 AdjGraph |         100 |         500 |     105.83 us |     0.427 us |     0.357 us |     105.76 us |    7.2021 |    1.4648 |        - |   134.53 KB |
//|                Access_Di |         100 |         500 |      51.59 us |     0.870 us |     0.814 us |      51.65 us |    2.3193 |         - |        - |    43.45 KB |
//|   Access_UndirectedGraph |         100 |         500 |      53.22 us |     0.833 us |     0.780 us |      53.22 us |    2.3193 |         - |        - |    43.45 KB |
//|            Access_FGraph |         100 |         500 |      41.34 us |     0.794 us |     0.882 us |      41.78 us |    1.7090 |         - |        - |    31.73 KB |
//|               Access_Adj |         100 |         500 |      49.68 us |     1.010 us |     2.977 us |      51.31 us |    1.7090 |         - |        - |    31.73 KB |
//|              Djikstra_Di |         100 |         500 |      32.63 us |     0.645 us |     0.945 us |      32.67 us |    0.8545 |         - |        - |    15.75 KB |
//| Djikstra_UndirectedGraph |         100 |         500 |      53.69 us |     0.633 us |     0.592 us |      54.01 us |    1.0376 |         - |        - |    19.27 KB |
//|          Djikstra_FGraph |         100 |         500 |      72.19 us |     1.436 us |     1.709 us |      72.72 us |    1.3428 |         - |        - |    25.84 KB |
//|             Djikstra_Adj |         100 |         500 |     198.17 us |     4.288 us |    12.644 us |     204.68 us |    6.3477 |    0.2441 |        - |   118.92 KB |
//|                  DiGraph |         100 |       50000 |  13,104.66 us |   272.510 us |   803.501 us |  13,490.76 us |  265.6250 |  109.3750 |        - |  5125.69 KB |
//|          UndirectedGraph |         100 |       50000 |  18,295.08 us |   358.910 us |   440.773 us |  18,225.11 us |  250.0000 |  125.0000 |        - |  4721.16 KB |
//|                   FGraph |         100 |       50000 |   8,673.95 us |    85.586 us |    75.870 us |   8,691.29 us |  312.5000 |  156.2500 |        - |  5880.02 KB |
//|                 AdjGraph |         100 |       50000 |   7,480.81 us |    41.585 us |    34.725 us |   7,488.64 us |  218.7500 |  101.5625 |        - |   4116.3 KB |
//|                Access_Di |         100 |       50000 |  18,609.88 us |   370.530 us | 1,014.319 us |  18,701.62 us |  312.5000 |  156.2500 | 156.2500 |  4540.39 KB |
//|   Access_UndirectedGraph |         100 |       50000 |  18,863.70 us |   397.040 us | 1,170.681 us |  19,095.09 us |  312.5000 |  156.2500 | 156.2500 |  4540.31 KB |
//|            Access_FGraph |         100 |       50000 |   9,956.11 us |   213.784 us |   630.348 us |   9,960.42 us |  265.6250 |  156.2500 | 156.2500 |  3368.51 KB |
//|               Access_Adj |         100 |       50000 |  11,258.24 us |   220.456 us |   380.276 us |  11,208.29 us |  281.2500 |  171.8750 | 171.8750 |  3368.42 KB |
//|              Djikstra_Di |         100 |       50000 |     360.19 us |     7.132 us |     6.671 us |     355.72 us |    1.9531 |         - |        - |    36.69 KB |
//| Djikstra_UndirectedGraph |         100 |       50000 |     334.21 us |     1.591 us |     1.328 us |     333.87 us |    1.4648 |         - |        - |    35.67 KB |
//|          Djikstra_FGraph |         100 |       50000 |   2,192.17 us |    43.677 us |    53.639 us |   2,188.84 us |         - |         - |        - |    55.15 KB |
//|             Djikstra_Adj |         100 |       50000 |   2,894.22 us |    59.299 us |   163.326 us |   2,970.64 us |   78.1250 |    3.9063 |        - |  1444.09 KB |
//|                  DiGraph |         100 |      250000 |  63,760.42 us | 1,264.378 us | 1,456.060 us |  63,898.06 us | 1125.0000 |  375.0000 |        - | 22315.41 KB |
//|          UndirectedGraph |         100 |      250000 |  69,500.88 us | 1,356.751 us | 1,764.159 us |  70,178.41 us | 1142.8571 |  571.4286 |        - | 21908.73 KB |
//|                   FGraph |         100 |      250000 |  42,488.05 us |   807.610 us |   961.402 us |  42,566.78 us | 1083.3333 |  500.0000 |        - |    21424 KB |
//|                 AdjGraph |         100 |      250000 |  38,867.67 us |   605.568 us |   566.449 us |  38,859.82 us |  846.1538 |  384.6154 |        - | 16554.31 KB |
//|                Access_Di |         100 |      250000 |  77,484.51 us | 1,338.877 us | 1,252.386 us |  77,957.76 us | 1000.0000 |  142.8571 | 142.8571 | 21677.34 KB |
//|   Access_UndirectedGraph |         100 |      250000 |  75,997.19 us | 1,513.219 us | 1,858.369 us |  76,583.00 us | 1000.0000 |  142.8571 | 142.8571 | 21674.98 KB |
//|            Access_FGraph |         100 |      250000 |  41,061.28 us |   819.599 us | 1,655.630 us |  40,800.35 us |  666.6667 |  166.6667 | 166.6667 | 15815.35 KB |
//|               Access_Adj |         100 |      250000 |  51,921.00 us | 1,037.901 us | 1,274.636 us |  51,903.61 us |  700.0000 |  200.0000 | 200.0000 | 15815.38 KB |
//|              Djikstra_Di |         100 |      250000 |     372.05 us |     7.195 us |     8.286 us |     373.61 us |    1.9531 |         - |        - |    36.92 KB |
//| Djikstra_UndirectedGraph |         100 |      250000 |     397.48 us |     6.035 us |     5.645 us |     395.77 us |    1.9531 |         - |        - |    40.36 KB |
//|          Djikstra_FGraph |         100 |      250000 |   2,038.12 us |    36.178 us |    33.841 us |   2,055.27 us |         - |         - |        - |    52.53 KB |
//|             Djikstra_Adj |         100 |      250000 |   3,677.56 us |    34.910 us |    29.151 us |   3,671.87 us |   78.1250 |    3.9063 |        - |   1464.7 KB |
//|                  DiGraph |       10000 |         500 |   8,383.41 us |   166.887 us |   369.811 us |   8,362.72 us |  250.0000 |  234.3750 | 140.6250 |  3310.54 KB |
//|          UndirectedGraph |       10000 |         500 |   6,764.15 us |   132.278 us |   261.105 us |   6,754.89 us |  210.9375 |  203.1250 | 125.0000 |  2741.18 KB |
//|                   FGraph |       10000 |         500 |   8,876.50 us |   174.832 us |   277.301 us |   8,804.57 us |  265.6250 |  187.5000 |  93.7500 |  4057.31 KB |
//|                 AdjGraph |       10000 |         500 |   7,454.53 us |   147.844 us |   234.496 us |   7,428.37 us |  218.7500 |  179.6875 |  93.7500 |  3105.75 KB |
//|                Access_Di |       10000 |         500 |      39.54 us |     0.193 us |     0.412 us |      39.43 us |    2.3193 |         - |        - |    43.45 KB |
//|   Access_UndirectedGraph |       10000 |         500 |      38.59 us |     0.168 us |     0.149 us |      38.57 us |    2.3193 |         - |        - |    43.45 KB |
//|            Access_FGraph |       10000 |         500 |      30.97 us |     0.090 us |     0.085 us |      30.98 us |    1.7090 |         - |        - |    31.73 KB |
//|               Access_Adj |       10000 |         500 |      45.17 us |     0.103 us |     0.096 us |      45.20 us |    1.7090 |         - |        - |    31.73 KB |
//|              Djikstra_Di |       10000 |         500 |     106.36 us |     0.627 us |     0.697 us |     106.17 us |   25.3906 |   11.9629 |        - |   469.11 KB |
//| Djikstra_UndirectedGraph |       10000 |         500 |     104.81 us |     0.525 us |     0.438 us |     104.82 us |   25.3906 |    8.5449 |        - |   469.19 KB |
//|          Djikstra_FGraph |       10000 |         500 |   1,112.49 us |    24.652 us |    72.301 us |   1,106.87 us |   48.8281 |   41.0156 |  37.1094 |    920.3 KB |
//|             Djikstra_Adj |       10000 |         500 |   1,056.42 us |    21.075 us |    59.442 us |   1,066.07 us |   46.8750 |   39.0625 |  35.1563 |   920.77 KB |
//|                  DiGraph |       10000 |       50000 |  65,516.72 us |   761.529 us |   712.335 us |  65,639.54 us |  687.5000 |  437.5000 | 125.0000 | 11689.13 KB |
//|          UndirectedGraph |       10000 |       50000 |  41,586.07 us |   752.723 us |   704.097 us |  41,593.59 us |  615.3846 |  384.6154 |  76.9231 | 11313.59 KB |
//|                   FGraph |       10000 |       50000 |  60,927.77 us | 1,186.966 us | 1,165.759 us |  61,084.14 us |  800.0000 |  500.0000 | 100.0000 | 15383.45 KB |
//|                 AdjGraph |       10000 |       50000 |  58,618.97 us |   680.202 us |   636.262 us |  58,626.99 us |  812.5000 |  500.0000 | 125.0000 | 13993.59 KB |
//|                Access_Di |       10000 |       50000 |  10,159.93 us |   175.102 us |   163.790 us |  10,136.66 us |  187.5000 |   31.2500 |  31.2500 |  4540.36 KB |
//|   Access_UndirectedGraph |       10000 |       50000 |  11,462.07 us |   124.056 us |   109.973 us |  11,468.52 us |  218.7500 |   46.8750 |  46.8750 |   4540.1 KB |
//|            Access_FGraph |       10000 |       50000 |   8,424.30 us |   162.411 us |   173.778 us |   8,374.31 us |  156.2500 |   46.8750 |  46.8750 |  3368.28 KB |
//|               Access_Adj |       10000 |       50000 |  17,175.08 us |   333.203 us |   311.678 us |  17,247.71 us |  156.2500 |   46.8750 |  46.8750 |  3368.27 KB |
//|              Djikstra_Di |       10000 |       50000 |   8,165.97 us |    22.080 us |    20.653 us |   8,164.88 us |   78.1250 |   31.2500 |        - |  1604.91 KB |
//| Djikstra_UndirectedGraph |       10000 |       50000 |  12,669.72 us |    28.238 us |    26.414 us |  12,670.71 us |  109.3750 |   46.8750 |        - |  2021.07 KB |
//|          Djikstra_FGraph |       10000 |       50000 |  15,921.54 us |    52.669 us |    49.267 us |  15,905.47 us |   93.7500 |   31.2500 |        - |  2500.44 KB |
//|             Djikstra_Adj |       10000 |       50000 |  41,209.43 us |   215.018 us |   201.128 us |  41,159.58 us |  600.0000 |  300.0000 |        - | 13023.76 KB |
//|                  DiGraph |       10000 |      250000 | 179,806.70 us | 1,738.431 us | 1,541.074 us | 179,422.62 us | 2333.3333 | 1000.0000 |        - |  46223.8 KB |
//|          UndirectedGraph |       10000 |      250000 | 193,360.16 us |   421.937 us |   394.680 us | 193,357.03 us | 2333.3333 | 1000.0000 |        - | 44909.46 KB |
//|                   FGraph |       10000 |      250000 | 132,347.03 us |   965.111 us |   855.545 us | 132,169.38 us | 3000.0000 | 1666.6667 |        - | 61608.08 KB |
//|                 AdjGraph |       10000 |      250000 | 129,315.60 us |   627.881 us |   556.600 us | 129,387.06 us | 3250.0000 | 1500.0000 |        - | 62153.07 KB |
//|                Access_Di |       10000 |      250000 |  72,014.98 us |   385.397 us |   360.501 us |  71,895.26 us |  857.1429 |         - |        - |  21674.8 KB |
//|   Access_UndirectedGraph |       10000 |      250000 |  87,863.42 us |   228.682 us |   202.721 us |  87,921.32 us |  833.3333 |         - |        - |  21674.8 KB |
//|            Access_FGraph |       10000 |      250000 |  40,463.92 us |   160.531 us |   142.307 us |  40,402.52 us |  461.5385 |         - |        - |  15815.3 KB |
//|               Access_Adj |       10000 |      250000 |  55,266.89 us |   199.396 us |   176.759 us |  55,216.67 us |  428.5714 |         - |        - | 15815.34 KB |
//|              Djikstra_Di |       10000 |      250000 |  23,956.47 us |   177.760 us |   157.580 us |  23,960.38 us |  125.0000 |   62.5000 |        - |  2680.85 KB |
//| Djikstra_UndirectedGraph |       10000 |      250000 |  36,774.38 us |   206.621 us |   172.538 us |  36,790.95 us |  142.8571 |   71.4286 |        - |  3213.69 KB |
//|          Djikstra_FGraph |       10000 |      250000 |  85,807.09 us |   880.284 us |   780.349 us |  85,817.89 us |  166.6667 |         - |        - |  4013.62 KB |
//|             Djikstra_Adj |       10000 |      250000 | 271,846.14 us | 2,083.431 us | 1,846.908 us | 271,262.78 us | 3500.0000 |  500.0000 |        - | 65953.53 KB |
//|                  DiGraph |       50000 |         500 |  34,900.99 us |    78.667 us |    69.736 us |  34,892.41 us |  562.5000 |  437.5000 | 187.5000 | 14440.63 KB |
//|          UndirectedGraph |       50000 |         500 |  26,948.79 us |   446.148 us |   417.327 us |  26,886.88 us |  468.7500 |  343.7500 | 187.5000 | 11853.25 KB |
//|                   FGraph |       50000 |         500 |  51,107.94 us |   864.158 us |   808.334 us |  50,893.71 us |  937.5000 |  562.5000 | 187.5000 | 18991.19 KB |
//|                 AdjGraph |       50000 |         500 |  28,276.80 us |    91.700 us |    85.777 us |  28,279.28 us |  687.5000 |  437.5000 | 125.0000 | 14296.59 KB |
//|                Access_Di |       50000 |         500 |      38.62 us |     0.078 us |     0.061 us |      38.64 us |    2.3193 |         - |        - |    43.45 KB |
//|   Access_UndirectedGraph |       50000 |         500 |      38.85 us |     0.078 us |     0.069 us |      38.87 us |    2.3193 |         - |        - |    43.45 KB |
//|            Access_FGraph |       50000 |         500 |      32.35 us |     0.096 us |     0.085 us |      32.36 us |    1.7090 |         - |        - |    31.73 KB |
//|               Access_Adj |       50000 |         500 |      46.70 us |     0.162 us |     0.151 us |      46.77 us |    1.7090 |         - |        - |    31.73 KB |
//|              Djikstra_Di |       50000 |         500 |   5,413.26 us |   106.384 us |   199.815 us |   5,456.24 us |  101.5625 |   70.3125 |  31.2500 |  2344.05 KB |
//| Djikstra_UndirectedGraph |       50000 |         500 |   5,356.18 us |   105.955 us |   199.009 us |   5,430.55 us |  101.5625 |   70.3125 |  31.2500 |  2344.05 KB |
//|          Djikstra_FGraph |       50000 |         500 |   5,445.22 us |   108.503 us |   313.055 us |   5,406.89 us |   78.1250 |   70.3125 |  70.3125 |  3977.02 KB |
//|             Djikstra_Adj |       50000 |         500 |   5,174.86 us |   102.038 us |   230.317 us |   5,124.90 us |   62.5000 |   54.6875 |  54.6875 |  3977.18 KB |
//|                  DiGraph |       50000 |       50000 |  52,269.07 us |   962.164 us |   803.450 us |  52,024.08 us |  833.3333 |  333.3333 |        - | 24065.44 KB |
//|          UndirectedGraph |       50000 |       50000 |  63,397.74 us |   969.197 us |   906.587 us |  63,206.61 us |  777.7778 |  444.4444 | 111.1111 | 20579.53 KB |
//|                   FGraph |       50000 |       50000 |  93,369.25 us | 1,319.957 us | 1,102.223 us |  93,194.62 us | 1666.6667 | 1000.0000 | 166.6667 | 31604.45 KB |
//|                 AdjGraph |       50000 |       50000 |  79,052.83 us | 1,523.520 us | 1,496.301 us |  78,551.29 us | 1250.0000 |  750.0000 | 125.0000 | 24791.66 KB |
//|                Access_Di |       50000 |       50000 |  10,493.29 us |    45.795 us |    38.241 us |  10,478.85 us |  187.5000 |   15.6250 |  15.6250 |  4540.09 KB |
//|   Access_UndirectedGraph |       50000 |       50000 |   9,885.34 us |    31.084 us |    29.076 us |   9,890.47 us |  156.2500 |         - |        - |   4540.1 KB |
//|            Access_FGraph |       50000 |       50000 |   8,890.77 us |    29.135 us |    24.329 us |   8,890.22 us |  125.0000 |   15.6250 |  15.6250 |  3368.22 KB |
//|               Access_Adj |       50000 |       50000 |   9,413.63 us |    20.583 us |    18.246 us |   9,419.23 us |   93.7500 |         - |        - |  3368.22 KB |
//|              Djikstra_Di |       50000 |       50000 |   4,538.86 us |    68.770 us |    76.437 us |   4,538.35 us |  101.5625 |   62.5000 |  23.4375 |  2344.75 KB |
//| Djikstra_UndirectedGraph |       50000 |       50000 |  30,512.52 us |   182.491 us |   161.773 us |  30,488.02 us |  250.0000 |  125.0000 |        - |   5664.7 KB |
//|          Djikstra_FGraph |       50000 |       50000 |   4,378.03 us |    82.515 us |   168.556 us |   4,349.07 us |   39.0625 |   31.2500 |  31.2500 |  3976.89 KB |
//|             Djikstra_Adj |       50000 |       50000 |   4,267.11 us |    83.407 us |   119.619 us |   4,265.28 us |   39.0625 |   31.2500 |  31.2500 |  3978.44 KB |
//|                  DiGraph |       50000 |      250000 | 244,749.80 us | 4,812.815 us | 6,587.835 us | 241,513.70 us | 2500.0000 | 1000.0000 |        - | 56821.66 KB |
//|          UndirectedGraph |       50000 |      250000 | 234,899.62 us | 4,679.867 us | 7,422.765 us | 238,870.65 us | 2500.0000 | 1500.0000 |        - | 55209.95 KB |
//|                   FGraph |       50000 |      250000 | 228,286.10 us | 2,054.677 us | 1,604.157 us | 227,663.70 us | 3500.0000 | 1500.0000 |        - | 76211.97 KB |
//|                 AdjGraph |       50000 |      250000 | 225,596.13 us |   567.205 us |   530.564 us | 225,433.83 us | 3333.3333 | 1666.6667 |        - |  69196.3 KB |
//|                Access_Di |       50000 |      250000 |  68,980.52 us |   241.834 us |   201.942 us |  68,991.08 us |  800.0000 |         - |        - | 21674.75 KB |
//|   Access_UndirectedGraph |       50000 |      250000 |  82,048.39 us |   600.903 us |   501.781 us |  81,897.74 us |  857.1429 |         - |        - | 21677.36 KB |
//|            Access_FGraph |       50000 |      250000 |  51,278.64 us |   172.266 us |   134.494 us |  51,300.69 us |  500.0000 |         - |        - | 15815.31 KB |
//|               Access_Adj |       50000 |      250000 |  70,633.44 us |   273.289 us |   242.264 us |  70,664.09 us |  428.5714 |         - |        - | 15815.34 KB |
//|              Djikstra_Di |       50000 |      250000 |  71,509.96 us |   331.123 us |   309.733 us |  71,548.75 us |  333.3333 |  166.6667 |        - |  7985.07 KB |
//| Djikstra_UndirectedGraph |       50000 |      250000 | 102,796.69 us |   395.515 us |   308.792 us | 102,733.07 us |  400.0000 |  200.0000 |        - | 10136.89 KB |
//|          Djikstra_FGraph |       50000 |      250000 | 130,020.83 us |   559.879 us |   467.524 us | 130,096.35 us |  250.0000 |         - |        - | 11868.21 KB |
//|             Djikstra_Adj |       50000 |      250000 | 347,477.68 us |   996.900 us |   883.726 us | 347,324.60 us | 3000.0000 | 1000.0000 |        - | 64501.77 KB |

//// * Warnings *
//MultimodalDistribution
//  Graphs.Djikstra_Adj: Default           -> It seems that the distribution can have several modes (mValue = 2.96)
//  Graphs.Access_UndirectedGraph: Default -> It seems that the distribution can have several modes (mValue = 2.81)
//  Graphs.Access_FGraph: Default          -> It seems that the distribution is bimodal (mValue = 3.25)
//  Graphs.AdjGraph: Default               -> It seems that the distribution can have several modes (mValue = 2.83)
//  Graphs.Djikstra_Adj: Default           -> It seems that the distribution can have several modes (mValue = 3.1)

//// * Hints *
//Outliers
//  Graphs.AdjGraph: Default                 -> 2 outliers were removed (107.72 us, 108.75 us)
//  Graphs.Access_Adj: Default               -> 5 outliers were detected (43.11 us..43.81 us)
//  Graphs.Djikstra_Di: Default              -> 1 outlier  was  detected (30.38 us)
//  Graphs.Djikstra_FGraph: Default          -> 3 outliers were removed, 5 outliers were detected (66.19 us, 69.39 us, 85.13 us..85.33 us)
//  Graphs.DiGraph: Default                  -> 23 outliers were detected (11.53 ms..11.86 ms)
//  Graphs.FGraph: Default                   -> 1 outlier  was  removed, 3 outliers were detected (8.45 ms, 8.57 ms, 9.22 ms)
//  Graphs.AdjGraph: Default                 -> 2 outliers were removed, 3 outliers were detected (7.41 ms, 7.57 ms, 7.62 ms)
//  Graphs.Access_Di: Default                -> 5 outliers were detected (15.35 ms..16.10 ms)
//  Graphs.Access_UndirectedGraph: Default   -> 5 outliers were detected (14.54 ms..16.12 ms)
//  Graphs.Access_Adj: Default               -> 4 outliers were removed (12.46 ms..13.30 ms)
//  Graphs.Djikstra_UndirectedGraph: Default -> 2 outliers were removed (341.83 us, 344.06 us)
//  Graphs.Djikstra_Adj: Default             -> 12 outliers were removed, 34 outliers were detected (2.57 ms..2.67 ms, 3.45 ms..3.65 ms)
//  Graphs.DiGraph: Default                  -> 2 outliers were detected (59.90 ms, 60.25 ms)
//  Graphs.AdjGraph: Default                 -> 1 outlier  was  detected (37.36 ms)
//  Graphs.Access_Di: Default                -> 1 outlier  was  detected (74.71 ms)
//  Graphs.Access_Adj: Default               -> 1 outlier  was  detected (48.90 ms)
//  Graphs.Djikstra_Adj: Default             -> 2 outliers were removed, 3 outliers were detected (3.61 ms, 3.79 ms, 3.87 ms)
//  Graphs.FGraph: Default                   -> 1 outlier  was  removed (9.70 ms)
//  Graphs.Access_Di: Default                -> 18 outliers were removed (50.79 us..54.94 us)
//  Graphs.Access_UndirectedGraph: Default   -> 1 outlier  was  removed (52.83 us)
//  Graphs.Djikstra_Di: Default              -> 6 outliers were removed (151.01 us..155.25 us)
//  Graphs.Djikstra_UndirectedGraph: Default -> 2 outliers were removed (150.76 us, 152.11 us)
//  Graphs.Djikstra_FGraph: Default          -> 1 outlier  was  removed (1.33 ms)
//  Graphs.Djikstra_Adj: Default             -> 1 outlier  was  removed, 3 outliers were detected (892.60 us, 893.69 us, 1.22 ms)
//  Graphs.DiGraph: Default                  -> 2 outliers were detected (63.61 ms, 64.55 ms)
//  Graphs.UndirectedGraph: Default          -> 2 outliers were detected (40.15 ms, 40.20 ms)
//  Graphs.FGraph: Default                   -> 1 outlier  was  removed (63.92 ms)
//  Graphs.Access_UndirectedGraph: Default   -> 1 outlier  was  removed, 3 outliers were detected (11.21 ms, 11.26 ms, 13.16 ms)
//  Graphs.Access_FGraph: Default            -> 2 outliers were removed (10.11 ms, 10.15 ms)
//  Graphs.Djikstra_Di: Default              -> 1 outlier  was  detected (8.12 ms)
//  Graphs.DiGraph: Default                  -> 1 outlier  was  removed (187.18 ms)
//  Graphs.FGraph: Default                   -> 1 outlier  was  removed (200.87 ms)
//  Graphs.AdjGraph: Default                 -> 1 outlier  was  removed (202.29 ms)
//  Graphs.Access_UndirectedGraph: Default   -> 1 outlier  was  removed (149.17 ms)
//  Graphs.Access_FGraph: Default            -> 1 outlier  was  removed (41.77 ms)
//  Graphs.Access_Adj: Default               -> 1 outlier  was  removed (93.20 ms)
//  Graphs.Djikstra_Di: Default              -> 1 outlier  was  removed (36.99 ms)
//  Graphs.Djikstra_UndirectedGraph: Default -> 2 outliers were removed (50.00 ms, 55.01 ms)
//  Graphs.Djikstra_FGraph: Default          -> 1 outlier  was  removed, 2 outliers were detected (84.23 ms, 89.12 ms)
//  Graphs.Djikstra_Adj: Default             -> 1 outlier  was  removed (423.80 ms)
//  Graphs.DiGraph: Default                  -> 1 outlier  was  removed (35.46 ms)
//  Graphs.Access_Di: Default                -> 3 outliers were removed, 5 outliers were detected (38.48 us, 38.53 us, 52.12 us..52.62 us)
//  Graphs.Access_UndirectedGraph: Default   -> 1 outlier  was  removed, 3 outliers were detected (38.72 us, 38.73 us, 44.15 us)
//  Graphs.Access_FGraph: Default            -> 1 outlier  was  removed (33.47 us)
//  Graphs.Djikstra_Di: Default              -> 1 outlier  was  removed, 6 outliers were detected (4.83 ms..4.96 ms, 5.70 ms)
//  Graphs.Djikstra_UndirectedGraph: Default -> 1 outlier  was  removed, 6 outliers were detected (4.78 ms..4.89 ms, 5.82 ms)
//  Graphs.Djikstra_FGraph: Default          -> 4 outliers were removed (6.41 ms..6.74 ms)
//  Graphs.Djikstra_Adj: Default             -> 3 outliers were removed (5.74 ms..5.97 ms)
//  Graphs.DiGraph: Default                  -> 2 outliers were removed (56.09 ms, 72.16 ms)
//  Graphs.FGraph: Default                   -> 2 outliers were removed (118.40 ms, 128.23 ms)
//  Graphs.AdjGraph: Default                 -> 5 outliers were removed (105.94 ms..113.30 ms)
//  Graphs.Access_Di: Default                -> 4 outliers were removed (11.64 ms..15.03 ms)
//  Graphs.Access_FGraph: Default            -> 2 outliers were removed (8.97 ms, 9.63 ms)
//  Graphs.Access_Adj: Default               -> 1 outlier  was  removed, 3 outliers were detected (9.36 ms, 9.39 ms, 9.45 ms)
//  Graphs.Djikstra_Di: Default              -> 6 outliers were removed (5.24 ms..5.55 ms)
//  Graphs.Djikstra_UndirectedGraph: Default -> 1 outlier  was  removed (45.40 ms)
//  Graphs.Djikstra_FGraph: Default          -> 2 outliers were removed (4.85 ms, 5.06 ms)
//  Graphs.DiGraph: Default                  -> 1 outlier  was  removed (390.22 ms)
//  Graphs.UndirectedGraph: Default          -> 5 outliers were removed, 12 outliers were detected (221.69 ms..223.88 ms, 268.17 ms..270.44 ms)
//  Graphs.FGraph: Default                   -> 3 outliers were removed (242.08 ms..352.34 ms)
//  Graphs.Access_Di: Default                -> 2 outliers were removed (69.81 ms, 71.91 ms)
//  Graphs.Access_UndirectedGraph: Default   -> 2 outliers were removed (102.68 ms, 103.80 ms)
//  Graphs.Access_FGraph: Default            -> 3 outliers were removed (52.03 ms..61.97 ms)
//  Graphs.Access_Adj: Default               -> 1 outlier  was  removed (71.45 ms)
//  Graphs.Djikstra_UndirectedGraph: Default -> 3 outliers were removed (111.20 ms..155.06 ms)
//  Graphs.Djikstra_FGraph: Default          -> 2 outliers were removed (192.20 ms, 198.45 ms)
//  Graphs.Djikstra_Adj: Default             -> 1 outlier  was  removed (355.74 ms)

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





/////// NODEKEYS AS INTS


//[<MemoryDiagnoser>]
//type Graphs () =
//    let mutable edgesArr : (int*int*float) [] = [||]
//    let mutable adjGraph     = AdjGraph.empty<int,int,float>
//    let mutable diGraph      = DiGraph.empty<int,int,float>
//    let mutable diNodeGraph  = UndirectedGraph.empty<int,int,float>
//    let mutable fGraph       = FGraph.empty<int,int,float>

//    [<Params (100, 10000, 50000)>] 
//    member val public NumberNodes = 0 with get, set

//    [<Params (500, 50000, 250000)>] 
//    member val public NumberEdges = 0 with get, set

//    [<GlobalSetup>]
//    member this.GlobalSetupData() =
//        // prepare edges
//        let edges = 
//            [|
//            for i=0 to this.NumberEdges-1 do
//                let node1 = rnd.Next(0,this.NumberNodes-1)
//                let node2 = rnd.Next(0,this.NumberNodes-1)
//                yield (node1,node2,float i)
//            |]
//        edgesArr <- edges
//        //prepare AdjGraph
//        let gAdj= AdjGraph.create<int,int,float>()
//        for i=0 to this.NumberNodes-1 do
//            AdjGraph.addNode (i) (i) gAdj |> ignore
//        for i=0 to this.NumberEdges-1 do
//            let (node1,node2,data) = edgesArr.[i]
//            AdjGraph.addElement node1 node1 node2 node2 data gAdj |> ignore
//        adjGraph <- gAdj
//        //prepare DiGraph
//        let gDi = DiGraph.empty<int,int,float>
//        for i=0 to this.NumberNodes-1 do
//            DiGraph.addNode (i) (i) gDi|> ignore
//        for i=0 to this.NumberEdges-1 do
//            let (node1,node2,data) = edgesArr.[i]
//            DiGraph.addElement node1 node1 node2 node2 data gDi|> ignore
//        diGraph <- gDi
//        //prepare DiNodeGraph
//        let gDiNo = UndirectedGraph.empty<int,int,float>
//        for i=0 to this.NumberNodes-1 do
//            UndirectedGraph.addNode (i) (i) gDiNo|> ignore
//        for i=0 to this.NumberEdges-1 do
//            let (node1,node2,data) = edgesArr.[i]
//            UndirectedGraph.addElement node1 node1 node2 node2 data gDiNo|> ignore
//        diNodeGraph <- gDiNo
//        //prepare FGraph
//        let gF = FGraph.create<int,int,float>()
//        for i=0 to this.NumberNodes-1 do
//            FGraph.addNode (i) (i) gF |> ignore
//        for i=0 to this.NumberEdges-1 do
//            let (node1,node2,data) = edgesArr.[i]
//            FGraph.addElement node1 node1 node2 node2 data gF |> ignore
//        fGraph <- gF

//    [<Benchmark>]
//    member this.DiGraph () =
//        let g = DiGraph.empty<int,int,float>
//         // Add nodes
//        for i=0 to this.NumberNodes-1 do
//            DiGraph.addNode (i) (i)g|> ignore
//        // Add edges
//        for i=0 to this.NumberEdges-1 do
//            let (node1,node2,data) = edgesArr.[i]
//            //DiGraph.addElement ((node1),(node1), (node2),(node2), data) g |> ignore
//            DiGraph.addElement node1 node1 node2 node2 data g|> ignore
//        g

//    [<Benchmark>]
//    member this.UndirectedGraph () =
//        let g = UndirectedGraph.empty<int,int,float>
//         // Add nodes
//        for i=0 to this.NumberNodes-1 do
//            UndirectedGraph.addNode (i) (i) g |> ignore
//        // Add edges
//        for i=0 to this.NumberEdges-1 do
//            let (node1,node2,data) = edgesArr.[i]
//            //UndirectedGraph.addElement((node1),(node1), (node2),(node2), data) g|> ignore
//            UndirectedGraph.addElement node1 node1 node2 node2 data g|> ignore
//        g

//    [<Benchmark>]
//    member this.FGraph () =
//        let g = FGraph.create<int,int,float>()
//        // Add nodes
//        for i=0 to this.NumberNodes-1 do
//           FGraph.addNode (i) (i) g |> ignore
//        // Add edges
//        for i=0 to this.NumberEdges-1 do
//            let (node1,node2,data) = edgesArr.[i]
//            FGraph.addElement node1 node1 node2 node2 data g|> ignore
//        g

//    [<Benchmark>]
//    member this.AdjGraph () = 
//        let g = AdjGraph.create<int,int,float>()
//        // Add nodes
//        for i=0 to this.NumberNodes-1 do
//            AdjGraph.addNode (i) (i) g |> ignore
//        // Add edges
//        for i=0 to this.NumberEdges-1 do
//            let (node1,node2,data) = edgesArr.[i]
//            AdjGraph.addElement node1 node1 node2 node2 data g |> ignore
//        g


//    // ##############################################
//    // Access 

//    [<Benchmark>]
//    member this.Access_Di () =  
//       [|
//       for i=0 to this.NumberEdges-1 do
//           let (node1,node2,_) = edgesArr.[i]
//           //prstringfn "%i %i" node1 node2
//           let _,_,d = 
//                //try 
//                DiGraph.find node1 node2 diGraph
//                //with 
//                //   | :? System.Collections.Generic.KeyNotFoundException ->  DiGraph.find node2 node1 diGraph
//           yield d
//       |] 

//    [<Benchmark>]
//    member this.Access_UndirectedGraph () =  
//        [|
//       for i=0 to this.NumberEdges-1 do
//           let (node1,node2,_) = edgesArr.[i]
//           //prstringfn "%i %i" node1 node2
//           let _,_,d = 
//                //try 
//                    UndirectedGraph.find node1 node2 diNodeGraph
//                //with 
//                //   | :? System.Collections.Generic.KeyNotFoundException ->  UndirectedGraph.find node2 node1 diNodeGraph

//           yield d
//       |] 

//    [<Benchmark>]
//    member this.Access_FGraph () =  
//       [|
//       for i=0 to this.NumberEdges-1 do
//           let (node1,node2,_) = edgesArr.[i]
//           let _,_,d = FGraph.findEdge node1 node2 fGraph
//           yield d
//       |] 

//    [<Benchmark>]
//    member this.Access_Adj () =     
//       [|
//       for i=0 to this.NumberEdges-1 do
//           let (node1,node2,_) = edgesArr.[i]
//           let _,_,d = AdjGraph.Edge.find node1 node2 adjGraph            
//           yield d
//       |] 

//    [<Benchmark>]
//    member this.Djikstra_Di() =     
//       Algorithms.Dijkstra.ofDiGraph 0 id diGraph

//    [<Benchmark>]
//    member this.Djikstra_UndirectedGraph() =     
//       Algorithms.Dijkstra.ofUndirected 0 id diNodeGraph

//    [<Benchmark>]
//    member this.Djikstra_FGraph() =     
//       Algorithms.Dijkstra.ofFGraph 0 id fGraph

//    [<Benchmark>]
//    member this.Djikstra_Adj() =     
//       Algorithms.Dijkstra.ofAdjGraph 0 id adjGraph


//// * Summary *

//BenchmarkDotNet=v0.13.5, OS=Windows 10 (10.0.20348.2031)
//Intel Xeon Gold 6336Y CPU 2.40GHz, 2 CPU, 96 logical and 48 physical cores
//.NET SDK=6.0.407
//  [Host]     : .NET 6.0.18 (6.0.1823.26907), X64 RyuJIT AVX2 DEBUG
//  DefaultJob : .NET 6.0.18 (6.0.1823.26907), X64 RyuJIT AVX2


//|                   Method | NumberNodes | NumberEdges |          Mean |        Error |        StdDev |        Median |      Gen0 |      Gen1 |     Gen2 |    Allocated |
//|------------------------- |------------ |------------ |--------------:|-------------:|--------------:|--------------:|----------:|----------:|---------:|-------------:|
//|                  DiGraph |         100 |         500 |      60.09 us |     1.167 us |      1.597 us |      60.18 us |    5.3711 |    0.9766 |        - |     99.41 KB |
//|          UndirectedGraph |         100 |         500 |      91.17 us |     1.750 us |      1.719 us |      90.75 us |    6.3477 |    0.9766 |        - |    116.73 KB |
//|                   FGraph |         100 |         500 |      93.36 us |     0.908 us |      0.805 us |      93.68 us |    9.1553 |    2.0752 |        - |    169.44 KB |
//|                 AdjGraph |         100 |         500 |      88.99 us |     1.335 us |      1.249 us |      89.86 us |    8.3008 |    1.5869 |        - |    153.86 KB |
//|                Access_Di |         100 |         500 |      34.62 us |     0.632 us |      0.493 us |      34.87 us |    2.1362 |         - |        - |     39.54 KB |
//|   Access_UndirectedGraph |         100 |         500 |      38.92 us |     0.774 us |      0.860 us |      38.88 us |    2.1362 |         - |        - |     39.54 KB |
//|            Access_FGraph |         100 |         500 |      23.43 us |     0.464 us |      0.587 us |      23.52 us |    1.4954 |         - |        - |     27.82 KB |
//|               Access_Adj |         100 |         500 |      32.51 us |     0.643 us |      0.902 us |      32.50 us |    1.4648 |         - |        - |     27.82 KB |
//|              Djikstra_Di |         100 |         500 |      34.53 us |     0.690 us |      0.677 us |      34.55 us |    0.8545 |         - |        - |     16.45 KB |
//| Djikstra_UndirectedGraph |         100 |         500 |      52.50 us |     0.742 us |      0.694 us |      52.39 us |    1.0986 |         - |        - |      20.2 KB |
//|          Djikstra_FGraph |         100 |         500 |     104.23 us |     1.159 us |      1.027 us |     104.68 us |    4.0283 |    0.1221 |        - |     75.27 KB |
//|             Djikstra_Adj |         100 |         500 |     274.41 us |     5.408 us |      5.311 us |     277.40 us |   12.6953 |    0.4883 |        - |    240.05 KB |
//|                  DiGraph |         100 |       50000 |   9,746.61 us |   193.522 us |    377.450 us |   9,899.22 us |  250.0000 |  109.3750 |        - |   4724.46 KB |
//|          UndirectedGraph |         100 |       50000 |  10,194.18 us |   203.041 us |    462.427 us |  10,435.07 us |  234.3750 |   93.7500 |        - |   4552.23 KB |
//|                   FGraph |         100 |       50000 |   5,748.21 us |    70.210 us |     62.239 us |   5,720.32 us |  445.3125 |  210.9375 |        - |   8219.62 KB |
//|                 AdjGraph |         100 |       50000 |   5,146.79 us |    87.722 us |     82.055 us |   5,184.26 us |  343.7500 |  156.2500 |        - |   6454.05 KB |
//|                Access_Di |         100 |       50000 |  16,323.60 us |   325.198 us |    700.024 us |  16,379.66 us |  421.8750 |  281.2500 | 281.2500 |   4149.75 KB |
//|   Access_UndirectedGraph |         100 |       50000 |  15,054.62 us |   317.760 us |    936.923 us |  14,934.71 us |  406.2500 |  281.2500 | 281.2500 |   4149.72 KB |
//|            Access_FGraph |         100 |       50000 |   7,830.93 us |   152.204 us |    241.411 us |   7,842.18 us |  335.9375 |  250.0000 | 250.0000 |   2977.85 KB |
//|               Access_Adj |         100 |       50000 |   8,307.49 us |   202.405 us |    596.796 us |   8,271.88 us |  328.1250 |  250.0000 | 250.0000 |   2977.75 KB |
//|              Djikstra_Di |         100 |       50000 |     389.20 us |     7.606 us |      8.759 us |     390.37 us |    1.9531 |         - |        - |     38.17 KB |
//| Djikstra_UndirectedGraph |         100 |       50000 |     349.33 us |     6.940 us |      7.127 us |     352.68 us |    1.9531 |         - |        - |     38.25 KB |
//|          Djikstra_FGraph |         100 |       50000 |   2,472.57 us |    19.422 us |     18.167 us |   2,479.94 us |  113.2813 |    7.8125 |        - |   2098.78 KB |
//|             Djikstra_Adj |         100 |       50000 |   3,425.73 us |    39.442 us |     36.894 us |   3,436.11 us |  179.6875 |   11.7188 |        - |   3367.32 KB |
//|                  DiGraph |         100 |      250000 |  63,876.07 us | 1,229.306 us |  1,207.343 us |  63,697.49 us | 1000.0000 |  500.0000 |        - |  20351.95 KB |
//|          UndirectedGraph |         100 |      250000 |  63,127.09 us | 1,236.281 us |  1,156.418 us |  63,639.01 us | 1000.0000 |  500.0000 |        - |   20177.3 KB |
//|                   FGraph |         100 |      250000 |  25,242.19 us |   237.775 us |    210.782 us |  25,277.59 us | 1781.2500 |  218.7500 |        - |  33141.79 KB |
//|                 AdjGraph |         100 |      250000 |  17,056.76 us |   339.208 us |    611.662 us |  17,253.30 us | 1531.2500 |   93.7500 |        - |   28267.5 KB |
//|                Access_Di |         100 |      250000 |  54,855.71 us | 1,083.139 us |  2,308.258 us |  54,947.48 us | 1111.1111 |  444.4444 | 444.4444 |  19722.54 KB |
//|   Access_UndirectedGraph |         100 |      250000 |  57,028.08 us | 1,127.718 us |  2,522.303 us |  57,037.22 us | 1181.8182 |  454.5455 | 454.5455 |   19722.1 KB |
//|            Access_FGraph |         100 |      250000 |  26,719.94 us |   532.404 us |  1,475.288 us |  26,595.43 us |  906.2500 |  500.0000 | 500.0000 |  13862.76 KB |
//|               Access_Adj |         100 |      250000 |  27,595.75 us |   550.527 us |  1,006.670 us |  27,553.17 us |  875.0000 |  500.0000 | 500.0000 |  13862.78 KB |
//|              Djikstra_Di |         100 |      250000 |     318.58 us |     4.292 us |      3.805 us |     320.38 us |    1.9531 |         - |        - |     36.22 KB |
//| Djikstra_UndirectedGraph |         100 |      250000 |     258.83 us |     5.133 us |      8.141 us |     261.68 us |    1.9531 |         - |        - |      37.7 KB |
//|          Djikstra_FGraph |         100 |      250000 |   2,687.18 us |    52.859 us |     51.915 us |   2,653.10 us |  113.2813 |    7.8125 |        - |   2141.43 KB |
//|             Djikstra_Adj |         100 |      250000 |   2,491.11 us |    49.017 us |     87.128 us |   2,521.52 us |  175.7813 |   11.7188 |        - |   3230.56 KB |
//|                  DiGraph |       10000 |         500 |   4,732.37 us |    92.792 us |    157.568 us |   4,733.58 us |  187.5000 |  179.6875 | 109.3750 |   2163.87 KB |
//|          UndirectedGraph |       10000 |         500 |   3,428.09 us |    68.356 us |    117.911 us |   3,419.30 us |  144.5313 |  132.8125 |  85.9375 |   1616.78 KB |
//|                   FGraph |       10000 |         500 |   8,415.94 us |   167.676 us |    429.818 us |   8,429.24 us |  281.2500 |  265.6250 | 132.8125 |   3455.18 KB |
//|                 AdjGraph |       10000 |         500 |   6,211.11 us |   123.851 us |    161.042 us |   6,221.66 us |  226.5625 |  203.1250 | 132.8125 |   2506.45 KB |
//|                Access_Di |       10000 |         500 |      31.56 us |     0.565 us |      0.529 us |      31.36 us |    2.1362 |         - |        - |     39.54 KB |
//|   Access_UndirectedGraph |       10000 |         500 |      32.29 us |     0.571 us |      0.506 us |      32.52 us |    2.1362 |         - |        - |     39.54 KB |
//|            Access_FGraph |       10000 |         500 |      18.15 us |     0.363 us |      0.725 us |      18.48 us |    1.4954 |         - |        - |     27.82 KB |
//|               Access_Adj |       10000 |         500 |      33.46 us |     0.655 us |      0.828 us |      33.57 us |    1.4648 |         - |        - |     27.82 KB |
//|              Djikstra_Di |       10000 |         500 |     136.83 us |     2.457 us |      2.298 us |     136.96 us |   25.3906 |    9.2773 |        - |    469.03 KB |
//| Djikstra_UndirectedGraph |       10000 |         500 |     139.22 us |     2.661 us |      3.552 us |     139.57 us |   25.3906 |    9.5215 |        - |    469.03 KB |
//|          Djikstra_FGraph |       10000 |         500 |   3,347.06 us |    65.621 us |    127.989 us |   3,386.78 us |  148.4375 |  125.0000 | 113.2813 |   1389.04 KB |
//|             Djikstra_Adj |       10000 |         500 |   3,324.70 us |    65.573 us |    150.666 us |   3,341.33 us |  148.4375 |  121.0938 | 113.2813 |   1389.24 KB |
//|                  DiGraph |       10000 |       50000 |  53,890.97 us |   297.174 us |    277.976 us |  53,896.07 us |  562.5000 |  312.5000 |  62.5000 |  10158.44 KB |
//|          UndirectedGraph |       10000 |       50000 |  61,420.86 us | 1,184.298 us |  1,539.922 us |  61,744.04 us |  700.0000 |  400.0000 | 100.0000 |  12126.66 KB |
//|                   FGraph |       10000 |       50000 |  56,563.55 us |   594.822 us |    556.397 us |  56,790.21 us | 1062.5000 |  625.0000 | 187.5000 |  17090.12 KB |
//|                 AdjGraph |       10000 |       50000 |  46,952.61 us |   544.475 us |    509.302 us |  46,916.93 us |  937.5000 |  562.5000 | 125.0000 |  15685.73 KB |
//|                Access_Di |       10000 |       50000 |  12,483.63 us |   151.614 us |    134.402 us |  12,518.93 us |  187.5000 |   46.8750 |  46.8750 |   4149.47 KB |
//|   Access_UndirectedGraph |       10000 |       50000 |  13,617.75 us |   174.351 us |    163.088 us |  13,592.41 us |  187.5000 |   46.8750 |  46.8750 |   4149.54 KB |
//|            Access_FGraph |       10000 |       50000 |   9,787.67 us |   192.561 us |    214.031 us |   9,737.86 us |  125.0000 |   46.8750 |  46.8750 |    2977.6 KB |
//|               Access_Adj |       10000 |       50000 |  13,940.65 us |   174.790 us |    163.499 us |  13,923.22 us |  125.0000 |   46.8750 |  46.8750 |   2977.62 KB |
//|              Djikstra_Di |       10000 |       50000 |  12,527.39 us |   202.342 us |    189.271 us |  12,561.35 us |   78.1250 |   31.2500 |        - |   1600.37 KB |
//| Djikstra_UndirectedGraph |       10000 |       50000 |  19,601.30 us |   330.285 us |    308.949 us |  19,699.09 us |   93.7500 |   31.2500 |        - |   2022.88 KB |
//|          Djikstra_FGraph |       10000 |       50000 |  22,983.54 us |   444.875 us |    546.347 us |  22,895.29 us |  375.0000 |  187.5000 |  31.2500 |   7171.69 KB |
//|             Djikstra_Adj |       10000 |       50000 |  46,964.88 us |   728.257 us |    645.581 us |  47,180.06 us | 1272.7273 |  545.4545 |        - |  24133.49 KB |
//|                  DiGraph |       10000 |      250000 | 276,886.29 us | 5,450.614 us |  5,832.096 us | 275,086.33 us | 2000.0000 | 1000.0000 |        - |  43131.95 KB |
//|          UndirectedGraph |       10000 |      250000 | 183,642.74 us | 3,418.422 us |  2,854.537 us | 183,829.20 us | 2500.0000 | 1000.0000 |        - |  53519.62 KB |
//|                   FGraph |       10000 |      250000 | 197,062.96 us | 3,760.350 us |  3,693.167 us | 198,282.08 us | 3750.0000 | 1750.0000 |        - |  72767.46 KB |
//|                 AdjGraph |       10000 |      250000 | 181,154.69 us | 1,884.448 us |  1,762.714 us | 181,133.50 us | 3666.6667 | 1666.6667 |        - |  73134.39 KB |
//|                Access_Di |       10000 |      250000 |  53,348.45 us |   342.982 us |    286.405 us |  53,209.22 us |  666.6667 |         - |        - |  19721.67 KB |
//|   Access_UndirectedGraph |       10000 |      250000 |  74,217.57 us |   590.707 us |    523.646 us |  74,180.08 us |  714.2857 |         - |        - |  19721.68 KB |
//|            Access_FGraph |       10000 |      250000 |  31,839.77 us |   530.714 us |    470.464 us |  31,822.25 us |  437.5000 |   62.5000 |  62.5000 |  13862.64 KB |
//|               Access_Adj |       10000 |      250000 |  81,784.16 us | 1,274.997 us |  1,192.633 us |  81,617.68 us |  375.0000 |         - |        - |  13862.25 KB |
//|              Djikstra_Di |       10000 |      250000 |  27,203.71 us |   725.159 us |  2,115.323 us |  28,085.22 us |  125.0000 |   62.5000 |        - |   2683.66 KB |
//| Djikstra_UndirectedGraph |       10000 |      250000 |  59,226.46 us |   334.264 us |    296.316 us |  59,182.23 us |  111.1111 |         - |        - |   3219.52 KB |
//|          Djikstra_FGraph |       10000 |      250000 |  92,113.46 us |   668.471 us |    592.582 us |  91,983.03 us | 2000.0000 |  833.3333 |        - |  39492.43 KB |
//|             Djikstra_Adj |       10000 |      250000 | 219,189.51 us | 2,219.889 us |  2,076.486 us | 220,067.03 us | 8000.0000 |  666.6667 |        - | 151423.09 KB |
//|                  DiGraph |       50000 |         500 |  18,361.96 us |   408.275 us |  1,203.809 us |  17,939.88 us |  406.2500 |  375.0000 | 218.7500 |   9152.44 KB |
//|          UndirectedGraph |       50000 |         500 |  11,878.58 us |   234.907 us |    500.606 us |  12,030.00 us |  296.8750 |  281.2500 | 171.8750 |   6588.38 KB |
//|                   FGraph |       50000 |         500 |  37,200.65 us |   614.455 us |    574.762 us |  37,201.31 us |  812.5000 |  500.0000 | 218.7500 |  15889.98 KB |
//|                 AdjGraph |       50000 |         500 |  26,009.22 us |   515.456 us |    955.431 us |  26,255.01 us |  562.5000 |  406.2500 | 187.5000 |  11193.86 KB |
//|                Access_Di |       50000 |         500 |      31.71 us |     0.536 us |      0.501 us |      31.95 us |    2.1362 |         - |        - |     39.54 KB |
//|   Access_UndirectedGraph |       50000 |         500 |      25.16 us |     0.500 us |      1.318 us |      25.64 us |    2.1362 |    0.0305 |        - |     39.54 KB |
//|            Access_FGraph |       50000 |         500 |      24.18 us |     0.482 us |      0.574 us |      24.18 us |    1.4954 |         - |        - |     27.82 KB |
//|               Access_Adj |       50000 |         500 |      24.51 us |     0.248 us |      0.193 us |      24.49 us |    1.4648 |         - |        - |     27.82 KB |
//|              Djikstra_Di |       50000 |         500 |   4,466.46 us |    87.963 us |    163.045 us |   4,452.76 us |  109.3750 |   70.3125 |  31.2500 |   2344.05 KB |
//| Djikstra_UndirectedGraph |       50000 |         500 |   4,455.75 us |    88.450 us |    188.493 us |   4,505.24 us |  109.3750 |   70.3125 |  31.2500 |   2344.05 KB |
//|          Djikstra_FGraph |       50000 |         500 |  10,729.46 us |   212.237 us |    512.576 us |  10,815.04 us |  296.8750 |  187.5000 | 171.8750 |    6320.8 KB |
//|             Djikstra_Adj |       50000 |         500 |  10,876.78 us |   213.139 us |    367.654 us |  10,913.49 us |  296.8750 |  187.5000 | 171.8750 |    6321.1 KB |
//|                  DiGraph |       50000 |       50000 |  69,985.37 us | 1,103.005 us |  1,031.751 us |  69,930.52 us |  750.0000 |  375.0000 | 125.0000 |  18389.59 KB |
//|          UndirectedGraph |       50000 |       50000 |  62,913.32 us | 1,058.446 us |    883.851 us |  63,040.03 us |  625.0000 |  250.0000 |        - |  17247.98 KB |
//|                   FGraph |       50000 |       50000 | 110,592.43 us | 1,701.557 us |  1,591.637 us | 111,175.62 us | 1500.0000 |  833.3333 | 166.6667 |  30808.41 KB |
//|                 AdjGraph |       50000 |       50000 |  75,961.29 us | 1,454.051 us |  1,674.487 us |  75,252.41 us | 1000.0000 |  428.5714 |        - |  23985.52 KB |
//|                Access_Di |       50000 |       50000 |   7,858.75 us |   155.816 us |    166.721 us |   7,916.20 us |  171.8750 |   31.2500 |  31.2500 |   4149.47 KB |
//|   Access_UndirectedGraph |       50000 |       50000 |   8,854.18 us |   170.494 us |    209.381 us |   8,929.75 us |  171.8750 |   31.2500 |  31.2500 |   4149.47 KB |
//|            Access_FGraph |       50000 |       50000 |  10,725.52 us |   207.689 us |    203.979 us |  10,766.23 us |  109.3750 |   31.2500 |  31.2500 |   2977.66 KB |
//|               Access_Adj |       50000 |       50000 |   8,281.25 us |   165.591 us |    242.721 us |   8,249.12 us |  109.3750 |   31.2500 |  31.2500 |   2977.64 KB |
//|              Djikstra_Di |       50000 |       50000 |   4,768.80 us |    94.045 us |    174.319 us |   4,776.27 us |  109.3750 |   78.1250 |  31.2500 |   2344.52 KB |
//| Djikstra_UndirectedGraph |       50000 |       50000 |   6,097.86 us |   113.535 us |    143.585 us |   6,116.94 us |  109.3750 |   78.1250 |  31.2500 |   2344.05 KB |
//|          Djikstra_FGraph |       50000 |       50000 |   9,407.53 us |   182.006 us |    194.744 us |   9,388.67 us |  203.1250 |   93.7500 |  78.1250 |   6320.68 KB |
//|             Djikstra_Adj |       50000 |       50000 |  50,669.78 us |   334.874 us |    279.635 us |  50,741.98 us | 1333.3333 |  500.0000 |        - |  29206.91 KB |
//|                  DiGraph |       50000 |      250000 | 228,401.25 us | 4,818.602 us | 14,132.126 us | 234,135.33 us | 2333.3333 | 1333.3333 |        - |  49579.27 KB |
//|          UndirectedGraph |       50000 |      250000 | 433,021.47 us | 5,115.414 us |  4,784.962 us | 432,521.15 us | 3000.0000 | 1500.0000 |        - |  59676.45 KB |
//|                   FGraph |       50000 |      250000 | 337,753.73 us | 2,431.497 us |  2,155.459 us | 337,596.12 us | 4000.0000 | 2000.0000 |        - |  84776.88 KB |
//|                 AdjGraph |       50000 |      250000 | 284,158.34 us | 3,842.865 us |  3,594.618 us | 283,935.10 us | 4000.0000 | 2000.0000 |        - |  77794.17 KB |
//|                Access_Di |       50000 |      250000 |  91,396.15 us |   397.935 us |    332.293 us |  91,328.97 us |  666.6667 |         - |        - |  19721.71 KB |
//|   Access_UndirectedGraph |       50000 |      250000 | 104,232.40 us |   814.159 us |    761.565 us | 104,414.82 us |  600.0000 |         - |        - |  19721.63 KB |
//|            Access_FGraph |       50000 |      250000 |  61,215.12 us |   319.523 us |    283.249 us |  61,266.96 us |  333.3333 |         - |        - |  13864.25 KB |
//|               Access_Adj |       50000 |      250000 |  87,954.28 us |   966.070 us |    903.663 us |  87,601.28 us |  333.3333 |         - |        - |  13862.33 KB |
//|              Djikstra_Di |       50000 |      250000 | 103,639.36 us |   761.113 us |    711.945 us | 103,628.18 us |  200.0000 |         - |        - |   7994.16 KB |
//| Djikstra_UndirectedGraph |       50000 |      250000 | 152,402.35 us | 1,073.828 us |  1,004.460 us | 152,136.73 us |  500.0000 |  250.0000 |        - |  10143.05 KB |
//|          Djikstra_FGraph |       50000 |      250000 | 179,779.56 us | 2,026.531 us |  1,796.467 us | 179,555.95 us | 1666.6667 |  666.6667 |        - |  35126.89 KB |
//|             Djikstra_Adj |       50000 |      250000 | 401,850.65 us | 6,116.557 us |  5,422.169 us | 399,933.65 us | 6000.0000 | 1000.0000 |        - | 120829.38 KB |

//// * Warnings *
//MultimodalDistribution
//  Graphs.Access_FGraph: Default -> It seems that the distribution can have several modes (mValue = 3.11)
//  Graphs.AdjGraph: Default      -> It seems that the distribution can have several modes (mValue = 2.92)
//  Graphs.Access_Adj: Default    -> It seems that the distribution can have several modes (mValue = 2.89)
//  Graphs.Djikstra_Adj: Default  -> It seems that the distribution is bimodal (mValue = 3.33)

//// * Hints *
//Outliers
//  Graphs.DiGraph: Default                  -> 3 outliers were removed, 6 outliers were detected (56.68 us..57.16 us, 64.37 us..65.58 us)
//  Graphs.FGraph: Default                   -> 1 outlier  was  removed, 3 outliers were detected (91.53 us, 91.61 us, 95.49 us)
//  Graphs.Access_Di: Default                -> 3 outliers were removed, 6 outliers were detected (33.79 us..33.88 us, 35.83 us..35.87 us)
//  Graphs.Access_UndirectedGraph: Default   -> 1 outlier  was  removed (41.00 us)
//  Graphs.Djikstra_FGraph: Default          -> 1 outlier  was  removed (107.41 us)
//  Graphs.DiGraph: Default                  -> 1 outlier  was  removed, 9 outliers were detected (8.81 ms..9.12 ms, 10.84 ms)
//  Graphs.UndirectedGraph: Default          -> 1 outlier  was  removed, 4 outliers were detected (9.18 ms..9.21 ms, 11.40 ms)
//  Graphs.FGraph: Default                   -> 1 outlier  was  removed (6.00 ms)
//  Graphs.AdjGraph: Default                 -> 1 outlier  was  removed (5.36 ms)
//  Graphs.Access_Adj: Default               -> 1 outlier  was  detected (6.29 ms)
//  Graphs.Djikstra_Di: Default              -> 1 outlier  was  detected (365.21 us)
//  Graphs.Djikstra_UndirectedGraph: Default -> 2 outliers were detected (330.34 us, 338.44 us)
//  Graphs.DiGraph: Default                  -> 2 outliers were detected (61.22 ms, 61.50 ms)
//  Graphs.FGraph: Default                   -> 1 outlier  was  removed, 4 outliers were detected (24.77 ms..24.98 ms, 25.77 ms)
//  Graphs.Access_Di: Default                -> 2 outliers were removed (63.96 ms, 66.13 ms)
//  Graphs.Access_Adj: Default               -> 6 outliers were removed (31.47 ms..33.99 ms)
//  Graphs.Djikstra_Di: Default              -> 1 outlier  was  removed, 4 outliers were detected (311.34 us..312.07 us, 325.19 us)
//  Graphs.Djikstra_UndirectedGraph: Default -> 7 outliers were detected (243.09 us..246.13 us)
//  Graphs.Djikstra_Adj: Default             -> 3 outliers were removed, 8 outliers were detected (2.27 ms..2.29 ms, 2.65 ms..3.06 ms)
//  Graphs.DiGraph: Default                  -> 1 outlier  was  detected (4.31 ms)
//  Graphs.Access_UndirectedGraph: Default   -> 1 outlier  was  removed, 4 outliers were detected (31.18 us..31.55 us, 33.50 us)
//  Graphs.Access_FGraph: Default            -> 1 outlier  was  removed, 6 outliers were detected (16.50 us..16.85 us, 19.90 us)
//  Graphs.Djikstra_FGraph: Default          -> 3 outliers were detected (2.97 ms..3.08 ms)
//  Graphs.Djikstra_Adj: Default             -> 2 outliers were detected (2.94 ms, 2.98 ms)
//  Graphs.UndirectedGraph: Default          -> 2 outliers were detected (56.73 ms, 58.60 ms)
//  Graphs.Access_Di: Default                -> 1 outlier  was  removed, 2 outliers were detected (12.06 ms, 12.71 ms)
//  Graphs.Djikstra_Adj: Default             -> 1 outlier  was  removed (48.69 ms)
//  Graphs.UndirectedGraph: Default          -> 2 outliers were removed, 3 outliers were detected (176.77 ms, 192.90 ms, 277.60 ms)
//  Graphs.FGraph: Default                   -> 3 outliers were detected (188.89 ms..191.45 ms)
//  Graphs.Access_Di: Default                -> 2 outliers were removed (74.19 ms, 99.31 ms)
//  Graphs.Access_UndirectedGraph: Default   -> 1 outlier  was  removed (91.81 ms)
//  Graphs.Access_FGraph: Default            -> 1 outlier  was  removed (33.17 ms)
//  Graphs.Access_Adj: Default               -> 1 outlier  was  detected (78.45 ms)
//  Graphs.Djikstra_Di: Default              -> 2 outliers were removed, 22 outliers were detected (22.63 ms..26.87 ms, 29.29 ms, 29.40 ms)
//  Graphs.Djikstra_UndirectedGraph: Default -> 1 outlier  was  removed (60.56 ms)
//  Graphs.Djikstra_FGraph: Default          -> 1 outlier  was  removed (94.94 ms)
//  Graphs.Djikstra_Adj: Default             -> 1 outlier  was  detected (214.48 ms)
//  Graphs.UndirectedGraph: Default          -> 2 outliers were detected (10.60 ms, 10.65 ms)
//  Graphs.Access_UndirectedGraph: Default   -> 13 outliers were detected (21.92 us..24.34 us)
//  Graphs.Access_Adj: Default               -> 3 outliers were removed (32.21 us..33.11 us)
//  Graphs.Djikstra_UndirectedGraph: Default -> 1 outlier  was  removed, 2 outliers were detected (3.89 ms, 5.01 ms)
//  Graphs.UndirectedGraph: Default          -> 2 outliers were removed (65.88 ms, 66.10 ms)
//  Graphs.Access_UndirectedGraph: Default   -> 1 outlier  was  removed, 5 outliers were detected (8.36 ms..8.55 ms, 9.20 ms)
//  Graphs.Access_Adj: Default               -> 1 outlier  was  removed, 3 outliers were detected (7.55 ms, 7.81 ms, 9.88 ms)
//  Graphs.Djikstra_Di: Default              -> 2 outliers were removed, 3 outliers were detected (4.31 ms, 5.19 ms, 5.43 ms)
//  Graphs.Djikstra_UndirectedGraph: Default -> 1 outlier  was  removed, 2 outliers were detected (5.80 ms, 6.42 ms)
//  Graphs.Djikstra_Adj: Default             -> 2 outliers were removed (79.79 ms, 79.85 ms)
//  Graphs.DiGraph: Default                  -> 1 outlier  was  removed, 21 outliers were detected (199.69 ms..213.27 ms, 252.00 ms)
//  Graphs.FGraph: Default                   -> 1 outlier  was  removed (344.06 ms)
//  Graphs.Access_Di: Default                -> 2 outliers were removed (93.26 ms, 93.80 ms)
//  Graphs.Access_UndirectedGraph: Default   -> 1 outlier  was  detected (102.39 ms)
//  Graphs.Access_FGraph: Default            -> 1 outlier  was  removed (62.54 ms)
//  Graphs.Djikstra_FGraph: Default          -> 1 outlier  was  removed (187.03 ms)
//  Graphs.Djikstra_Adj: Default             -> 1 outlier  was  removed (423.06 ms)

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
