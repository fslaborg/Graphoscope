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
    let mutable adjGraph     = AdjGraph.empty<int,int,float>
    let mutable diGraph      = DiGraph.empty<int,int,float>
    let mutable diNodeGraph  = UndirectedGraph.empty<int,int,float>
    let mutable fGraph       = FGraph.empty<int,int,float>

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
                let node1 = rnd.Next(0,this.NumberNodes-1)
                let node2 = rnd.Next(0,this.NumberNodes-1)
                yield (node1,node2,float i)
            |]
        edgesArr <- edges
        //prepare AdjGraph
        let gAdj= AdjGraph.create<int,int,float>()
        for i=0 to this.NumberNodes-1 do
            AdjGraph.addNode i i gAdj |> ignore
        for i=0 to this.NumberEdges-1 do
            let (node1,node2,data) = edgesArr.[i]
            AdjGraph.addElement node1 node1 node2 node2 data gAdj |> ignore
        adjGraph <- gAdj
        //prepare DiGraph
        let gDi = DiGraph.empty<int,int,float>
        for i=0 to this.NumberNodes-1 do
            DiGraph.addNode i i gDi|> ignore
        for i=0 to this.NumberEdges-1 do
            let (node1,node2,data) = edgesArr.[i]
            DiGraph.addElement node1 node1 node2 node2 data gDi|> ignore
        diGraph <- gDi
        //prepare DiNodeGraph
        let gDiNo = UndirectedGraph.empty<int,int,float>
        for i=0 to this.NumberNodes-1 do
            UndirectedGraph.addNode i i gDiNo|> ignore
        for i=0 to this.NumberEdges-1 do
            let (node1,node2,data) = edgesArr.[i]
            UndirectedGraph.addElement node1 node1 node2 node2 data gDiNo|> ignore
        diNodeGraph <- gDiNo
        //prepare FGraph
        let gF = FGraph.create<int,int,float>()
        for i=0 to this.NumberNodes-1 do
            FGraph.addNode i i gF |> ignore
        for i=0 to this.NumberEdges-1 do
            let (node1,node2,data) = edgesArr.[i]
            FGraph.addElement node1 node1 node2 node2 data gF |> ignore
        fGraph <- gF

    [<Benchmark>]
    member this.DiGraph () =
        let g = DiGraph.empty<int,int,float>
         // Add nodes
        for i=0 to this.NumberNodes-1 do
            DiGraph.addNode i i g|> ignore
        // Add edges
        for i=0 to this.NumberEdges-1 do
            let (node1,node2,data) = edgesArr.[i]
            //DiGraph.addElement ((node1),(node1), (node2),(node2), data) g |> ignore
            DiGraph.addElement node1 node1 node2 node2 data g|> ignore
        g

    [<Benchmark>]
    member this.UndirectedGraph () =
        let g = UndirectedGraph.empty<int,int,float>
         // Add nodes
        for i=0 to this.NumberNodes-1 do
            UndirectedGraph.addNode i i g |> ignore
        // Add edges
        for i=0 to this.NumberEdges-1 do
            let (node1,node2,data) = edgesArr.[i]
            //UndirectedGraph.addElement((node1),(node1), (node2),(node2), data) g|> ignore
            UndirectedGraph.addElement node1 node1 node2 node2 data g|> ignore
        g

    [<Benchmark>]
    member this.FGraph () =
        let g = FGraph.create<int,int,float>()
        // Add nodes
        for i=0 to this.NumberNodes-1 do
           FGraph.addNode i i g |> ignore
        // Add edges
        for i=0 to this.NumberEdges-1 do
            let (node1,node2,data) = edgesArr.[i]
            FGraph.addElement node1 node1 node2 node2 data g|> ignore
        g

    [<Benchmark>]
    member this.AdjGraph () = 
        let g = AdjGraph.create<int,int,float>()
        // Add nodes
        for i=0 to this.NumberNodes-1 do
            AdjGraph.addNode i i g |> ignore
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
           //printfn "%i %i" node1 node2
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
           //printfn "%i %i" node1 node2
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



  
// * Summary *

//BenchmarkDotNet=v0.13.5, OS=Windows 10 (10.0.20348.2031)
//Intel Xeon Gold 6336Y CPU 2.40GHz, 2 CPU, 96 logical and 48 physical cores
//.NET SDK=6.0.407
//  [Host]     : .NET 6.0.18 (6.0.1823.26907), X64 RyuJIT AVX2 DEBUG
//  DefaultJob : .NET 6.0.18 (6.0.1823.26907), X64 RyuJIT AVX2


//|                 Method | NumberNodes | NumberEdges |          Mean |        Error |        StdDev |        Median |      Gen0 |      Gen1 |     Gen2 |   Allocated |
//|----------------------- |------------ |------------ |--------------:|-------------:|--------------:|--------------:|----------:|----------:|---------:|------------:|
//|                DiGraph |         100 |         500 |      61.40 us |     0.687 us |      0.609 us |      61.23 us |    5.4321 |    0.9155 |        - |   100.74 KB |
//|        UndirectedGraph |         100 |         500 |      91.95 us |     1.351 us |      1.197 us |      91.76 us |    6.3477 |    0.9766 |        - |   117.53 KB |
//|                 FGraph |         100 |         500 |      93.74 us |     1.142 us |      1.068 us |      93.97 us |    9.1553 |    2.1973 |        - |   169.98 KB |
//|               AdjGraph |         100 |         500 |      87.12 us |     1.527 us |      1.429 us |      88.02 us |    8.3008 |    1.5869 |        - |   154.16 KB |
//|              Access_Di |         100 |         500 |      35.55 us |     0.484 us |      0.429 us |      35.70 us |    2.1362 |         - |        - |    39.54 KB |
//| Access_UndirectedGraph |         100 |         500 |      39.07 us |     0.713 us |      0.632 us |      39.29 us |    2.1362 |         - |        - |    39.54 KB |
//|          Access_FGraph |         100 |         500 |      23.78 us |     0.286 us |      0.253 us |      23.77 us |    1.4954 |         - |        - |    27.82 KB |
//|             Access_Adj |         100 |         500 |      33.61 us |     0.622 us |      0.519 us |      33.76 us |    1.4648 |         - |        - |    27.82 KB |

//|                DiGraph |         100 |       50000 |  12,457.11 us |    49.341 us |     41.202 us |  12,457.88 us |  250.0000 |  109.3750 |        - |  4724.15 KB |
//|        UndirectedGraph |         100 |       50000 |  12,877.65 us |    88.852 us |     83.112 us |  12,894.10 us |  234.3750 |   93.7500 |        - |  4552.17 KB |
//|                 FGraph |         100 |       50000 |   5,665.26 us |    29.638 us |     26.274 us |   5,674.75 us |  445.3125 |  210.9375 |        - |  8219.58 KB |
//|               AdjGraph |         100 |       50000 |   4,998.29 us |    98.719 us |     96.955 us |   4,985.89 us |  343.7500 |  156.2500 |        - |  6454.74 KB |
//|              Access_Di |         100 |       50000 |  14,581.46 us |   283.312 us |    488.699 us |  14,522.93 us |  421.8750 |  281.2500 | 281.2500 |  4149.94 KB |
//| Access_UndirectedGraph |         100 |       50000 |  14,086.37 us |   277.794 us |    567.459 us |  14,167.74 us |  421.8750 |  281.2500 | 281.2500 |  4149.75 KB |
//|          Access_FGraph |         100 |       50000 |   4,495.23 us |    86.070 us |    138.987 us |   4,489.01 us |  312.5000 |  226.5625 | 226.5625 |  2977.93 KB |
//|             Access_Adj |         100 |       50000 |   5,106.01 us |   122.644 us |    361.618 us |   5,047.90 us |  335.9375 |  250.0000 | 250.0000 |  2977.79 KB |

//|                DiGraph |         100 |      250000 |  63,774.09 us |   564.062 us |    527.623 us |  63,818.49 us | 1000.0000 |  444.4444 |        - |    20354 KB |
//|        UndirectedGraph |         100 |      250000 |  63,971.55 us |   841.042 us |    745.562 us |  64,260.83 us | 1000.0000 |  500.0000 |        - | 20179.65 KB |
//|                 FGraph |         100 |      250000 |  24,267.79 us |   365.503 us |    341.891 us |  24,296.36 us | 1781.2500 |  218.7500 |        - |  33141.2 KB |
//|               AdjGraph |         100 |      250000 |  21,949.14 us |    53.545 us |     41.804 us |  21,945.00 us | 1531.2500 |   93.7500 |        - | 28266.26 KB |
//|              Access_Di |         100 |      250000 |  66,627.75 us |   924.154 us |    819.239 us |  66,844.90 us | 1250.0000 |  500.0000 | 500.0000 | 19723.22 KB |
//| Access_UndirectedGraph |         100 |      250000 |  66,543.48 us | 1,319.054 us |  2,275.302 us |  66,135.91 us | 1250.0000 |  500.0000 | 500.0000 | 19723.05 KB |
//|          Access_FGraph |         100 |      250000 |  20,106.89 us |   385.416 us |    664.824 us |  20,132.16 us |  875.0000 |  468.7500 | 468.7500 | 13862.29 KB |
//|             Access_Adj |         100 |      250000 |  25,816.90 us |   510.996 us |  1,142.915 us |  25,725.35 us |  875.0000 |  468.7500 | 468.7500 | 13862.62 KB |

//|                DiGraph |       10000 |         500 |   3,478.24 us |    68.554 us |    156.132 us |   3,493.79 us |  167.9688 |  164.0625 |  89.8438 |     2164 KB |
//|        UndirectedGraph |       10000 |         500 |   2,242.35 us |    44.505 us |     89.903 us |   2,232.08 us |  128.9063 |  121.0938 |  70.3125 |  1616.98 KB |
//|                 FGraph |       10000 |         500 |   6,651.30 us |   130.941 us |    145.540 us |   6,637.43 us |  265.6250 |  226.5625 | 117.1875 |  3456.15 KB |
//|               AdjGraph |       10000 |         500 |   4,792.73 us |    95.530 us |    228.884 us |   4,758.62 us |  195.3125 |  187.5000 | 101.5625 |   2507.2 KB |
//|              Access_Di |       10000 |         500 |      29.44 us |     1.132 us |      3.339 us |      30.04 us |    2.1362 |         - |        - |    39.54 KB |
//| Access_UndirectedGraph |       10000 |         500 |      24.11 us |     0.067 us |      0.108 us |      24.08 us |    2.1362 |         - |        - |    39.54 KB |
//|          Access_FGraph |       10000 |         500 |      24.43 us |     0.479 us |      0.606 us |      24.36 us |    1.4954 |         - |        - |    27.82 KB |
//|             Access_Adj |       10000 |         500 |      25.87 us |     0.021 us |      0.018 us |      25.87 us |    1.4648 |         - |        - |    27.82 KB |

//|                DiGraph |       10000 |       50000 |  55,940.28 us |   640.989 us |    535.255 us |  55,927.79 us |  562.5000 |  312.5000 |  62.5000 | 10154.05 KB |
//|        UndirectedGraph |       10000 |       50000 |  60,783.64 us |   515.353 us |    482.061 us |  60,925.68 us |  750.0000 |  437.5000 | 125.0000 | 12127.18 KB |
//|                 FGraph |       10000 |       50000 |  54,742.38 us | 1,014.618 us |    996.491 us |  54,763.57 us | 1062.5000 |  625.0000 | 187.5000 | 17073.82 KB |
//|               AdjGraph |       10000 |       50000 |  44,396.62 us |   877.899 us |    862.215 us |  44,354.08 us |  937.5000 |  562.5000 | 125.0000 | 15675.56 KB |
//|              Access_Di |       10000 |       50000 |  12,117.46 us |   227.752 us |    213.040 us |  12,047.17 us |  187.5000 |   46.8750 |  46.8750 |  4149.59 KB |
//| Access_UndirectedGraph |       10000 |       50000 |  13,428.52 us |   197.405 us |    184.653 us |  13,475.42 us |  187.5000 |   46.8750 |  46.8750 |  4149.51 KB |
//|          Access_FGraph |       10000 |       50000 |   9,487.36 us |   161.899 us |    151.441 us |   9,414.71 us |  125.0000 |   46.8750 |  46.8750 |   2977.6 KB |
//|             Access_Adj |       10000 |       50000 |  14,172.71 us |   192.758 us |    180.306 us |  14,149.50 us |  125.0000 |   46.8750 |  46.8750 |  2977.63 KB |

//|                DiGraph |       10000 |      250000 | 280,975.60 us | 5,350.809 us |  6,571.275 us | 279,870.10 us | 2000.0000 | 1000.0000 |        - |  43112.6 KB |
//|        UndirectedGraph |       10000 |      250000 | 309,337.21 us | 6,057.569 us |  5,949.343 us | 307,967.45 us | 2500.0000 | 1000.0000 |        - | 53501.74 KB |
//|                 FGraph |       10000 |      250000 | 207,109.06 us | 3,940.021 us |  4,379.323 us | 207,074.43 us | 3666.6667 | 1666.6667 |        - | 72672.95 KB |
//|               AdjGraph |       10000 |      250000 | 180,446.44 us | 1,422.016 us |  1,110.216 us | 180,853.03 us | 3666.6667 | 1666.6667 |        - | 73178.99 KB |
//|              Access_Di |       10000 |      250000 | 101,617.37 us |   263.988 us |    234.018 us | 101,584.94 us |  600.0000 |         - |        - | 19721.75 KB |
//| Access_UndirectedGraph |       10000 |      250000 | 129,080.17 us |   936.642 us |    876.135 us | 129,306.80 us |  750.0000 |         - |        - | 19722.47 KB |
//|          Access_FGraph |       10000 |      250000 |  55,683.67 us |   350.083 us |    310.340 us |  55,620.85 us |  400.0000 |         - |        - | 13862.19 KB |
//|             Access_Adj |       10000 |      250000 |  81,634.06 us |   244.622 us |    228.819 us |  81,586.71 us |  428.5714 |         - |        - |  13862.3 KB |

//|                DiGraph |       50000 |         500 |  15,041.20 us |   380.160 us |  1,120.909 us |  14,850.05 us |  343.7500 |  281.2500 | 156.2500 |  9152.05 KB |
//|        UndirectedGraph |       50000 |         500 |   9,597.60 us |   231.455 us |    682.450 us |   9,697.19 us |  250.0000 |  218.7500 | 125.0000 |  6588.55 KB |
//|                 FGraph |       50000 |         500 |  34,917.96 us |   691.796 us |    899.531 us |  35,068.20 us |  812.5000 |  500.0000 | 187.5000 | 15890.37 KB |
//|               AdjGraph |       50000 |         500 |  24,614.59 us |   485.012 us |    647.477 us |  24,764.68 us |  500.0000 |  343.7500 | 156.2500 | 11194.46 KB |
//|              Access_Di |       50000 |         500 |      32.95 us |     0.645 us |      0.662 us |      33.17 us |    2.1362 |         - |        - |    39.54 KB |
//| Access_UndirectedGraph |       50000 |         500 |      31.65 us |     0.065 us |      0.060 us |      31.66 us |    2.1362 |         - |        - |    39.54 KB |
//|          Access_FGraph |       50000 |         500 |      24.27 us |     0.277 us |      0.232 us |      24.16 us |    1.4954 |         - |        - |    27.82 KB |
//|             Access_Adj |       50000 |         500 |      36.39 us |     0.486 us |      0.455 us |      36.53 us |    1.4648 |         - |        - |    27.82 KB |

//|                DiGraph |       50000 |       50000 |  76,295.83 us | 1,479.683 us |  1,644.663 us |  76,015.75 us |  750.0000 |  375.0000 | 125.0000 | 18384.66 KB |
//|        UndirectedGraph |       50000 |       50000 |  65,685.81 us | 1,280.215 us |  1,664.641 us |  65,832.30 us |  625.0000 |  250.0000 |        - | 17250.66 KB |
//|                 FGraph |       50000 |       50000 | 117,281.09 us | 2,233.547 us |  2,089.261 us | 117,655.57 us | 1500.0000 |  833.3333 | 166.6667 |  30789.1 KB |
//|               AdjGraph |       50000 |       50000 |  85,622.69 us | 1,695.118 us |  2,737.300 us |  86,704.91 us | 1000.0000 |  500.0000 |        - | 23977.03 KB |
//|              Access_Di |       50000 |       50000 |  13,170.01 us |   179.538 us |    167.940 us |  13,221.91 us |  171.8750 |   31.2500 |  31.2500 |  4149.49 KB |
//| Access_UndirectedGraph |       50000 |       50000 |  15,924.17 us |    68.329 us |     57.058 us |  15,907.15 us |  156.2500 |   31.2500 |  31.2500 |  4149.48 KB |
//|          Access_FGraph |       50000 |       50000 |   6,016.72 us |    20.613 us |     18.273 us |   6,012.61 us |  109.3750 |   31.2500 |  31.2500 |  2977.67 KB |
//|             Access_Adj |       50000 |       50000 |  14,115.27 us |   194.691 us |    182.114 us |  14,070.98 us |  109.3750 |   31.2500 |  31.2500 |   2977.6 KB |

//|                DiGraph |       50000 |      250000 | 327,099.39 us | 6,584.282 us | 16,639.321 us | 332,231.70 us | 2000.0000 | 1000.0000 |        - | 49580.13 KB |
//|        UndirectedGraph |       50000 |      250000 | 374,900.01 us | 7,207.743 us |  9,115.488 us | 376,582.20 us | 3000.0000 | 1000.0000 |        - | 59685.27 KB |
//|                 FGraph |       50000 |      250000 | 345,738.73 us | 5,662.851 us |  5,297.034 us | 347,466.60 us | 4000.0000 | 2000.0000 |        - | 84727.34 KB |
//|               AdjGraph |       50000 |      250000 | 301,269.05 us | 2,379.182 us |  2,109.083 us | 301,160.03 us | 4000.0000 | 2000.0000 |        - | 77869.06 KB |
//|              Access_Di |       50000 |      250000 |  94,828.50 us |   556.362 us |    520.421 us |  94,748.83 us |  666.6667 |         - |        - | 19721.71 KB |
//| Access_UndirectedGraph |       50000 |      250000 | 105,444.91 us |   325.905 us |    288.906 us | 105,423.04 us |  600.0000 |         - |        - | 19721.63 KB |
//|          Access_FGraph |       50000 |      250000 |  63,475.32 us |   245.998 us |    230.107 us |  63,535.19 us |  375.0000 |         - |        - | 13864.52 KB |
//|             Access_Adj |       50000 |      250000 |  90,777.81 us |   612.568 us |    511.522 us |  90,693.33 us |  333.3333 |         - |        - | 13862.33 KB |

//// * Warnings *
//MultimodalDistribution
//  Graphs.Access_Adj: Default -> It seems that the distribution is bimodal (mValue = 3.86)
//  Graphs.Access_Di: Default  -> It seems that the distribution is bimodal (mValue = 4.16)
//  Graphs.DiGraph: Default    -> It seems that the distribution is multimodal (mValue = 4.44)

//// * Hints *
//Outliers
//  Graphs.DiGraph: Default                -> 1 outlier  was  removed (63.09 us)
//  Graphs.UndirectedGraph: Default        -> 1 outlier  was  removed (96.25 us)
//  Graphs.Access_Di: Default              -> 1 outlier  was  removed (37.11 us)
//  Graphs.Access_UndirectedGraph: Default -> 1 outlier  was  removed (40.80 us)
//  Graphs.Access_FGraph: Default          -> 1 outlier  was  removed, 3 outliers were detected (23.25 us, 23.33 us, 24.55 us)
//  Graphs.Access_Adj: Default             -> 2 outliers were removed, 3 outliers were detected (31.99 us, 34.62 us, 34.91 us)
//  Graphs.DiGraph: Default                -> 2 outliers were removed, 3 outliers were detected (12.35 ms, 12.58 ms, 12.61 ms)
//  Graphs.UndirectedGraph: Default        -> 2 outliers were detected (12.64 ms, 12.74 ms)
//  Graphs.FGraph: Default                 -> 1 outlier  was  removed, 2 outliers were detected (5.62 ms, 5.81 ms)
//  Graphs.Access_Di: Default              -> 2 outliers were removed (16.21 ms, 16.44 ms)
//  Graphs.Access_FGraph: Default          -> 1 outlier  was  removed (4.93 ms)
//  Graphs.DiGraph: Default                -> 1 outlier  was  detected (62.19 ms)
//  Graphs.UndirectedGraph: Default        -> 1 outlier  was  removed, 4 outliers were detected (62.23 ms..63.80 ms, 64.56 ms)
//  Graphs.AdjGraph: Default               -> 3 outliers were removed (22.18 ms..22.23 ms)
//  Graphs.Access_Di: Default              -> 1 outlier  was  removed, 3 outliers were detected (64.69 ms, 64.84 ms, 68.10 ms)
//  Graphs.Access_FGraph: Default          -> 1 outlier  was  removed (22.25 ms)
//  Graphs.Access_UndirectedGraph: Default -> 11 outliers were removed (30.91 us..32.33 us)
//  Graphs.Access_Adj: Default             -> 2 outliers were removed (25.93 us, 25.94 us)
//  Graphs.DiGraph: Default                -> 2 outliers were removed, 3 outliers were detected (54.72 ms, 57.45 ms, 57.84 ms)
//  Graphs.UndirectedGraph: Default        -> 3 outliers were removed, 4 outliers were detected (296.30 ms, 329.95 ms..339.74 ms)
//  Graphs.AdjGraph: Default               -> 3 outliers were removed, 4 outliers were detected (177.59 ms, 183.40 ms..183.92 ms)
//  Graphs.Access_Di: Default              -> 1 outlier  was  removed (103.81 ms)
//  Graphs.Access_UndirectedGraph: Default -> 1 outlier  was  detected (126.80 ms)
//  Graphs.Access_FGraph: Default          -> 1 outlier  was  removed (57.03 ms)
//  Graphs.AdjGraph: Default               -> 5 outliers were detected (23.04 ms..23.75 ms)
//  Graphs.Access_Di: Default              -> 1 outlier  was  removed (35.50 us)
//  Graphs.Access_FGraph: Default          -> 2 outliers were removed (24.94 us, 24.95 us)
//  Graphs.Access_Adj: Default             -> 3 outliers were detected (35.41 us..35.80 us)
//  Graphs.UndirectedGraph: Default        -> 2 outliers were detected (61.83 ms, 62.60 ms)
//  Graphs.FGraph: Default                 -> 1 outlier  was  detected (111.98 ms)
//  Graphs.AdjGraph: Default               -> 6 outliers were removed, 12 outliers were detected (77.44 ms..82.03 ms, 89.43 ms..90.70 ms)
//  Graphs.Access_UndirectedGraph: Default -> 2 outliers were removed (16.10 ms, 16.24 ms)
//  Graphs.Access_FGraph: Default          -> 1 outlier  was  removed (6.77 ms)
//  Graphs.DiGraph: Default                -> 1 outlier  was  removed, 8 outliers were detected (268.86 ms..281.69 ms, 340.91 ms)
//  Graphs.UndirectedGraph: Default        -> 2 outliers were detected (352.87 ms, 356.05 ms)
//  Graphs.FGraph: Default                 -> 3 outliers were detected (334.59 ms..337.19 ms)
//  Graphs.AdjGraph: Default               -> 1 outlier  was  removed (307.30 ms)
//  Graphs.Access_UndirectedGraph: Default -> 1 outlier  was  removed (107.75 ms)
//  Graphs.Access_Adj: Default             -> 2 outliers were removed (92.33 ms, 93.01 ms)

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