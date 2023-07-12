# Graphoscope

## Design Principles 
The library has four aspirational principles

- Performance: Be very fast to run analysis.  
- Completeness: Support a broad range of use cases and easy extension.
- Ease of Use: User friendly api enabling succinct code.
- Community: Maintain excellent documention and encourage contributions. 

## Prerequisites

### Users 

- .NET 6.0 or higher (https://dotnet.microsoft.com/download/dotnet)

### Developers

- .NET 6.0 or higher (https://dotnet.microsoft.com/download/dotnet)
- An IDE suited to work in the .NET and F# ecosystem, here are some suggestions:
	- Visual Studio 2022 (https://visualstudio.microsoft.com/vs/)
	- Visual Studio Code (https://code.visualstudio.com/) with the Ionide plugin (https://ionide.io/)
	- JetBrains Rider (https://www.jetbrains.com/rider/)

## repo structure

- The `src` folder contains 1 subfolder per project. 

- The `tests` folder contains 1 subfolder per test project.

- The `docs` folder contains documentation snippets (`.fsx` and `.md` files)

- The `build` folder contains the buildproject which contains all build tasks.

## Develop

in general, `dotnet tool restore` must be run in the repo root once after cloning to install the dotnet tools used in this repo

The `build.cmd` and `build.sh` scripts are shorthand scripts that execute the `/build/Build.fsproj` build project. This build project contains various tasks.

### Build

In the repo root, run the following command based on your OS:

Linux/MacOS:

```bash
./build.sh
```

Windows:

```powershell
./build.cmd
```

### Test

#### via IDE

If your IDE supports TestAdapters, open the solution and run the tests from Test Explorer.

#### via CLI

In the repo root, run the following command based on your OS:

Linux/MacOS:

```bash
./build.sh runTests
```

Windows:

```powershell
./build.cmd runTests
```

### Current Benchmark

BenchmarkDotNet=v0.13.5, OS=Windows 11 (10.0.22621.1992/22H2/2022Update/SunValley2)
Intel Core i7-1065G7 CPU 1.30GHz, 1 CPU, 8 logical and 4 physical cores
.NET SDK=6.0.411
  [Host]     : .NET 6.0.19 (6.0.1923.31806), X64 RyuJIT AVX2 DEBUG
  DefaultJob : .NET 6.0.19 (6.0.1923.31806), X64 RyuJIT AVX2


|        Method | NumberNodes | NumberEdges |         Mean |      Error |       StdDev |       Median |      Gen0 |     Gen1 |     Gen2 |   Allocated |
|-------------- |------------ |------------ |-------------:|-----------:|-------------:|-------------:|----------:|---------:|---------:|------------:|
|      AdjGraph |         100 |         500 |     36.52 us |   0.727 us |     0.995 us |     36.42 us |   17.5171 |        - |        - |    71.64 KB |
|       AdjComp |         100 |         500 |     38.04 us |   0.638 us |     0.655 us |     38.31 us |   14.8926 |   0.2441 |        - |    60.89 KB |
|       DiGraph |         100 |         500 |     15.80 us |   0.217 us |     0.193 us |     15.84 us |    9.9792 |   0.0305 |        - |    40.76 KB |
|   DiNodeGraph |         100 |         500 |     30.55 us |   0.592 us |     0.582 us |     30.44 us |   17.0898 |        - |        - |    70.01 KB |
|        FGraph |         100 |         500 |     40.27 us |   0.804 us |     1.428 us |     39.95 us |   24.7803 |   0.1221 |        - |   101.35 KB |
|    Access_Adj |         100 |         500 |     23.86 us |   0.402 us |     0.356 us |     23.94 us |    6.8054 |        - |        - |    27.82 KB |
|   Access_Comp |         100 |         500 |     21.33 us |   0.421 us |     0.968 us |     21.48 us |    6.8054 |        - |        - |    27.82 KB |
|     Access_Di |         100 |         500 |     21.88 us |   0.429 us |     0.573 us |     21.96 us |    9.6741 |        - |        - |    39.54 KB |
| Access_DiNode |         100 |         500 |     39.21 us |   0.753 us |     1.433 us |     38.79 us |   16.3574 |        - |        - |    66.88 KB |
| Access_FGraph |         100 |         500 |     15.16 us |   0.302 us |     0.790 us |     15.01 us |    6.8054 |        - |        - |    27.82 KB |
|      AdjGraph |         100 |       50000 |  3,013.35 us |  46.135 us |    43.155 us |  3,008.92 us |  500.0000 | 246.0938 |        - |  2561.04 KB |
|       AdjComp |         100 |       50000 |  3,162.62 us |  61.554 us |    82.172 us |  3,141.63 us |  195.3125 |  70.3125 |        - |  1002.56 KB |
|       DiGraph |         100 |       50000 |  1,826.41 us |  29.391 us |    26.054 us |  1,829.23 us |  445.3125 | 222.6563 |        - |  2720.63 KB |
|   DiNodeGraph |         100 |       50000 |  5,599.60 us | 110.431 us |   118.160 us |  5,650.62 us |  914.0625 | 453.1250 |        - |  5070.45 KB |
|        FGraph |         100 |       50000 |  3,312.77 us |  60.671 us |    53.783 us |  3,311.50 us |  359.3750 | 148.4375 |        - |  1984.74 KB |
|    Access_Adj |         100 |       50000 |  6,027.10 us | 166.613 us |   491.261 us |  6,032.00 us |  578.1250 | 187.5000 | 164.0625 |  2977.72 KB |
|   Access_Comp |         100 |       50000 |  5,605.19 us | 216.585 us |   638.606 us |  5,763.08 us |  578.1250 | 195.3125 | 164.0625 |  2977.73 KB |
|     Access_Di |         100 |       50000 | 11,669.94 us | 186.577 us |   222.107 us | 11,598.24 us |  859.3750 | 187.5000 | 171.8750 |  4149.85 KB |
| Access_DiNode |         100 |       50000 | 13,634.85 us | 262.426 us |   570.493 us | 13,488.59 us | 1531.2500 | 234.3750 | 171.8750 |  6884.08 KB |
| Access_FGraph |         100 |       50000 |  4,651.53 us | 213.642 us |   629.927 us |  4,907.75 us |  578.1250 | 210.9375 | 164.0625 |  2977.67 KB |
|      AdjGraph |       10000 |         500 |  3,732.55 us | 124.537 us |   349.216 us |  3,781.20 us |  386.7188 | 324.2188 | 164.0625 |  2093.79 KB |
|       AdjComp |       10000 |         500 |    903.90 us |  19.537 us |    57.605 us |    906.83 us |  164.0625 | 106.4453 |  74.2188 |    807.8 KB |
|       DiGraph |       10000 |         500 |  2,695.42 us |  88.026 us |   259.546 us |  2,682.65 us |  253.9063 | 222.6563 | 113.2813 |  1396.86 KB |
|   DiNodeGraph |       10000 |         500 |  4,177.83 us | 190.091 us |   560.488 us |  4,236.07 us |  367.1875 | 347.6563 | 191.4063 |  2045.24 KB |
|        FGraph |       10000 |         500 |  5,451.31 us | 146.396 us |   431.653 us |  5,497.14 us |  546.8750 | 375.0000 | 187.5000 |  3003.15 KB |
|    Access_Adj |       10000 |         500 |     22.55 us |   0.433 us |     0.444 us |     22.47 us |    6.8054 |        - |        - |    27.82 KB |
|   Access_Comp |       10000 |         500 |     20.42 us |   0.401 us |     0.521 us |     20.37 us |    6.8054 |        - |        - |    27.82 KB |
|     Access_Di |       10000 |         500 |     19.38 us |   0.344 us |     0.305 us |     19.42 us |    9.6741 |        - |        - |    39.54 KB |
| Access_DiNode |       10000 |         500 |     35.03 us |   0.841 us |     2.412 us |     34.03 us |   16.3574 |        - |        - |    66.88 KB |
| Access_FGraph |       10000 |         500 |     15.11 us |   0.252 us |     0.236 us |     15.14 us |    6.8054 |        - |        - |    27.82 KB |
|      AdjGraph |       10000 |       50000 | 21,790.54 us | 455.362 us | 1,342.645 us | 21,721.45 us | 1250.0000 | 531.2500 | 156.2500 |  7369.87 KB |
|       AdjComp |       10000 |       50000 | 14,915.89 us | 293.346 us |   605.811 us | 14,903.43 us |  906.2500 | 421.8750 | 125.0000 |   6134.5 KB |
|       DiGraph |       10000 |       50000 | 11,529.53 us | 230.098 us |   660.194 us | 11,545.51 us |  656.2500 | 328.1250 |  93.7500 |  4040.71 KB |
|   DiNodeGraph |       10000 |       50000 | 23,243.74 us | 454.927 us | 1,026.847 us | 22,997.41 us | 1125.0000 | 468.7500 | 156.2500 |  7015.79 KB |
|        FGraph |       10000 |       50000 | 29,492.95 us | 587.841 us | 1,290.326 us | 29,455.82 us | 1937.5000 | 937.5000 | 343.7500 | 10451.67 KB |
|    Access_Adj |       10000 |       50000 |  4,962.23 us | 128.893 us |   380.044 us |  5,033.85 us |  468.7500 |  93.7500 |  62.5000 |  2977.65 KB |
|   Access_Comp |       10000 |       50000 |  5,289.03 us | 138.180 us |   391.995 us |  5,315.84 us |  468.7500 |  93.7500 |  62.5000 |  2977.65 KB |
|     Access_Di |       10000 |       50000 |  6,017.33 us | 119.178 us |   305.498 us |  5,965.08 us |  765.6250 |  93.7500 |  70.3125 |  4149.48 KB |
| Access_DiNode |       10000 |       50000 |  7,690.06 us | 175.736 us |   515.404 us |  7,745.51 us | 1421.8750 | 125.0000 |  62.5000 |  6883.86 KB |
| Access_FGraph |       10000 |       50000 |  5,599.11 us | 132.537 us |   384.515 us |  5,477.96 us |  468.7500 |  93.7500 |  62.5000 |  2977.65 KB |
