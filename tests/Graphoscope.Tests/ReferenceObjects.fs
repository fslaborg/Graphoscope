module ReferenceObjects


open Graphoscope


let fGraph1 =
    [1 .. 7] |> List.fold (fun acc nk -> FGraph.addNode nk "" acc) FGraph.empty
    |> FGraph.addEdge 1 2 "1-2"
    |> FGraph.addEdge 2 3 "2-3"
    |> FGraph.addEdge 3 4 "3-4"
    |> FGraph.addEdge 4 5 "4-5"
    |> FGraph.addEdge 5 6 "5-6"
    |> FGraph.addEdge 1 7 "1-7"
    |> FGraph.addEdge 5 1 "5-1"