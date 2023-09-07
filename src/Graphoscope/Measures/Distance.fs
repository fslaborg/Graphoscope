namespace Graphoscope.Measures
open Graphoscope


type Distance() =
    
    static member averageOfFGraph (nodeIndexer:'NodeKey -> int) (graph :  FGraph<'NodeKey, 'NodeData, 'EdgeData>) = 
        let distances =
            graph
            |> FGraph.toArray2D nodeIndexer
            |> Algorithms.FloydWarshall.fromArray2D

        distances
        |> Seq.cast<float>
        |> Seq.average


    static member maxOfFGraph (nodeIndexer:'NodeKey -> int) (graph :  FGraph<'NodeKey, 'NodeData, 'EdgeData>) = 
        let distances =
            graph
            |> FGraph.toArray2D nodeIndexer
            |> Algorithms.FloydWarshall.fromArray2D

        distances
        |> Seq.cast<float>
        |> Seq.max


    static member minOFGraph (nodeIndexer:'NodeKey -> int) (graph :  FGraph<'NodeKey, 'NodeData, 'EdgeData>) = 
        let distances =
            graph
            |> FGraph.toArray2D nodeIndexer
            |> Algorithms.FloydWarshall.fromArray2D

        distances
        |> Seq.cast<float>
        |> Seq.min


type Distance2(graph :  FGraph<'NodeKey, 'NodeData, 'EdgeData> , nodeIndexer:'NodeKey->int) =
    let distance = FGraph.toArray2D nodeIndexer graph |> Algorithms.FloydWarshall.fromArray2D |> Seq.cast<float>

    member this.Average()   = distance |> Seq.average
    member this.Max()       = distance |> Seq.max
    member this.Min()       = distance |> Seq.min



        
