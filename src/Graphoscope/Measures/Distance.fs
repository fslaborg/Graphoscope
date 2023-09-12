namespace Graphoscope.Measures
open Graphoscope
open FSharpAux

type Distance() =
    
    // static member averageOfFGraph () (nodeIndexer:'NodeKey -> int) (graph :  FGraph<'NodeKey, 'NodeData, 'EdgeData>) = 
    //     let distances =
    //         graph
    //         |> FGraph.toArray2D nodeIndexer
    //         |> Algorithms.FloydWarshall.fromArray2D

    //     distances
    //     |> Seq.cast<float>
    //     |> Seq.average

    // static member maxOfFGraph (nodeIndexer:'NodeKey -> int) (graph :  FGraph<'NodeKey, 'NodeData, 'EdgeData>) = 
    //     let distances =
    //         graph
    //         |> FGraph.toArray2D nodeIndexer
    //         |> Algorithms.FloydWarshall.fromArray2D

    //     distances
    //     |> Seq.cast<float>
    //     |> Seq.max


    // static member minOFGraph (nodeIndexer:'NodeKey -> int) (graph :  FGraph<'NodeKey, 'NodeData, 'EdgeData>) = 
    //     let distances =
    //         graph
    //         |> FGraph.toArray2D nodeIndexer
    //         |> Algorithms.FloydWarshall.fromArray2D

    //     distances
    //     |> Seq.cast<float>
    //     |> Seq.min

    /// <summary> 
    /// Get the average of the shortest Paths in a graph calculated by their flowyWarshall result
    /// </summary>
    /// <param name="floydWarshall">Result of the FloydWarshall shortest Path calculation of a graph</param> 
    /// <returns>A float of average of the shortest Paths of a graph</returns>
    static member averageOfFloydWarshall (floydWarshall : float [,]) = 
        ///Returns the element of the array which is the smallest after projection according to the Operators.min operator
        let average (arr: _ [,])  =
            let n,m = arr |> Array2D.length1, arr |> Array2D.length2
            let counts = n+m |> float
            let rec sumCol i j sum =
                if j = m then sum
                else
                    let value = arr.[i,j]
                    sumCol i (j+1) (sum+value) 

            let rec countRow sum i = 
                if i = n then sum
                else countRow (sumCol i 0 sum) (i+1)
            countRow 0. 0
            |> fun x -> (x /counts)

        floydWarshall
        |> average

    /// <summary> 
    /// Get the longest shortest Path in a graph calculated by their flowyWarshall result
    /// </summary>
    /// <param name="floydWarshall">Result of the FloydWarshall shortest Path calculation of a graph</param> 
    /// <returns>A float of the longest shortest Path of a graph</returns>
    static member maxOfFloydWarshall (floydWarshall : float [,]) = 
        floydWarshall
        |> FSharpAux.Array2D.maxBy id

    /// <summary> 
    /// Get the shortest shortest Paths in a graph calculated by their flowyWarshall result
    /// </summary>
    /// <param name="floydWarshall">Result of the FloydWarshall shortest Path calculation of a graph</param> 
    /// <returns>A float of the shortest shortest Paths of a graph</returns>
    static member minOfFloydWarshall (floydWarshall : float [,]) = 
        ///Returns the element of the array which is the smallest after projection according to the Operators.min operator
        let minBy projection (arr: _ [,])  =
            let n,m = arr |> Array2D.length1, arr |> Array2D.length2
            let rec compareMin i j min =
                if j = m then min
                else
                    let value = arr.[i,j]
                    if (projection value) > (projection min) then compareMin i (j+1) min 
                    else compareMin i (j+1) value 
            let rec countRow min i = 
                if i = n then min
                else countRow (compareMin i 0 min) (i+1)
            countRow arr.[0,0] 0

        floydWarshall
        |> minBy id
